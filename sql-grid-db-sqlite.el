;;; sql-grid-db-sqlite.el --- SQLite driver for sql-grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'base64)
(require 'json)
(require 'sqlite)
(require 'subr-x)
(require 'sql-grid-db-core)
(defun sql-grid-db--raw-byte-string-p (string)
  "Return non-nil when multibyte STRING contains raw byte8 characters.
Emacs represents such characters as codepoints #x3FFF80 through
#x3FFFFF.  JSON cannot serialize them."
  (let ((i 0) (length (length string)) found)
    (while (and (< i length) (not found))
      (let ((char (aref string i)))
        (when (and (>= char #x3FFF80) (<= char #x3FFFFF))
          (setq found t)))
      (setq i (1+ i)))
    found))

(defun sql-grid-db--encode-wire-value (value)
  "Encode scalar VALUE for the SQLite worker's JSON transport.
SQL NULL becomes the JSON null object.  Unibyte strings (SQLite BLOBs)
are base64-tagged with their exact bytes.  Ordinary multibyte TEXT uses
plain JSON; TEXT containing raw byte8 characters that JSON rejects is
base64-tagged as UTF-8 and decoded back to the same native string.
Non-finite floats use a tagged textual form because JSON forbids
infinities."
  (cond
   ((null value) :null)
   ((stringp value)
    (cond
     ((not (multibyte-string-p value))
      (vector "b64" (base64-encode-string value t)))
     ((sql-grid-db--raw-byte-string-p value)
      (vector "text"
              (base64-encode-string
               (encode-coding-string value 'utf-8) t)))
     (t value)))
   ((and (floatp value)
         (string-match-p "INF\\|NaN" (number-to-string value)))
    (vector "float" (number-to-string value)))
   (t value)))

(defun sql-grid-db--decode-wire-value (value)
  "Decode scalar VALUE received from the SQLite worker."
  (cond
   ((vectorp value)
    (if (and (= (length value) 2) (stringp (aref value 1)))
        (cond
         ((equal (aref value 0) "b64")
          (base64-decode-string (aref value 1)))
         ((equal (aref value 0) "text")
          (let ((decoded
                 (decode-coding-string
                  (base64-decode-string (aref value 1)) 'utf-8)))
            (if (multibyte-string-p decoded)
                decoded
              (string-to-multibyte decoded))))
         ((equal (aref value 0) "float")
          (string-to-number (aref value 1)))
         (t (error "Invalid SQLite worker cell value")))
      (error "Invalid SQLite worker cell value")))
   ((stringp value)
    (if (multibyte-string-p value) value (string-to-multibyte value)))
   (t value)))

(defun sql-grid-db--decode-wire-list (value)
  "Decode JSON array VALUE from the SQLite worker into a list."
  (mapcar #'sql-grid-db--decode-wire-value (append value nil)))

(defun sql-grid-db--sqlite-with-verb (sql)
  "Return the main statement verb after SQL's WITH clause."
  (let ((i 0) (length (length sql)) (depth 0) (state 'normal) verb done)
    (while (and (not done) (< i length))
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (cond
         ((eq state 'normal)
          (cond
           ((memq c '(?\s ?\t ?\n ?\r ?\f)))
           ((and (= c ?-) (eq next ?-))
            (setq state 'line-comment i (1+ i)))
           ((and (= c ?/) (eq next ?*))
            (setq state 'block-comment i (1+ i)))
           ((= c ?') (setq state 'single))
           ((= c ?\") (setq state 'double))
           ((= c ?`) (setq state 'backtick))
           ((= c ?\[) (setq state 'bracket))
           ((= c ?\() (setq depth (1+ depth)))
           ((= c ?\)) (setq depth (max 0 (1- depth))))
           ((and (= depth 0)
                 (or (<= ?a (downcase c) ?z) (= c ?_)))
            (let ((end i))
              (while (and (< end length)
                          (let ((char (aref sql end)))
                            (or (<= ?a (downcase char) ?z)
                                (<= ?0 char ?9) (= char ?_))))
                (setq end (1+ end)))
              (let ((word (downcase (substring sql i end))))
                (when (member word '("select" "insert" "update" "delete"
                                     "replace" "values"))
                  (setq verb word done t)))
              (setq i (1- end))))))
         ((eq state 'single)
          (when (= c ?')
            (if (eq next ?') (setq i (1+ i))
              (setq state 'normal))))
         ((eq state 'double)
          (when (= c ?\")
            (if (eq next ?\") (setq i (1+ i))
              (setq state 'normal))))
         ((eq state 'backtick)
          (when (= c ?`)
            (if (eq next ?`) (setq i (1+ i))
              (setq state 'normal))))
         ((eq state 'bracket)
          (when (= c ?\])
            (if (eq next ?\]) (setq i (1+ i))
              (setq state 'normal))))
         ((eq state 'line-comment)
          (when (memq c '(?\n ?\r)) (setq state 'normal)))
         ((eq state 'block-comment)
          (when (and (= c ?*) (eq next ?/))
            (setq state 'normal i (1+ i)))))
        (setq i (1+ i))))
    verb))

(defun sql-grid-db--sqlite-statement-kind (sql)
  "Classify SQL as `query', `write', or `other' for SQLite execution."
  (let ((keyword (sql-grid-db--first-keyword sql)))
    (if (equal keyword "with")
        (let ((verb (sql-grid-db--sqlite-with-verb sql)))
          (cond
           ((member verb '("insert" "update" "delete" "replace")) 'write)
           ((member verb '("select" "values")) 'query)
           (t 'other)))
      (cond
       ((member keyword '("insert" "update" "delete" "replace")) 'write)
       ((member keyword '("select" "values" "pragma" "explain")) 'query)
       (t 'other)))))

(defun sql-grid-db--sqlite-results (handle sql)
  "Execute one SQL statement on SQLite HANDLE and return result structs.
Write statements report their affected-row count, including WITH writes
and RETURNING statements.  Reads and DDL never expose a stale changes()
value."
  (unless (sql-grid-db--single-statement-p sql 'sqlite)
    (user-error "Only one SQL statement may be executed at a time"))
  (let* ((started (float-time))
         (kind (sql-grid-db--sqlite-statement-kind sql))
         (returning (and (eq kind 'write)
                         (sql-grid-db--contains-keyword-p sql "returning")))
         (set nil)
         columns rows affected)
    (unwind-protect
        (if (or (eq kind 'query) returning)
            (progn
              (setq set (sqlite-select handle sql nil 'set))
              (when set
                (setq columns (sqlite-columns set))
                (let (row)
                  (while (setq row (sqlite-next set))
                    (push row rows)))
                (setq rows (nreverse rows))))
          (let ((count (sqlite-execute handle sql)))
            (when (eq kind 'write)
              (setq affected count))))
      (when set
        (sqlite-finalize set)
        (setq set nil)
        (when returning
          (setq affected
                (caar (sqlite-select handle "SELECT changes()"))))))
    (list (sql-grid-result-create
           :columns columns
           :rows rows
           :message (unless (eq kind 'query) "Statement completed")
           :affected-rows affected
           :elapsed (- (float-time) started)))))

(defun sql-grid-db--sqlite-teardown (connection &optional no-wait)
  "Close and invalidate CONNECTION's SQLite worker.
SQLite rolls back an uncommitted transaction when the worker's database
connection closes.  Never reconnect automatically.  With NO-WAIT, do not
wait for the worker process to finish exiting."
  (let ((socket (sql-grid-connection-handle connection))
        (worker (sql-grid-connection-process connection))
        (buffer (sql-grid-connection-worker-buffer connection))
        (pending (sql-grid-connection-pending connection)))
    (setf (sql-grid-connection-closed connection) t
          (sql-grid-connection-handle connection) nil
          (sql-grid-connection-process connection) nil
          (sql-grid-connection-worker-buffer connection) nil
          (sql-grid-connection-worker-token connection) nil
          (sql-grid-connection-on-close connection) nil)
    (when (hash-table-p pending)
      (clrhash pending))
    (when (and socket (process-live-p socket))
      (delete-process socket))
    (when worker
      (when (process-live-p worker)
        (delete-process worker))
      (unless no-wait
        (let ((deadline (+ (float-time) 2)))
          (while (and (process-live-p worker)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    connection))
(defun sql-grid-db--sqlite-decode-results (objects)
  "Convert result OBJECTS from the SQLite worker."
  (mapcar
   (lambda (object)
     (sql-grid-result-create
      :columns (sql-grid-db--decode-wire-list
                (alist-get 'columns object))
      :rows (mapcar #'sql-grid-db--decode-wire-list
                    (append (alist-get 'rows object) nil))
      :message (alist-get 'message object)
      :elapsed (alist-get 'elapsed object)
      :affected-rows (alist-get 'affected-rows object)))
   (append objects nil)))

(defun sql-grid-db--sqlite-filter (process chunk)
  "Dispatch worker replies received from PROCESS."
  (let ((text (concat (or (process-get process 'sql-grid-input) "") chunk))
        newline)
    (while (setq newline (string-match "\n" text))
      (let ((line (substring text 0 newline)))
        (setq text (substring text (1+ newline)))
        (unless (string-empty-p line)
          (let ((reply (condition-case nil
                           (json-parse-string
                            line :object-type 'alist
                            :array-type 'array
                            :null-object nil :false-object :false)
                         (error :invalid))))
            (if (eq reply :invalid)
                (delete-process process)
              (let* ((id (alist-get 'id reply))
                     (connection (process-get process
                                              'sql-grid-connection))
                     (pending (and connection
                                   (hash-table-p
                                    (sql-grid-connection-pending connection))
                                   (gethash id (sql-grid-connection-pending
                                                connection)))))
                (when pending
                  (remhash id (sql-grid-connection-pending connection))
                  (let* ((error-data (alist-get 'error reply))
                         (condition
                          (and error-data
                               (list (intern (aref error-data 0))
                                     (sql-grid-db--decode-wire-value
                                      (aref error-data 1)))))
                         (results
                          (and (not condition)
                               (sql-grid-db--sqlite-decode-results
                                (alist-get 'results reply)))))
                    (if (sql-grid-job-p pending)
                        (sql-grid-db--async-finish pending results condition)
                      (aset pending 1 t)
                      (aset pending 2 results)
                      (aset pending 3 condition))))))))))
    (process-put process 'sql-grid-input text)))

(defun sql-grid-db--sqlite-request (connection sql &optional callback)
  "Send SQL to CONNECTION's worker; with CALLBACK, return an async job."
  (let* ((process (sql-grid-connection-handle connection))
         (id (1+ (or (sql-grid-connection-next-id connection) 0)))
         (pending (if callback
                      (sql-grid-job-create
                       :connection connection :callback callback :id id
                       :sql sql :started (float-time))
                    (vector 'sync nil nil nil))))
    (unless (and process (process-live-p process))
      (error "SQLite connection is not open"))
    (setf (sql-grid-connection-next-id connection) id)
    (puthash id pending (sql-grid-connection-pending connection))
    (when callback
      (push pending (sql-grid-connection-jobs connection)))
    (condition-case err
        (process-send-string
         process
         (concat (json-serialize
                  `((id . ,id)
                    (operation . "execute")
                    (token . ,(sql-grid-connection-worker-token connection))
                    (sql . ,sql)))
                 "\n"))
      (error
       (remhash id (sql-grid-connection-pending connection))
       (when callback
         (setf (sql-grid-connection-jobs connection)
               (delq pending (sql-grid-connection-jobs connection))))
       (signal (car err) (cdr err))))
    (if callback
        pending
      (while (not (aref pending 1))
        (unless (process-live-p process)
          (remhash id (sql-grid-connection-pending connection))
          (error "SQLite worker stopped before replying"))
        (accept-process-output process 0.1))
      (when (aref pending 3)
        (signal (car (aref pending 3)) (cdr (aref pending 3))))
      (aref pending 2))))

(defun sql-grid-db--sqlite-sentinel (process _event)
  "Fail pending requests if the SQLite worker socket closes.
After teardown and pending-job callbacks, invoke CONNECTION's
`on-close' callback for unexpected loss.  Explicit teardown clears the
callback and never invokes it."
  (when (memq (process-status process) '(exit signal closed failed))
    (let ((connection (process-get process 'sql-grid-connection)))
      (when (and connection
                 (eq process (sql-grid-connection-handle connection)))
        (let ((closing-owner
               (not (sql-grid-connection-closing connection))))
          (when closing-owner
            (setf (sql-grid-connection-closing connection) t))
          (unwind-protect
              (let ((pending (sql-grid-connection-pending connection))
                    (on-close (sql-grid-connection-on-close connection))
                    entries callback-errors)
                (when (hash-table-p pending)
                  (maphash (lambda (id value)
                             (push (cons id value) entries))
                           pending))
                (sql-grid-db--sqlite-teardown connection t)
                (dolist (entry entries)
                  (let ((value (cdr entry)))
                    (if (sql-grid-job-p value)
                        (let ((err (sql-grid-db--async-finish
                                    value nil
                                    '(error "SQLite worker disconnected") t)))
                          (when err (push err callback-errors)))
                      (aset value 1 t)
                      (aset value 3 '(error "SQLite worker disconnected")))))
                (when on-close
                  (condition-case err
                      (funcall on-close connection)
                    ((error quit) (push err callback-errors))))
                (when callback-errors
                  (signal (caar callback-errors) (cdar callback-errors))))
            (when closing-owner
              (setf (sql-grid-connection-closing connection) nil))))))))

(defun sql-grid-db--sqlite-worker-connect (connection)
  "Start CONNECTION's persistent native SQLite worker.
The worker receives its authentication token over stdin, never argv."
  (let* ((library (locate-library "sql-grid-db"))
         (directory (file-name-directory library))
         (worker-file (expand-file-name "sql-grid-db-worker.el" directory))
         (program (or (executable-find invocation-name)
                      (error "Cannot locate Emacs executable")))
         (buffer (generate-new-buffer " *sql-grid-sqlite-worker*"))
         (token (secure-hash
                 'sha256
                 (format "%s-%s-%s-%s" (float-time) (emacs-pid)
                         (random most-positive-fixnum)
                         (make-temp-name "sql-grid"))))
         (worker (make-process
                  :name (concat "sql-grid-sqlite-worker-"
                                (substring token 0 8)) :buffer buffer
                  :command (list program "-Q" "--batch" "-L" directory
                                 "-l" worker-file "--"
                                 (sql-grid-connection-database connection))
                  :connection-type 'pipe :coding 'utf-8-unix :noquery t))
         (deadline (+ (float-time) 10))
         port socket)
    (unwind-protect
        (progn
          (process-send-string worker (concat token "\n"))
          (while (and (process-live-p worker)
                      (< (float-time) deadline)
                      (not port))
            (accept-process-output worker 0.1)
            (with-current-buffer buffer
              (goto-char (point-min))
              (when (re-search-forward "^ready \\([0-9]+\\)$" nil t)
                (setq port (string-to-number (match-string 1))))))
          (unless port
            (error "SQLite worker did not start: %s"
                   (with-current-buffer buffer (buffer-string))))
          (setq socket
                (make-network-process
                 :name (concat "sql-grid-sqlite-connection-"
                               (substring token 0 8)) :host "127.0.0.1"
                 :service port :family 'ipv4 :coding 'utf-8-unix
                 :noquery t :filter #'sql-grid-db--sqlite-filter
                 :sentinel #'sql-grid-db--sqlite-sentinel))
          (process-put socket 'sql-grid-connection connection)
          (process-put socket 'sql-grid-input "")
          (setf (sql-grid-connection-handle connection) socket
                (sql-grid-connection-process connection) worker
                (sql-grid-connection-worker-buffer connection) buffer
                (sql-grid-connection-worker-token connection) token
                (sql-grid-connection-pending connection) (make-hash-table)
                (sql-grid-connection-next-id connection) 0)
          connection)
      (unless socket
        (when (process-live-p worker) (delete-process worker))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))
(defun sql-grid-db-sqlite--where-predicate (sql)
  "Return top-level WHERE predicate from index SQL definition."
  (let ((i 0) (length (length sql)) (depth 0) (state 'normal) start)
    (while (and (< i length) (not start))
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (pcase state
          ('normal
           (cond
            ((and (= c ?-) (eq next ?-)) (setq state 'line i (1+ i)))
            ((and (= c ?/) (eq next ?*)) (setq state 'comment i (1+ i)))
            ((= c ?') (setq state 'single))
            ((= c ?\") (setq state 'double))
            ((= c ?`) (setq state 'backtick))
            ((= c ?\[) (setq state 'bracket))
            ((= c ?\() (setq depth (1+ depth)))
            ((= c ?\)) (setq depth (max 0 (1- depth))))
            ((and (= depth 0)
                  (<= (+ i 5) length)
                  (equal (upcase (substring sql i (+ i 5))) "WHERE")
                  (let ((before (and (> i 0) (aref sql (1- i))))
                        (after (and (< (+ i 5) length)
                                    (aref sql (+ i 5)))))
                    (and (or (null before)
                             (not (string-match-p
                                   "[[:alnum:]_$]" (char-to-string before))))
                         (or (null after)
                             (not (string-match-p
                                   "[[:alnum:]_$]" (char-to-string after)))))))
             (setq start (+ i 5) i (+ i 4)))))
          ('single
           (when (= c ?')
             (if (eq next ?') (setq i (1+ i))
               (setq state 'normal))))
          ('double
           (when (= c ?\")
             (if (eq next ?\") (setq i (1+ i))
               (setq state 'normal))))
          ('backtick
           (when (= c ?`)
             (if (eq next ?`) (setq i (1+ i))
               (setq state 'normal))))
          ('bracket (when (= c ?\]) (setq state 'normal)))
          ('line (when (memq c '(?\n ?\r)) (setq state 'normal)))
          ('comment
           (when (and (= c ?*) (eq next ?/))
             (setq state 'normal i (1+ i)))))
        (setq i (1+ i))))
    (when start
      (let ((predicate (string-trim (substring sql start))))
        (when (string-suffix-p ";" predicate)
          (setq predicate (string-trim (substring predicate 0 -1))))
        predicate))))

(defun sql-grid-db-sqlite--enqueue-schema (connection enqueue add-table)
  "Queue normalized SQLite metadata queries for CONNECTION."
  (ignore connection)
  (funcall enqueue
   "PRAGMA database_list"
   (lambda (databases)
     (dolist (database databases)
       (let* ((schema (nth 1 database))
              (schema-id (sql-grid-db--quote-sqlite-id schema)))
         (funcall
          enqueue
          (format "SELECT name, type FROM %s.sqlite_schema WHERE type IN ('table','view') AND name NOT GLOB 'sqlite_*' ORDER BY name" schema-id)
          (lambda (tables)
            (dolist (table tables)
              (let* ((name (car table))
                     (table-id (sql-grid-db--quote-sqlite-id name))
                     (entry (list :schema schema :name name
                                  :kind (nth 1 table) :columns nil
                                  :indexes nil :foreign-keys nil)))
                (funcall add-table entry)
                (funcall enqueue
                 (format "PRAGMA %s.table_xinfo(%s)" schema-id table-id)
                 (lambda (rows)
                   (plist-put
                    entry :columns
                    (mapcar (lambda (row)
                              (list :name (nth 1 row) :type (or (nth 2 row) "")
                                    :nullable (= (or (nth 3 row) 0) 0)
                                    :primary-key (> (or (nth 5 row) 0) 0)
                                    :default (nth 4 row) :hidden (> (or (nth 6 row) 0) 0)
                                    :key nil :extra nil))
                            rows))))
                (funcall enqueue
                 (format "PRAGMA %s.index_list(%s)" schema-id table-id)
                 (lambda (indexes)
                   (dolist (index indexes)
                     (let ((index-name (nth 1 index)))
                       (funcall enqueue
                        (format "PRAGMA %s.index_info(%s)" schema-id
                                (sql-grid-db--quote-sqlite-id index-name))
                        (lambda (columns)
                          (let ((ordered (sort (copy-sequence columns)
                                               (lambda (a b) (< (nth 0 a) (nth 0 b))))))
                            (funcall enqueue
                             (format "SELECT sql FROM %s.sqlite_schema WHERE type='index' AND name=%s"
                                     schema-id
                                     (sql-grid-db--quote-literal index-name))
                             (lambda (definitions)
                               (let* ((predicate
                                       (and (= (or (nth 4 index) 0) 1)
                                            (sql-grid-db-sqlite--where-predicate
                                             (caar definitions))))
                                      (index-entry
                                       (list :name index-name
                                             :unique (= (nth 2 index) 1)
                                             :columns (mapcar (lambda (column) (nth 2 column)) ordered)
                                             :include nil :predicate predicate
                                             :partial (= (nth 4 index) 1))))
                                 (plist-put entry :indexes
                                            (append (plist-get entry :indexes)
                                                    (list index-entry)))))))))))
                (funcall enqueue
                 (format "PRAGMA %s.foreign_key_list(%s)" schema-id table-id)
                 (lambda (rows)
                   (let ((groups nil))
                     (dolist (row rows)
                       (let* ((id (nth 0 row))
                              (group (assq id groups))
                              (part (list :name nil :sequence (1+ (nth 1 row))
                                          :column (nth 3 row)
                                          :target-schema nil :target-table (nth 2 row)
                                          :target-column (nth 4 row))))
                         (unless group
                           (setq group (cons id nil)
                                 groups (append groups (list group))))
                         (setcdr group (append (cdr group) (list part)))))
                     (plist-put entry :foreign-keys
                                (apply #'append (mapcar #'cdr groups)))))))))))))))))
(provide 'sql-grid-db-sqlite)
;;; sql-grid-db-sqlite.el ends here

