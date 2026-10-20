;;; sql-grid-db-mysql.el --- MySQL and MariaDB driver -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'xml)
(require 'subr-x)
(require 'sql-grid-db-core)
(defcustom sql-grid-mysql-program "mysql"
  "Path to the MySQL client executable."
  :type 'file
  :group 'sql-grid-db)

(defcustom sql-grid-mariadb-program "mariadb"
  "Path to the MariaDB client executable."
  :type 'file
  :group 'sql-grid-db)

(defun sql-grid-db--option-escape (value)
  "Escape VALUE for a quoted MySQL option-file value."
  (when (string-match-p "[\0\n\r]" value)
    (user-error "Credential contains an unsupported control character"))
  (concat "\"" (replace-regexp-in-string "[\\\\\"]" "\\\\\\&" value t)
          "\""))

(defun sql-grid-db--server-remove-echo (text sql)
  "Remove the exact echoed SQL frame from client output TEXT."
  (with-temp-buffer
    (insert text)
    (dolist (body (list (string-trim sql)
                        (concat (string-trim sql) ";")
                        (concat (string-trim sql) "\n;")))
      (goto-char (point-min))
      (let ((frame (concat "--------------\n" body "\n--------------")))
        (when (search-forward frame nil t)
          (delete-region (match-beginning 0) (match-end 0)))))
    (buffer-string)))

(defun sql-grid-db--server-output-without-xml (text sql)
  "Remove XML and the echoed SQL from client output TEXT."
  (replace-regexp-in-string
   "<resultset\\(?:.\\|\n\\)*?</resultset>" ""
   (sql-grid-db--server-remove-echo text sql) t))

(defun sql-grid-db--server-affected-rows (text sql)
  "Return an affected-row count explicitly reported in client TEXT."
  (let ((output (sql-grid-db--server-output-without-xml text sql)))
    (when (string-match
           "\\_<Query OK,\\s-*\\([0-9]+\\) row\\(?:s\\)? affected\\_>"
           output)
      (string-to-number (match-string 1 output)))))

(defun sql-grid-db--xml-fragments (text)
  "Extract all resultset elements from client output TEXT."
  (let ((start 0) fragments)
    (while (setq start (string-match "<resultset\\b" text start))
      (let ((end (string-match "</resultset>" text start)))
        (if end
            (progn
              (push (substring text start (+ end 12)) fragments)
              (setq start (+ end 12)))
          (setq start (length text)))))
    (mapconcat #'identity (nreverse fragments) "")))

(defun sql-grid-db--xml-results (text &optional sql)
  "Parse client XML TEXT into result structs, preserving duplicate fields."
  (let* ((input (if sql (sql-grid-db--server-remove-echo text sql) text))
         (xml-text (sql-grid-db--xml-fragments input))
         (without-declaration
          (replace-regexp-in-string
           "\\`[ \t\r\n]*<\\?xml[^?]*\\?>" "" xml-text))
         (wrapped (concat "<sql-grid-root>" without-declaration
                          "</sql-grid-root>"))
         results)
    (with-temp-buffer
      (insert wrapped)
      (goto-char (point-min))
      (let* ((tree (xml-parse-region (point-min) (point-max)))
             (root (car tree)))
        (dolist (node (xml-node-children root))
          (when (and (consp node) (eq (car node) 'resultset))
            (let* ((row-nodes
                    (cl-remove-if-not
                     (lambda (item) (and (consp item) (eq (car item) 'row)))
                     (xml-node-children node)))
                   (first-row (car row-nodes))
                   (fields
                    (and first-row
                         (cl-remove-if-not
                          (lambda (field)
                            (and (consp field) (eq (car field) 'field)))
                          (xml-node-children first-row))))
                   (columns
                    (mapcar (lambda (field)
                              (cdr (assq 'name (cadr field))))
                            fields))
                   rows)
              (dolist (row-node row-nodes)
                (let ((row-fields
                       (cl-remove-if-not
                        (lambda (field)
                          (and (consp field) (eq (car field) 'field)))
                        (xml-node-children row-node))))
                  (push
                   (mapcar
                    (lambda (field)
                      (if (cl-some
                           (lambda (attribute)
                             (and (string-match-p
                                   "\\(?:^\\|:\\)nil\\'"
                                   (symbol-name (car attribute)))
                                  (equal (cdr attribute) "true")))
                           (cadr field))
                          nil
                        (let ((content (xml-node-children field)))
                          (if content (mapconcat #'identity content "") ""))))
                    row-fields)
                   rows)))
              (push (sql-grid-result-create :columns columns
                                             :rows (nreverse rows))
                    results))))))
    (nreverse results)))

(defun sql-grid-db--server-leading-command-p (sql)
  "Return non-nil when SQL begins with a MySQL client command.
The mysql client only treats a command word as a command at the start
of a statement.  Leading whitespace and SQL comments are skipped;
newlines, quoted text, and comments later in SQL are data, not command
boundaries.  Client commands can escape to a shell or read files, so
they are never accepted as SQL."
  (let ((i 0) (length (length sql)) done)
    (while (and (not done) (< i length))
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r ?\f))
          (setq i (1+ i)))
         ((and (= c ?-) (eq next ?-)
               (or (>= (+ i 2) length)
                   (memq (aref sql (+ i 2))
                         '(?\s ?\t ?\n ?\r ?\f ?\v))))
          (setq i (or (string-match "[\n\r]" sql (+ i 2)) length)))
         ((= c ?#)
          (setq i (or (string-match "[\n\r]" sql (1+ i)) length)))
         ((and (= c ?/) (eq next ?*))
          (let ((end (string-match "\\*/" sql (+ i 2))))
            (setq i (if end (+ end 2) length))))
         (t (setq done t)))))
    (let ((rest (substring sql i)))
      (or (string-prefix-p "\\" rest)
          (string-match-p
           "\\`\\(?:source\\|system\\|delimiter\\|shell\\)\\(?:[ \t\n\r]\\|\\'\\)"
           (downcase rest))))))

(defun sql-grid-db--validate-server-sql (sql backend)
  "Reject SQL that is not one safe statement for BACKEND's client."
  (unless (sql-grid-db--single-statement-p sql backend)
    (user-error "Only one SQL statement may be executed at a time"))
  (when (sql-grid-db--server-leading-command-p sql)
    (user-error "MySQL client commands are not accepted as SQL")))

(defun sql-grid-db--server-prepare-client (connection)
  "Prepare CONNECTION credentials file and client arguments."
  (let* ((defaults (make-temp-file "sql-grid-client-"))
         (password (sql-grid-connection-password connection))
         (user (sql-grid-connection-user connection))
         (database (sql-grid-connection-database connection))
         prepared)
    (unwind-protect
        (progn
          (set-file-modes defaults #o600)
          (with-temp-file defaults
            (insert "[client]\n" "password="
                    (sql-grid-db--option-escape (or password "")) "\n"))
          (setq prepared
                (cons
                 defaults
                 (append (list (concat "--defaults-file=" defaults)
                               "--batch" "--binary-mode" "--xml"
                               "--verbose" "--verbose"
                               "--local-infile=0" "--no-beep"
                               "--default-character-set=utf8mb4")
                         (when (sql-grid-connection-host connection)
                           (list (concat "--host="
                                         (sql-grid-connection-host connection))))
                         (when (sql-grid-connection-port connection)
                           (list (format "--port=%s"
                                         (sql-grid-connection-port connection))))
                         (when user (list (concat "--user=" user)))
                         (when database
                           (list (concat "--database=" database))))))
          prepared)
      (unless prepared
        (when (file-exists-p defaults) (delete-file defaults))))))

(defun sql-grid-db--server-execute (connection sql)
  "Execute SQL using CONNECTION's command-line client."
  (sql-grid-db--validate-server-sql
   sql (sql-grid-connection-backend connection))
  (let* ((started (float-time))
         (backend (sql-grid-connection-backend connection))
         (program (pcase backend
                    ('mysql sql-grid-mysql-program)
                    ('mariadb sql-grid-mariadb-program)))
         (prepared (sql-grid-db--server-prepare-client connection))
         (defaults (car prepared))
         (args (cdr prepared))
         (stderr-file (make-temp-file "sql-grid-stderr-"))
         status output error-text
         (input-buffer (generate-new-buffer " *sql-grid-input*"))
         (output-buffer (generate-new-buffer " *sql-grid-output*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buffer
            (insert sql)
            (let ((coding-system-for-write 'utf-8-unix)
                  (coding-system-for-read 'utf-8-unix))
              (setq status
                    (apply #'call-process-region
                           (point-min) (point-max) program nil
                           (list output-buffer stderr-file) nil args))))
          (setq output (with-current-buffer output-buffer (buffer-string)))
          (setq error-text
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8-unix))
                    (insert-file-contents stderr-file))
                  (buffer-string)))
          (unless (and (integerp status) (= status 0))
            (error "%s client failed%s%s" backend
                   (if error-text ": " "") (or error-text "")))
          (let ((results (or (sql-grid-db--xml-results output sql)
                             (list (sql-grid-result-create
                                    :columns nil :rows nil
                                    :message "Statement completed")))))
            (dolist (result results)
              (setf (sql-grid-result-elapsed result)
                    (- (float-time) started)
                    (sql-grid-result-affected-rows result)
                    (sql-grid-db--server-affected-rows output sql)))
            results))
      (when (buffer-live-p input-buffer) (kill-buffer input-buffer))
      (when (buffer-live-p output-buffer) (kill-buffer output-buffer))
      (when (file-exists-p defaults) (delete-file defaults))
      (when (file-exists-p stderr-file) (delete-file stderr-file)))))

(defun sql-grid-db--server-async-sentinel (process _event)
  "Deliver the result of asynchronous server PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let ((job (process-get process 'sql-grid-job)))
      (when job
        (let ((status (process-exit-status process))
              (output (with-current-buffer (process-buffer process)
                        (buffer-string)))
              (stderr (with-current-buffer (process-get process 'sql-grid-stderr)
                        (buffer-string))))
          (if (and (= status 0) (not (sql-grid-job-cancelled job)))
              (condition-case err
                  (let ((results
                         (or (sql-grid-db--xml-results
                              output (sql-grid-job-sql job))
                             (list (sql-grid-result-create
                                    :columns nil :rows nil
                                    :message "Statement completed")))))
                    (dolist (result results)
                      (setf (sql-grid-result-elapsed result)
                            (- (float-time) (sql-grid-job-started job))
                            (sql-grid-result-affected-rows result)
                            (sql-grid-db--server-affected-rows
                             output (sql-grid-job-sql job))))
                    (sql-grid-db--async-finish job results nil))
                (error (sql-grid-db--async-finish job nil err)))
            (sql-grid-db--async-finish
             job nil
             (if (sql-grid-job-cancelled job)
                 '(quit "Query cancelled")
               (list 'error
                     (format "%s client failed%s%s"
                             (sql-grid-connection-backend
                              (sql-grid-job-connection job))
                             (if (string-empty-p stderr) "" ": ")
                             stderr))))))))))
(defun sql-grid-db--server-execute-async (connection sql callback)
  "Execute SQL asynchronously on a server using CALLBACK."
  (unless (functionp callback)
    (error "Callback must be a function"))
  (unless (memq (sql-grid-connection-backend connection) '(mysql mariadb))
    (error "Unsupported server backend"))
  (sql-grid-db--validate-server-sql
   sql (sql-grid-connection-backend connection))
  (let* ((backend (sql-grid-connection-backend connection))
         (program (pcase backend
                    ('mysql sql-grid-mysql-program)
                    ('mariadb sql-grid-mariadb-program)))
         (prepared (sql-grid-db--server-prepare-client connection))
         (defaults (car prepared))
         (args (cdr prepared))
         (output (generate-new-buffer " *sql-grid-async-output*"))
         (stderr (generate-new-buffer " *sql-grid-async-stderr*"))
         (job (sql-grid-job-create
               :connection connection :callback callback
               :buffers (list output stderr) :defaults defaults :sql sql
               :started (float-time)))
         process)
    (condition-case err
        (progn
          (setq process
                (make-process :name "sql-grid-query" :buffer output
                              :stderr stderr :command (cons program args)
                              :connection-type 'pipe :coding 'utf-8-unix
                              :sentinel #'sql-grid-db--server-async-sentinel))
          (process-put process 'sql-grid-job job)
          (process-put process 'sql-grid-stderr stderr)
          (setf (sql-grid-job-process job) process)
          (push job (sql-grid-connection-jobs connection))
          (process-send-string process sql)
          (process-send-eof process)
          job)
      (error
       (when (and process (process-live-p process))
         (process-put process 'sql-grid-job nil)
         (delete-process process))
       (setf (sql-grid-connection-jobs connection)
             (delq job (sql-grid-connection-jobs connection)))
       (sql-grid-db--async-cleanup job)
       (signal (car err) (cdr err))))))
(defun sql-grid-db-mysql--enqueue-schema (connection enqueue add-table)
  "Queue normalized MySQL or MariaDB metadata queries for CONNECTION."
  (funcall
   enqueue "SHOW FULL TABLES"
   (lambda (tables)
     (let ((schema (sql-grid-connection-database connection))
           (entries nil))
       (dolist (row tables)
         (let* ((name (car row))
                (entry (list :schema schema :name name
                             :kind (if (equal (cadr row) "BASE TABLE")
                                       "table" "view")
                             :columns nil :indexes nil :foreign-keys nil))
                (table-id (sql-grid-db--quote-mysql-id name)))
           (push entry entries)
           (funcall add-table entry)
           (funcall
            enqueue (format "SHOW FULL COLUMNS FROM %s" table-id)
            (lambda (column-rows)
              (plist-put
               entry :columns
               (mapcar
                (lambda (row)
                  (list :name (nth 0 row) :type (or (nth 1 row) "")
                        :nullable (equal (nth 3 row) "YES")
                        :primary-key (equal (nth 4 row) "PRI")
                        :default (nth 5 row) :hidden nil
                        :key (pcase (nth 4 row)
                               ("UNI" 'unique)
                               ("MUL" 'index))
                        :extra (nth 6 row)))
                column-rows))))
           (funcall
            enqueue (format "SHOW INDEX FROM %s" table-id)
            (lambda (index-rows)
              (let ((groups nil))
                (dolist (row index-rows)
                  (let* ((name (nth 2 row))
                         (group (assoc name groups)))
                    (unless group
                      (setq group (cons name nil)
                            groups (append groups (list group))))
                    (setcdr group (cons row (cdr group)))))
                (plist-put
                 entry :indexes
                 (mapcar
                  (lambda (group)
                    (let ((ordered
                           (sort (cdr group)
                                 (lambda (a b)
                                   (< (string-to-number (or (nth 3 a) "0"))
                                      (string-to-number (or (nth 3 b) "0")))))))
                      (list :name (car group)
                            :unique (equal (nth 1 (car ordered)) "0")
                            :columns (mapcar (lambda (item) (nth 4 item)) ordered)
                            :include nil :predicate nil :partial nil)))
                  groups)))))))
       (funcall
        enqueue
        "SELECT CONSTRAINT_NAME, TABLE_NAME, COLUMN_NAME, REFERENCED_TABLE_SCHEMA, REFERENCED_TABLE_NAME, REFERENCED_COLUMN_NAME, ORDINAL_POSITION FROM information_schema.KEY_COLUMN_USAGE WHERE TABLE_SCHEMA = DATABASE() AND REFERENCED_TABLE_NAME IS NOT NULL ORDER BY TABLE_NAME, CONSTRAINT_NAME, ORDINAL_POSITION"
        (lambda (rows)
          (dolist (row rows)
            (let ((entry (cl-find (nth 1 row) entries
                                  :key (lambda (item) (plist-get item :name))
                                  :test #'equal)))
              (when entry
                (plist-put
                 entry :foreign-keys
                 (append
                  (plist-get entry :foreign-keys)
                  (list (list :name (nth 0 row)
                              :sequence (string-to-number (or (nth 6 row) "0"))
                              :column (nth 2 row)
                              :target-schema (nth 3 row)
                              :target-table (nth 4 row)
                              :target-column (nth 5 row))))))))))))))
(provide 'sql-grid-db-mysql)
;;; sql-grid-db-mysql.el ends here

