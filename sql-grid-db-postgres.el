;;; sql-grid-db-postgres.el --- PostgreSQL driver for sql-grid -*- lexical-binding: t; -*-

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
(require 'subr-x)
(require 'sql-grid-db-core)
(defcustom sql-grid-postgres-program "psql"
  "Path to the PostgreSQL client executable."
  :type 'file
  :group 'sql-grid-db)
 
(defun sql-grid-db--postgres-password-escape (value)
  "Escape VALUE for a PostgreSQL password-file field."
  (when (string-match-p "[\0\n\r]" value)
    (user-error "Credential contains an unsupported control character"))
  (replace-regexp-in-string
   "[][\\\\:]" "\\\\\\&" value t))

(defun sql-grid-db--postgres-password-file (connection)
  "Create a mode-0600 libpq password file for CONNECTION."
  (let ((file (make-temp-file "sql-grid-pgpass-"))
        completed)
    (unwind-protect
        (progn
          (set-file-modes file #o600)
          (with-temp-file file
            (insert
             (mapconcat
              #'sql-grid-db--postgres-password-escape
              (list (or (sql-grid-connection-host connection) "*")
                    (number-to-string
                     (or (sql-grid-connection-port connection) 5432))
                    (or (sql-grid-connection-database connection) "*")
                    (or (sql-grid-connection-user connection)
                        (user-login-name))
                    (or (sql-grid-connection-password connection) ""))
              ":")
             "\n"))
          (setq completed t)
          file)
      (unless completed
        (ignore-errors (delete-file file))))))



(defun sql-grid-db--postgres-single-statement-p (sql)
  "Return non-nil when SQL is at most one PostgreSQL statement."
  (let ((i 0) (length (length sql)) (state 'normal)
        (terminated nil) (valid t) (depth 0) dollar)
    (while (and (< i length) valid)
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (pcase state
          ('normal
           (cond
            ((memq c '(?\s ?\t ?\n ?\r ?\f)))
            ((and (= c ?-) (eq next ?-))
             (setq state 'line-comment i (1+ i)))
            ((and (= c ?/) (eq next ?*))
             (setq state 'block-comment depth 1 i (1+ i)))
            (terminated (setq valid nil))
            ((and (= c ?$)
                  (or (= i 0)
                      (not (string-match-p
                            "\\`[[:alnum:]_$]\\'"
                            (char-to-string (aref sql (1- i))))))
                  (let ((start (string-match
                                "\\$\\(?:[[:alpha:]_][[:alnum:]_]*\\)?\\$"
                                sql i)))
                    (and start (= start i))))
             (setq dollar (match-string 0 sql)
                   state 'dollar
                   i (+ i (length dollar) -1)))
            ((= c ?')
             (if (and (> i 0) (memq (aref sql (1- i)) '(?e ?E))
                      (or (= i 1)
                          (not (string-match-p
                                "\\`[[:alnum:]_$]\\'"
                                (char-to-string (aref sql (- i 2)))))))
                 (setq state 'escape-single)
               (setq state 'single)))
            ((= c ?\") (setq state 'double))
            ((= c ?\;) (setq terminated t))))
          ('single
           (if (= c ?')
               (if (eq next ?') (setq i (1+ i))
                 (setq state 'normal))))
          ('escape-single
           (cond ((= c ?\\) (setq i (1+ i)))
                 ((= c ?') (if (eq next ?') (setq i (1+ i))
                             (setq state 'normal)))))
          ('double
           (when (= c ?\")
             (if (eq next ?\") (setq i (1+ i))
               (setq state 'normal))))
          ('dollar
           (when (and (= c ?$)
                      (<= (+ i (length dollar)) length)
                      (eq t (compare-strings
                             dollar nil nil sql i (+ i (length dollar)))))
             (setq i (+ i (length dollar) -1) state 'normal)))
          ('line-comment (when (memq c '(?\n ?\r)) (setq state 'normal)))
          ('block-comment
           (cond ((and (= c ?/) (eq next ?*))
                  (setq depth (1+ depth) i (1+ i)))
                 ((and (= c ?*) (eq next ?/))
                  (setq depth (1- depth) i (1+ i))
                  (when (= depth 0) (setq state 'normal))))))
        (setq i (1+ i))))
    (and valid (memq state '(normal line-comment)))))

(defun sql-grid-db--postgres-client-command-p (sql)
  "Return non-nil when SQL begins with a psql meta-command."
  (let ((i 0) (length (length sql)) found)
    (while (and (< i length) (not found))
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r ?\f)) (setq i (1+ i)))
         ((and (= c ?-) (eq next ?-))
          (setq i (or (string-match "[\n\r]" sql (+ i 2)) length)))
         ((and (= c ?/) (eq next ?*))
          (let ((end (string-match "\\*/" sql (+ i 2))))
            (setq i (if end (+ end 2) length))))
         ((= c ?\\) (setq found t))
         (t (setq i length)))))
    found))

(defun sql-grid-db--validate-postgres-sql (sql)
  "Reject SQL unless it is one safe PostgreSQL server statement."
  (unless (sql-grid-db--postgres-single-statement-p sql)
    (user-error "Only one SQL statement may be executed at a time"))
  (when (sql-grid-db--postgres-client-command-p sql)
    (user-error "PostgreSQL client commands are not accepted as SQL")))


(defun sql-grid-db--postgres-csv-records (text)
  "Parse psql CSV TEXT into records of (VALUE . QUOTED) fields."
  (let ((i 0) (length (length text)) (quoted nil)
        (field-quoted nil) (at-start t) fields records pieces)
    (while (< i length)
      (let ((c (aref text i))
            (next (and (< (1+ i) length) (aref text (1+ i)))))
        (cond
         ((and quoted (= c ?\") (eq next ?\"))
          (push "\"" pieces)
          (setq i (1+ i)))
         ((and quoted (= c ?\")) (setq quoted nil))
         ((and (not quoted) at-start (= c ?\"))
          (setq quoted t field-quoted t at-start nil))
         ((and (not quoted) (= c ?,))
          (push (cons (apply #'concat (nreverse pieces)) field-quoted)
                fields)
          (setq pieces nil field-quoted nil at-start t))
         ((and (not quoted) (memq c '(?\n ?\r)))
          (when (and (= c ?\r) (eq next ?\n)) (setq i (1+ i)))
          (push (cons (apply #'concat (nreverse pieces)) field-quoted)
                fields)
          (push (nreverse fields) records)
          (setq pieces nil fields nil field-quoted nil at-start t))
         (t (push (char-to-string c) pieces)
            (setq at-start nil))))
      (setq i (1+ i)))
    (when (or pieces fields (not at-start))
      (push (cons (apply #'concat (nreverse pieces)) field-quoted) fields)
      (push (nreverse fields) records))
    (nreverse records)))


(defun sql-grid-db--postgres-with-verb (sql)
  "Return main statement verb after a PostgreSQL WITH clause."
  (let ((i 0) (length (length sql)) (depth 0) (state 'normal)
        (block-depth 0) dollar verb done)
    (while (and (< i length) (not done))
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (pcase state
          ('normal
           (cond
            ((memq c '(?\s ?\t ?\n ?\r ?\f)))
            ((and (= c ?-) (eq next ?-))
             (setq state 'line-comment i (1+ i)))
            ((and (= c ?/) (eq next ?*))
             (setq state 'block-comment block-depth 1 i (1+ i)))
            ((and (= c ?$)
                  (or (= i 0)
                      (not (string-match-p
                            "\\`[[:alnum:]_$]\\'"
                            (char-to-string (aref sql (1- i))))))
                  (let ((start (string-match
                                "\\$\\(?:[[:alpha:]_][[:alnum:]_]*\\)?\\$"
                                sql i)))
                    (and start (= start i))))
             (setq dollar (match-string 0 sql)
                   state 'dollar i (+ i (length dollar) -1)))
            ((= c ?')
             (if (and (> i 0) (memq (aref sql (1- i)) '(?e ?E))
                      (or (= i 1)
                          (not (string-match-p
                                "\\`[[:alnum:]_$]\\'"
                                (char-to-string (aref sql (- i 2)))))))
                 (setq state 'escape-single)
               (setq state 'single)))
            ((= c ?\") (setq state 'double))
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
                                      "merge" "values"))
                   (setq verb word done t)))
               (setq i (1- end))))))
          ('single
           (when (= c ?')
             (if (eq next ?') (setq i (1+ i))
               (setq state 'normal))))
          ('escape-single
           (cond ((= c ?\\) (setq i (1+ i)))
                 ((= c ?') (if (eq next ?') (setq i (1+ i))
                             (setq state 'normal)))))
          ('double
           (when (= c ?\")
             (if (eq next ?\") (setq i (1+ i))
               (setq state 'normal))))
          ('dollar
           (when (and (= c ?$)
                      (<= (+ i (length dollar)) length)
                      (eq t (compare-strings
                             dollar nil nil sql i (+ i (length dollar)))))
             (setq i (+ i (length dollar) -1) state 'normal)))
          ('line-comment
           (when (memq c '(?\n ?\r)) (setq state 'normal)))
          ('block-comment
           (cond ((and (= c ?/) (eq next ?*))
                  (setq block-depth (1+ block-depth) i (1+ i)))
                 ((and (= c ?*) (eq next ?/))
                  (setq block-depth (1- block-depth) i (1+ i))
                  (when (= block-depth 0) (setq state 'normal))))))
        (setq i (1+ i))))
    verb))
(defun sql-grid-db--postgres-results (text sql null-marker count-marker)
  "Decode psql CSV TEXT for SQL with NULL-MARKER and its row-count trailer."
  (let* ((records (sql-grid-db--postgres-csv-records text))
         (trailer (car (last records)))
         (count-text
          (and (= (length trailer) 1)
               (not (cdr (car trailer)))
               (string-match
                (concat "\\`" (regexp-quote count-marker)
                        " \\([0-9]+\\)\\'")
                (caar trailer))
               (match-string 1 (caar trailer))))
         (affected (and count-text (string-to-number count-text)))
         (data (and count-text (butlast records)))
         (keyword (sql-grid-db--first-keyword sql))
         (verb (if (equal keyword "with")
                   (sql-grid-db--postgres-with-verb sql)
                 keyword))
         (write-p (member verb '("insert" "update" "delete" "merge"))))
    (unless count-text
      (error "Invalid PostgreSQL result trailer"))
    (let* ((columns (and data (mapcar #'car (car data))))
           (rows
            (mapcar
             (lambda (record)
               (unless (= (length record) (length columns))
                 (error "Invalid PostgreSQL CSV row width"))
               (mapcar (lambda (field)
                         (if (and (not (cdr field))
                                  (equal (car field) null-marker))
                             nil (car field)))
                       record))
             (cdr data))))
      (list (sql-grid-result-create
             :columns columns :rows rows
             :affected-rows (and write-p affected))))))

(defun sql-grid-db--postgres-markers ()
  "Return distinct per-execution NULL and row-count markers."
  (let ((nonce (secure-hash 'sha256
                            (format "%s:%s:%s" (float-time)
                                    (random) (emacs-pid)))))
    (list (concat "sql-grid-null-" nonce)
          (concat "sql-grid-count-" nonce))))

(defun sql-grid-db--postgres-conninfo-escape (value)
  "Escape VALUE inside a quoted libpq connection-string value."
  (when (string-match-p "[\0\n\r]" value)
    (user-error "Connection setting contains an unsupported control character"))
  (replace-regexp-in-string "[\\\\']" "\\\\\\&" value t))

(defun sql-grid-db--postgres-args (connection sql null-marker count-marker)
  "Return psql arguments for CONNECTION and SQL."
  (let ((database (sql-grid-connection-database connection)))
    (append
     (list "--no-psqlrc" "--no-password" "--quiet" "--csv"
           "--pset=pager=off"
           (concat "--pset=null=" null-marker)
           "--set=ON_ERROR_STOP=1")
     (when (sql-grid-connection-host connection)
       (list (concat "--host=" (sql-grid-connection-host connection))))
     (when (sql-grid-connection-port connection)
       (list (format "--port=%s" (sql-grid-connection-port connection))))
     (when (sql-grid-connection-user connection)
       (list (concat "--username=" (sql-grid-connection-user connection))))
     (when database
       (list (concat "--dbname=dbname='"
                     (sql-grid-db--postgres-conninfo-escape database)
                     "'")))
     (list (concat "--command=" sql)
           (concat "--command=\\echo " count-marker " :ROW_COUNT")))))

(defun sql-grid-db--postgres-execute (connection sql)
  "Execute one PostgreSQL statement on CONNECTION."
  (sql-grid-db--validate-postgres-sql sql)
  (let* ((started (float-time))
         (markers (sql-grid-db--postgres-markers))
         (null-marker (car markers))
         (count-marker (cadr markers))
         (password-file (sql-grid-db--postgres-password-file connection))
         (stderr-file (make-temp-file "sql-grid-pg-stderr-"))
         (output-buffer (generate-new-buffer " *sql-grid-postgres-output*"))
         (status nil) output error-text)
    (unwind-protect
        (let ((process-environment (copy-sequence process-environment)))
          (dolist (variable '("PGPASSWORD" "PGHOST" "PGPORT" "PGUSER"
                              "PGDATABASE"))
            (setenv variable nil))
          (setenv "PGPASSFILE" password-file)
          (setenv "PGCLIENTENCODING" "UTF8")
          (let ((coding-system-for-read 'utf-8-unix)
                (coding-system-for-write 'utf-8-unix))
            (setq status
                  (apply #'call-process sql-grid-postgres-program nil
                         (list output-buffer stderr-file) nil
                         (sql-grid-db--postgres-args
                          connection sql null-marker count-marker))))
          (setq output (with-current-buffer output-buffer (buffer-string))
                error-text
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8-unix))
                    (insert-file-contents stderr-file))
                  (buffer-string)))
          (unless (and (integerp status) (= status 0))
            (error "postgres client failed%s%s"
                   (if (string-empty-p error-text) "" ": ") error-text))
          (let ((results (sql-grid-db--postgres-results
                          output sql null-marker count-marker)))
            (dolist (result results)
              (setf (sql-grid-result-elapsed result)
                    (- (float-time) started)))
            results))
      (when (file-exists-p password-file) (delete-file password-file))
      (when (file-exists-p stderr-file) (delete-file stderr-file))
      (when (buffer-live-p output-buffer) (kill-buffer output-buffer)))))

(defun sql-grid-db--postgres-async-sentinel (process _event)
  "Deliver asynchronous PostgreSQL PROCESS results."
  (when (memq (process-status process) '(exit signal))
    (let ((job (process-get process 'sql-grid-job)))
      (when job
        (let ((status (process-exit-status process))
              (output (with-current-buffer (process-buffer process)
                        (buffer-string)))
              (stderr (with-current-buffer
                          (process-get process 'sql-grid-stderr)
                        (buffer-string)))
              (markers (process-get process 'sql-grid-postgres-markers)))
          (if (and (= status 0) (not (sql-grid-job-cancelled job)))
              (condition-case err
                  (let ((results (sql-grid-db--postgres-results
                                  output (sql-grid-job-sql job)
                                  (car markers) (cadr markers))))
                    (dolist (result results)
                      (setf (sql-grid-result-elapsed result)
                            (- (float-time) (sql-grid-job-started job))))
                    (sql-grid-db--async-finish job results nil))
                (error (sql-grid-db--async-finish job nil err)))
            (sql-grid-db--async-finish
             job nil
             (if (sql-grid-job-cancelled job)
                 '(quit "Query cancelled")
               (list 'error
                     (format "postgres client failed%s%s"
                             (if (string-empty-p stderr) "" ": ") stderr))))))))))

(defun sql-grid-db--postgres-execute-async (connection sql callback)
  "Execute PostgreSQL SQL asynchronously and call CALLBACK."
  (sql-grid-db--validate-postgres-sql sql)
  (let* ((markers (sql-grid-db--postgres-markers))
         (password-file (sql-grid-db--postgres-password-file connection))
         (output (generate-new-buffer " *sql-grid-postgres-async-output*"))
         (stderr (generate-new-buffer " *sql-grid-postgres-async-stderr*"))
         (job (sql-grid-job-create
               :connection connection :callback callback
               :buffers (list output stderr) :defaults password-file :sql sql
               :started (float-time)))
         process)
    (condition-case err
        (let ((process-environment (copy-sequence process-environment)))
          (dolist (variable '("PGPASSWORD" "PGHOST" "PGPORT" "PGUSER"
                              "PGDATABASE"))
            (setenv variable nil))
          (setenv "PGPASSFILE" password-file)
          (setenv "PGCLIENTENCODING" "UTF8")
          (setq process
                (make-process
                 :name "sql-grid-postgres-query" :buffer output
                 :stderr stderr
                 :command
                 (cons sql-grid-postgres-program
                       (sql-grid-db--postgres-args
                        connection sql (car markers) (cadr markers)))
                 :connection-type 'pipe :coding 'utf-8-unix
                 :sentinel #'sql-grid-db--postgres-async-sentinel))
          (process-put process 'sql-grid-job job)
          (process-put process 'sql-grid-stderr stderr)
          (process-put process 'sql-grid-postgres-markers markers)
          (setf (sql-grid-job-process job) process)
          (push job (sql-grid-connection-jobs connection))
          job)
      (error
       (setf (sql-grid-connection-jobs connection)
             (delq job (sql-grid-connection-jobs connection)))
       (sql-grid-db--async-cleanup job)
       (signal (car err) (cdr err))))))
(defun sql-grid-db-postgres--enqueue-schema (connection enqueue add-table)
  "Queue normalized PostgreSQL metadata queries for CONNECTION."
  (ignore connection)
  (funcall
   enqueue
   "SELECT table_schema, table_name, CASE table_type WHEN 'BASE TABLE' THEN 'table' ELSE 'view' END FROM information_schema.tables WHERE table_schema NOT IN ('information_schema') AND table_schema NOT LIKE 'pg\\_%' ESCAPE '\\' AND table_type IN ('BASE TABLE','VIEW') ORDER BY table_schema, table_name"
   (lambda (tables)
     (dolist (row tables)
       (let* ((schema (nth 0 row))
              (name (nth 1 row))
              (entry (list :schema schema :name name
                           :kind (if (member (nth 2 row) '("table" "BASE TABLE"))
                                     "table" "view")
                           :columns nil :indexes nil :foreign-keys nil))
              (schema-lit (sql-grid-db--quote-literal schema))
              (name-lit (sql-grid-db--quote-literal name)))
         (funcall add-table entry)
         (funcall
          enqueue
          (format "SELECT c.column_name, pg_catalog.format_type(a.atttypid, a.atttypmod), c.is_nullable, c.column_default, CASE WHEN pk.column_name IS NULL THEN 'NO' ELSE 'YES' END FROM information_schema.columns c JOIN pg_namespace ns ON ns.nspname = c.table_schema JOIN pg_class cl ON cl.relnamespace = ns.oid AND cl.relname = c.table_name JOIN pg_attribute a ON a.attrelid = cl.oid AND a.attname = c.column_name LEFT JOIN (SELECT kcu.column_name FROM information_schema.table_constraints tc JOIN information_schema.key_column_usage kcu USING (constraint_catalog, constraint_schema, constraint_name, table_schema, table_name) WHERE tc.constraint_type = 'PRIMARY KEY' AND tc.table_schema = %s AND tc.table_name = %s) pk USING (column_name) WHERE c.table_schema = %s AND c.table_name = %s ORDER BY c.ordinal_position"
                  schema-lit name-lit schema-lit name-lit)
          (lambda (rows)
            (plist-put
             entry :columns
             (mapcar (lambda (column)
                       (list :name (nth 0 column) :type (or (nth 1 column) "")
                             :nullable (equal (nth 2 column) "YES")
                             :primary-key (equal (nth 4 column) "YES")
                             :default (nth 3 column) :hidden nil :key nil :extra nil))
                     rows))))
         (funcall
          enqueue
          (format "SELECT ci.relname, i.indisunique::text, pg_get_indexdef(i.indexrelid, keys.ordinality::integer, true), CASE WHEN keys.ordinality <= i.indnkeyatts THEN 'key' ELSE 'include' END, pg_get_expr(i.indpred, i.indrelid) FROM pg_index i JOIN pg_class ct ON ct.oid = i.indrelid JOIN pg_namespace ns ON ns.oid = ct.relnamespace JOIN pg_class ci ON ci.oid = i.indexrelid CROSS JOIN LATERAL unnest(i.indkey) WITH ORDINALITY AS keys(attnum, ordinality) WHERE ns.nspname = %s AND ct.relname = %s AND keys.ordinality <= i.indnatts ORDER BY ci.relname, keys.ordinality"
                  schema-lit name-lit)
          (lambda (rows)
            (let ((groups (make-hash-table :test #'equal)) order)
              (dolist (index-row rows)
                (let* ((index-name (nth 0 index-row))
                       (group (gethash index-name groups)))
                  (unless group
                    (setq group (list :name index-name
                                      :unique (equal (nth 1 index-row) "true")
                                      :columns nil :include nil
                                      :predicate (nth 4 index-row)
                                      :partial (not (string-empty-p (or (nth 4 index-row) ""))))
                          order (append order (list index-name)))
                    (puthash index-name group groups))
                  (let ((key (if (equal (nth 3 index-row) "include")
                                 :include :columns)))
                    (plist-put group key
                               (append (plist-get group key)
                                       (list (nth 2 index-row)))))))
              (plist-put entry :indexes (mapcar (lambda (key) (gethash key groups)) order))))
         )
         (funcall
          enqueue
          (format "SELECT con.conname, keys.ordinality, sa.attname, tn.nspname, tgt.relname, ta.attname FROM pg_constraint con JOIN pg_class src ON src.oid = con.conrelid JOIN pg_namespace sn ON sn.oid = src.relnamespace JOIN pg_class tgt ON tgt.oid = con.confrelid JOIN pg_namespace tn ON tn.oid = tgt.relnamespace CROSS JOIN LATERAL unnest(con.conkey) WITH ORDINALITY AS keys(attnum, ordinality) JOIN LATERAL unnest(con.confkey) WITH ORDINALITY AS refs(attnum, ordinality) USING (ordinality) JOIN pg_attribute sa ON sa.attrelid = src.oid AND sa.attnum = keys.attnum JOIN pg_attribute ta ON ta.attrelid = tgt.oid AND ta.attnum = refs.attnum WHERE con.contype = 'f' AND sn.nspname = %s AND src.relname = %s ORDER BY con.conname, keys.ordinality"
                  schema-lit name-lit)
          (lambda (rows)
            (plist-put
             entry :foreign-keys
             (mapcar (lambda (fk)
                       (list :name (nth 0 fk)
                             :sequence (string-to-number (format "%s" (nth 1 fk)))
                             :column (nth 2 fk) :target-schema (nth 3 fk)
                             :target-table (nth 4 fk) :target-column (nth 5 fk)))
                     rows)))))))))
(provide 'sql-grid-db-postgres)
;;; sql-grid-db-postgres.el ends here

