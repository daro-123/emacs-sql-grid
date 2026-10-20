;;; sql-grid-db.el --- Database connections for sql-grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; SQLite uses Emacs's native SQLite interface and retains one connection
;; (and therefore transaction state) until disconnected.  MySQL, MariaDB,
;; and PostgreSQL use one command-line client process per execute call; their
;; session state, including temporary tables and uncommitted transactions,
;; does not persist between calls.

;;; Code:

(require 'sql-grid-db-core)
(require 'sql-grid-db-sqlite)
(require 'sql-grid-db-mysql)
(require 'sql-grid-db-postgres)
 


(defun sql-grid-db--enqueue-schema (connection enqueue add-table)
  "Dispatch CONNECTION discovery to its backend adapter.
ENQUEUE accepts (SQL CONSUME); CONSUME receives first-result ROWS.
ADD-TABLE receives a plist with :schema, :name, :kind, :columns,
:indexes, and :foreign-keys. Columns use :name, :type, :nullable,
:primary-key, :default, :hidden, :key, and :extra. Indexes use :name,
:unique, :columns, :include, :predicate, and :partial. Foreign-key parts
use :name, integer :sequence, :column, :target-schema, :target-table,
and :target-column. Drivers enqueue and mutate metadata through callbacks;
the coordinator owns SQL execution, cancellation, and caching."
  (pcase (sql-grid-connection-backend connection)
    ('sqlite
     (sql-grid-db-sqlite--enqueue-schema connection enqueue add-table))
    ((or 'mysql 'mariadb)
     (sql-grid-db-mysql--enqueue-schema connection enqueue add-table))
    ('postgres
     (sql-grid-db-postgres--enqueue-schema connection enqueue add-table))
    (_ (error "Unsupported database backend: %S"
              (sql-grid-connection-backend connection)))))






(defun sql-grid-db-execute-async (connection sql callback)
  "Execute SQL asynchronously and call CALLBACK with results and error.
Return a job that can be cancelled with `sql-grid-db-cancel'."
  (sql-grid-db--ensure-open connection)
  (unless (functionp callback)
    (error "Callback must be a function"))
  (pcase (sql-grid-connection-backend connection)
    ('sqlite
     (unless (sql-grid-db--single-statement-p sql 'sqlite)
       (user-error "Only one SQL statement may be executed at a time"))
     (sql-grid-db--sqlite-request connection sql callback))
    ((or 'mysql 'mariadb)
     (unless (sql-grid-db--single-statement-p
              sql (sql-grid-connection-backend connection))
       (user-error "Only one SQL statement may be executed at a time"))
     (sql-grid-db--server-execute-async connection sql callback))
    ('postgres (sql-grid-db--postgres-execute-async connection sql callback))
    (_ (error "Unsupported database backend: %S"
              (sql-grid-connection-backend connection)))))



(defun sql-grid-db-cancel (job)
  "Cancel JOB and invoke its callback once with a quit condition.
Emacs's SQLite interface has no interrupt function.  Cancelling a
SQLite job therefore disconnects its worker and invalidates the
connection; an uncommitted transaction rolls back when SQLite closes
the worker's database connection.  No reconnect is attempted."
  (unless (sql-grid-job-completed job)
    (setf (sql-grid-job-cancelled job) t)
    (let* ((connection (sql-grid-job-connection job))
           (sqlite-p (eq (sql-grid-connection-backend connection) 'sqlite))
           (closing-owner (and sqlite-p
                               (not (sql-grid-connection-closing connection))))
           callback-errors)
      (when closing-owner
        (setf (sql-grid-connection-closing connection) t))
      (if sqlite-p
          (unwind-protect
              (let (others)
                (dolist (item (copy-sequence
                               (sql-grid-connection-jobs connection)))
                  (when (and (sql-grid-job-p item) (not (eq item job)))
                    (push item others)))
                (sql-grid-db--sqlite-teardown connection)
                (let ((err (sql-grid-db--async-finish
                            job nil '(quit "Query cancelled") t)))
                  (when err (push err callback-errors)))
                (dolist (other others)
                  (unless (sql-grid-job-completed other)
                    (let ((err (sql-grid-db--async-finish
                                other nil
                                '(error "SQLite connection cancelled") t)))
                      (when err (push err callback-errors))))))
            (when closing-owner
              (setf (sql-grid-connection-closing connection) nil)))
        (let ((process (sql-grid-job-process job)))
          (when process (process-put process 'sql-grid-job nil))
          (when (and process (process-live-p process))
            (delete-process process))
          (let ((err (sql-grid-db--async-finish
                      job nil '(quit "Query cancelled") t)))
            (when err (push err callback-errors)))))
      (when callback-errors
        (signal (caar callback-errors) (cdar callback-errors)))))
  nil)


(defun sql-grid-db-connect (connection)
  "Open CONNECTION and return it."
  (when (sql-grid-connection-closing connection)
    (user-error "Database connection is closed while disconnecting"))
  (when (and (sql-grid-connection-closed connection)
             (sql-grid-connection-jobs connection))
    (error "Cannot reconnect while database jobs remain"))
  (let ((result
         (pcase (sql-grid-connection-backend connection)
           ('sqlite
            (unless (sqlite-available-p)
              (error "This Emacs has no SQLite support"))
            (sql-grid-db--sqlite-worker-connect connection))
           ((or 'mysql 'mariadb)
            (let ((program
                   (if (eq (sql-grid-connection-backend connection) 'mysql)
                       sql-grid-mysql-program sql-grid-mariadb-program)))
              (unless (executable-find program)
                (error "Database client executable not found: %s" program))
              connection))
           ('postgres
            (unless (executable-find sql-grid-postgres-program)
              (error "Database client executable not found: %s"
                     sql-grid-postgres-program))
            connection)
           (_ (error "Unsupported database backend: %S"
                     (sql-grid-connection-backend connection))))))
    (setf (sql-grid-connection-closed connection) nil)
    result))

(defun sql-grid-db--ensure-open (connection)
  "Reject execution on closing or closed CONNECTION."
  (when (or (sql-grid-connection-closed connection)
            (sql-grid-connection-closing connection))
    (user-error "Database connection is closed")))

(defun sql-grid-db-execute (connection sql)
  "Execute SQL on CONNECTION and return all rows in result structs."
  (sql-grid-db--ensure-open connection)
  (pcase (sql-grid-connection-backend connection)
    ('sqlite
     (unless (sql-grid-connection-handle connection)
       (error "SQLite connection is not open"))
     (sql-grid-db--sqlite-request connection sql))
    ((or 'mysql 'mariadb) (sql-grid-db--server-execute connection sql))
    ('postgres (sql-grid-db--postgres-execute connection sql))
    (_ (error "Unsupported database backend: %S"
              (sql-grid-connection-backend connection)))))

(defun sql-grid-db-disconnect (connection)
  "Close CONNECTION's resources."
  (unless (sql-grid-connection-closing connection)
    (setf (sql-grid-connection-closing connection) t
          (sql-grid-connection-closed connection) t)
    (unwind-protect
        (let ((jobs (copy-sequence (sql-grid-connection-jobs connection)))
              callback-errors)
          (when (eq (sql-grid-connection-backend connection) 'sqlite)
            (sql-grid-db--sqlite-teardown connection))
          (dolist (job jobs)
            (unless (sql-grid-job-completed job)
              (when (sql-grid-job-process job)
                (process-put (sql-grid-job-process job) 'sql-grid-job nil))
              (let ((err (sql-grid-db--async-finish
                          job nil '(quit "Connection closed") t)))
                (when err (push err callback-errors)))))
          (when callback-errors
            (signal (caar callback-errors) (cdar callback-errors))))
      (setf (sql-grid-connection-closing connection) nil)))
  nil)

(provide 'sql-grid-db)
;;; sql-grid-db.el ends here
