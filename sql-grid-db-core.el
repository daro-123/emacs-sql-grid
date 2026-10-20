;;; sql-grid-db-core.el --- Shared sql-grid database types -*- lexical-binding: t; -*-

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
(defgroup sql-grid-db nil
  "Database client settings for sql-grid."
  :group 'tools)

(cl-defstruct (sql-grid-connection
               (:constructor sql-grid-connection-create))
  backend database host port user password handle process worker-buffer
  pending next-id jobs worker-token on-close closed closing)

(cl-defstruct (sql-grid-result
               (:constructor sql-grid-result-create))
  columns rows message elapsed affected-rows)

(cl-defstruct (sql-grid-job (:constructor sql-grid-job-create))
  connection callback process buffers defaults stderr-buffer sql id
  completed cancelled started)

(defun sql-grid-db--quote-sqlite-id (name)
  "Quote SQLite identifier NAME."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" name t t) "\""))

(defun sql-grid-db--quote-mysql-id (name)
  "Quote MySQL identifier NAME."
  (concat "`" (replace-regexp-in-string "`" "``" name t t) "`"))

(defun sql-grid-db--quote-literal (value)
  "Quote SQL string literal VALUE."
  (concat "'" (replace-regexp-in-string "'" "''" value t t) "'"))
(defun sql-grid-db--single-statement-p (sql &optional backend)
  "Return non-nil if SQL contains at most one statement.
BACKEND controls backslash escapes in quoted values.  Semicolons inside
quoted strings, identifiers, and comments are ignored.  A semicolon
followed only by whitespace/comments is permitted."
  (let ((i 0) (length (length sql)) (state 'normal)
        (server-p (memq backend '(mysql mariadb)))
        (terminated nil) (valid t))
    (while (and (< i length) valid)
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (pcase state
          ('normal
           (cond
            ((memq c '(?\s ?\t ?\n ?\r ?\f)))
            ((and (= c ?-) (eq next ?-)
                  (or (not server-p) (>= (+ i 2) length)
                      (memq (aref sql (+ i 2))
                            '(?\s ?\t ?\n ?\r ?\f ?\v))))
             (setq state 'line-comment i (1+ i)))
            ((and server-p (= c ?#)) (setq state 'line-comment))
            ((and (= c ?/) (eq next ?*))
             (setq state 'block-comment i (1+ i)))
            (terminated (setq valid nil))
            ((= c ?') (setq state 'single))
            ((= c ?\") (setq state 'double))
            ((and (eq backend 'sqlite) (= c ?\[)) (setq state 'bracket))
            ((= c ?`) (setq state 'backtick))
            ((= c ?\;) (setq terminated t))))
          ('single
           (cond ((and server-p (= c ?\\)) (setq i (1+ i)))
                 ((= c ?') (if (eq next ?') (setq i (1+ i))
                             (setq state 'normal)))))
          ('double
           (cond ((and server-p (= c ?\\)) (setq i (1+ i)))
                 ((= c ?\") (if (eq next ?\") (setq i (1+ i))
                              (setq state 'normal)))))
          ('backtick
           (when (= c ?`) (if (eq next ?`) (setq i (1+ i))
                            (setq state 'normal))))
          ('bracket (when (= c ?\]) (setq state 'normal)))
          ('line-comment (when (memq c '(?\n ?\r)) (setq state 'normal)))
          ('block-comment
           (when (and (= c ?*) (eq next ?/))
             (setq state 'normal i (1+ i)))))
        (setq i (1+ i))))
    valid))
(defun sql-grid-db--contains-keyword-p (sql keyword)
  "Return non-nil when KEYWORD occurs in SQL outside quoted text/comments."
  (let ((i 0) (length (length sql)) (state 'normal) found)
    (while (and (< i length) (not found))
      (let ((c (aref sql i))
            (next (and (< (1+ i) length) (aref sql (1+ i)))))
        (cond
         ((eq state 'normal)
          (cond
           ((and (= c ?-) (eq next ?-))
            (setq state 'line-comment i (1+ i)))
           ((and (= c ?/) (eq next ?*))
            (setq state 'block-comment i (1+ i)))
           ((= c ?') (setq state 'single))
           ((= c ?\") (setq state 'double))
           ((= c ?`) (setq state 'backtick))
           ((= c ?\[) (setq state 'bracket))
           ((and (or (<= ?a (downcase c) ?z) (= c ?_))
                 (not (and (> i 0)
                           (let ((previous (aref sql (1- i))))
                             (or (<= ?a (downcase previous) ?z)
                                 (<= ?0 previous ?9)
                                 (= previous ?_)))))
            (let ((end i))
              (while (and (< end length)
                          (let ((char (aref sql end)))
                            (or (<= ?a (downcase char) ?z)
                                (<= ?0 char ?9)
                                (= char ?_))))
                (setq end (1+ end)))
              (when (equal (downcase (substring sql i end))
                           (downcase keyword))
                      (setq found t))
              (setq i (1- end)))))))
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
    found))
(defun sql-grid-db--first-keyword (sql)
  "Return the first SQL keyword in SQL, skipping leading comments."
  (let ((i 0) (length (length sql)) keyword searching)
    (setq searching t)
    (while (and searching (< i length))
      (cond
       ((memq (aref sql i) '(?\s ?\t ?\n ?\r ?\f))
        (setq i (1+ i)))
       ((and (= (aref sql i) ?-) (< (1+ i) length)
             (= (aref sql (1+ i)) ?-))
        (setq i (+ i 2))
        (while (and (< i length) (not (memq (aref sql i) '(?\n ?\r))))
          (setq i (1+ i)))
        (setq i (min length (1+ i))))
       ((and (= (aref sql i) ?/) (< (1+ i) length)
             (= (aref sql (1+ i)) ?*))
        (let ((end (string-match "\\*/" sql (+ i 2))))
          (setq i (if end (+ end 2) length))))
       (t
        (if (string-match "\\`[[:alpha:]_][[:alnum:]_]*"
                          (substring sql i))
            (progn
              (setq keyword (downcase (match-string 0 (substring sql i))))
              (setq searching nil))
          (setq i (1+ i))))))
    keyword))
(defun sql-grid-db--async-cleanup (job)
  "Release process resources owned by JOB."
  (when (and (sql-grid-job-defaults job)
             (file-exists-p (sql-grid-job-defaults job)))
    (ignore-errors (delete-file (sql-grid-job-defaults job))))
  (dolist (buffer (sql-grid-job-buffers job))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((process (get-buffer-process (current-buffer))))
          (when (and process (process-live-p process))
            (ignore-errors (delete-process process)))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))
(defun sql-grid-db--async-finish (job results error-data &optional defer-error)
  "Complete JOB once with RESULTS or ERROR-DATA.
DEFER-ERROR returns callback errors for handling after outer cleanup."
  (unless (sql-grid-job-completed job)
    (setf (sql-grid-job-completed job) t)
    (let ((connection (sql-grid-job-connection job)))
      (setf (sql-grid-connection-jobs connection)
            (delq job (sql-grid-connection-jobs connection))))
    (sql-grid-db--async-cleanup job)
    (condition-case err
        (progn
          (funcall (sql-grid-job-callback job) results error-data)
          nil)
      ((error quit)
       (if defer-error
           err
         (signal (car err) (cdr err)))))))


(provide 'sql-grid-db-core)
;;; sql-grid-db-core.el ends here
