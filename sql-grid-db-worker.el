;;; sql-grid-db-worker.el --- Persistent SQLite worker for sql-grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'sql-grid-db-sqlite)

(defvar sql-grid-db-worker--handle nil)
(defvar sql-grid-db-worker--token nil)
(defvar sql-grid-db-worker--client nil)
(defvar sql-grid-db-worker--alive t)

(defun sql-grid-db-worker--write (process object)
  "Send OBJECT as one UTF-8 JSON line to PROCESS."
  (process-send-string process (concat (json-serialize object) "\n")))

(defun sql-grid-db-worker--result-object (result)
  "Convert RESULT to a JSON-encodable alist."
  `((columns . ,(vconcat
                 (mapcar #'sql-grid-db--encode-wire-value
                         (sql-grid-result-columns result))))
    (rows . ,(vconcat
              (mapcar
               (lambda (row)
                 (vconcat (mapcar #'sql-grid-db--encode-wire-value row)))
               (sql-grid-result-rows result))))
    (message . ,(or (sql-grid-result-message result) :null))
    (elapsed . ,(sql-grid-result-elapsed result))
    (affected-rows . ,(or (sql-grid-result-affected-rows result) :null))))

(defun sql-grid-db-worker--reject (process)
  "Close unauthenticated or duplicate client PROCESS only."
  (delete-process process))

(defun sql-grid-db-worker--request (process request)
  "Handle REQUEST received from PROCESS."
  (let ((id (alist-get 'id request))
        (token (alist-get 'token request))
        (operation (alist-get 'operation request)))
    (cond
     ((not (equal token sql-grid-db-worker--token))
      (sql-grid-db-worker--reject process))
     ((and sql-grid-db-worker--client
           (not (eq process sql-grid-db-worker--client)))
      (sql-grid-db-worker--reject process))
     (t
      (setq sql-grid-db-worker--client process)
      (cond
       ((equal operation "close")
        (when sql-grid-db-worker--handle
          (sqlite-close sql-grid-db-worker--handle))
        (setq sql-grid-db-worker--handle nil
              sql-grid-db-worker--alive nil)
        (sql-grid-db-worker--write process `((id . ,id) (ok . t))))
       ((equal operation "execute")
        (condition-case err
            (progn
              (unless sql-grid-db-worker--handle
                (error "SQLite worker has no database"))
              (let ((results
                     (sql-grid-db--sqlite-results
                      sql-grid-db-worker--handle
                      (alist-get 'sql request))))
                (sql-grid-db-worker--write
                 process `((id . ,id)
                           (results . ,(vconcat
                                        (mapcar
                                         #'sql-grid-db-worker--result-object
                                         results)))))))
          (error
           (sql-grid-db-worker--write
            process `((id . ,id)
                      (error . ,(vector
                                 (symbol-name (car err))
                                 (sql-grid-db--encode-wire-value
                                  (error-message-string err)))))))))
       (t
        (sql-grid-db-worker--write
         process `((id . ,id)
                   (error . ,(vector
                              "error"
                              (sql-grid-db--encode-wire-value
                               "Unsupported worker operation")))))))))))

(defun sql-grid-db-worker--filter (process chunk)
  "Handle incoming JSON lines from PROCESS."
  (let ((text (concat (or (process-get process 'sql-grid-input) "") chunk))
        newline)
    (while (setq newline (string-match "\n" text))
      (let ((line (substring text 0 newline)))
        (setq text (substring text (1+ newline)))
        (unless (string-empty-p line)
          (condition-case nil
              (sql-grid-db-worker--request
               process (json-parse-string line :object-type 'alist
                                          :array-type 'array
                                          :null-object nil
                                          :false-object :false))
            (error (sql-grid-db-worker--reject process))))))
    (process-put process 'sql-grid-input text)))

(defun sql-grid-db-worker--sentinel (process _event)
  "Stop the worker only when its authenticated client PROCESS closes."
  (when (and (eq process sql-grid-db-worker--client)
             (memq (process-status process) '(exit signal closed failed)))
    (setq sql-grid-db-worker--client nil
          sql-grid-db-worker--alive nil)))

(defun sql-grid-db-worker--log (_server client _message)
  "Install UTF-8 input handlers for accepted CLIENT."
  (set-process-coding-system client 'utf-8-unix 'utf-8-unix)
  (process-put client 'sql-grid-db-input "")
  (set-process-filter client #'sql-grid-db-worker--filter)
  (set-process-sentinel client #'sql-grid-db-worker--sentinel))

(defun sql-grid-db-worker-main ()
  "Open the persistent database and serve one authenticated local client.
The database path is the sole command-line argument.  The authentication
token is read from stdin so it never appears in the process command line."
  (let* ((args (if (equal (car command-line-args-left) "--")
                   (cdr command-line-args-left)
                 command-line-args-left))
         (database (car args))
         (token (string-trim (read-string "")))
         (server nil))
    (setq command-line-args-left nil)
    (unless (and database (not (string-empty-p token)))
      (error "Missing SQLite worker database or token"))
    (setq sql-grid-db-worker--token token
          sql-grid-db-worker--handle
          (sqlite-open (unless (equal database ":memory:") database)))
    (setq server
          (make-network-process :name "sql-grid-sqlite-worker"
                                :server t :host "127.0.0.1" :service t
                                :family 'ipv4 :noquery t
                                :coding 'utf-8-unix
                                :log #'sql-grid-db-worker--log))
    (princ (format "ready %s\n" (process-contact server :service)))
    (flush-standard-output)
    (while sql-grid-db-worker--alive
      (accept-process-output nil 0.5))
    (when (process-live-p server)
      (delete-process server))
    (when sql-grid-db-worker--handle
      (sqlite-close sql-grid-db-worker--handle))))

(sql-grid-db-worker-main)

;;; sql-grid-db-worker.el ends here
