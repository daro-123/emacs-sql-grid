;;; sql-grid-schema.el --- Browse connected database schema -*- lexical-binding: t; -*-

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

;; Asynchronous schema discovery, a read-only per-editor schema browser, and
;; cached table/column completion for sql-grid editor buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sql)
(require 'sql-grid-db)

(defvar sql-grid--connection)

(defvar-local sql-grid-schema--generation 0)
(defvar-local sql-grid-schema--cache nil)
(defvar-local sql-grid-schema--loaded nil)
(defvar-local sql-grid-schema--error nil)
(defvar-local sql-grid-schema--job nil)
(defvar-local sql-grid-schema--browser-source nil)

(defun sql-grid-schema--rows (results)
  "Return rows from first result in RESULTS, or nil."
  (and results (sql-grid-result-rows (car results))))


(defun sql-grid-schema--query (connection sql callback source)
  "Run metadata SQL asynchronously on CONNECTION, then call CALLBACK.
SOURCE owns the job slot used to notice cancellation."
  (let (job)
    (condition-case err
        (setq job
              (sql-grid-db-execute-async
               connection sql
               (lambda (results error-data)
                 (when (and (buffer-live-p source)
                            (eq job
                                (buffer-local-value
                                 'sql-grid-schema--job source)))
                   (with-current-buffer source
                     (setq sql-grid-schema--job nil)))
                 (funcall callback results error-data))))
      (error
       (funcall callback nil err)))
    (when (and job (buffer-live-p source))
      (with-current-buffer source
        (setq sql-grid-schema--job job)))
    job))


(defun sql-grid-schema--sort (schema)
  "Return SCHEMA sorted by schema then object name."
  (sort (copy-sequence schema)
        (lambda (a b)
          (let ((a-schema (or (plist-get a :schema) ""))
                (b-schema (or (plist-get b :schema) "")))
            (if (equal a-schema b-schema)
                (string< (or (plist-get a :name) "")
                         (or (plist-get b :name) ""))
              (string< a-schema b-schema))))))

(defun sql-grid-schema--discover (connection done)
  "Discover CONNECTION schema asynchronously, then call DONE with data/error."
  (let ((source (current-buffer))
        (generation sql-grid-schema--generation)
        (data nil)
        (queue-front nil)
        (queue-back nil)
        (failure nil))
    (cl-labels
        ((enqueue (sql consume)
           (push (cons sql consume) queue-back))
         (add-table (table)
           (push table data))
         (step ()
           (if (or failure
                   (not (and (buffer-live-p source)
                             (= generation
                                (buffer-local-value
                                 'sql-grid-schema--generation source)))))
               (funcall done nil (or failure '(quit "Schema discovery cancelled")))
             (if (and (null queue-front) (null queue-back))
                 (funcall done (nreverse data) nil)
               (unless queue-front
                 (setq queue-front (nreverse queue-back)
                       queue-back nil))
               (let ((item (pop queue-front)))
                 (sql-grid-schema--query
                  connection (car item)
                  (lambda (results error-data)
                    (if error-data
                        (setq failure error-data)
                      (condition-case err
                          (funcall (cdr item)
                                   (sql-grid-schema--rows results))
                        (error (setq failure err))))
                    (step))
                  source))))))
      (sql-grid-db--enqueue-schema connection #'enqueue #'add-table)
      (step))))

(defun sql-grid-schema-refresh (&optional source)
  "Refresh schema cache for SOURCE, defaulting to current editor buffer."
  (interactive)
  (let ((source (or source (current-buffer))))
    (unless (buffer-live-p source) (user-error "SQL source buffer is gone"))
    (with-current-buffer source
      (unless (and (boundp 'sql-grid--connection) sql-grid--connection)
        (user-error "SQL buffer is not connected"))
      (when (and (eq (sql-grid-connection-backend sql-grid--connection)
                     'sqlite)
                 (fboundp 'sql-grid-connection-handle)
                 (null (sql-grid-connection-handle sql-grid--connection)))
        (user-error "SQLite connection is no longer valid; reconnect"))
      (if (and sql-grid-schema--job
               (eq (sql-grid-connection-backend sql-grid--connection) 'sqlite))
          (message "Schema refresh already running")
        (cl-incf sql-grid-schema--generation)
        (let ((generation sql-grid-schema--generation)
              (connection sql-grid--connection))
          (when (and sql-grid-schema--job
                     (memq (sql-grid-connection-backend connection)
                           '(mysql mariadb postgres)))
            (ignore-errors (sql-grid-db-cancel sql-grid-schema--job)))
          (setq sql-grid-schema--error nil)
          (message "Discovering database schema…")
          (sql-grid-schema--discover
           connection
           (lambda (schema error-data)
             (when (and (buffer-live-p source)
                        (= generation
                           (buffer-local-value
                            'sql-grid-schema--generation source))
                        (eq connection
                            (buffer-local-value 'sql-grid--connection source)))
               (with-current-buffer source
                 (setq sql-grid-schema--job nil)
                 (if error-data
                     (progn
                       (when (and (eq (sql-grid-connection-backend connection)
                                      'sqlite)
                                  (fboundp 'sql-grid-connection-handle)
                                  (null (sql-grid-connection-handle connection)))
                         (setq sql-grid-schema--cache nil
                               sql-grid-schema--loaded nil))
                       (setq sql-grid-schema--error
                             (error-message-string error-data))
                       (message "Schema discovery failed: %s"
                                sql-grid-schema--error))
                   (setq sql-grid-schema--cache
                         (sql-grid-schema--sort schema)
                         sql-grid-schema--loaded t
                         sql-grid-schema--error nil)
                   (message "Schema cache updated (%d objects)" (length schema)))
                 (sql-grid-schema-update-browser))))))))))

(defun sql-grid-refresh-schema ()
  "Refresh cached schema for current SQL editor."
  (interactive)
  (sql-grid-schema-refresh))

(defun sql-grid-schema-clear ()
  "Cancel schema work and clear schema cache in current buffer."
  (interactive)
  (cl-incf sql-grid-schema--generation)
  (when (and sql-grid-schema--job
             (boundp 'sql-grid--connection)
             sql-grid--connection
             (memq (sql-grid-connection-backend sql-grid--connection)
                   '(mysql mariadb postgres)))
    (ignore-errors (sql-grid-db-cancel sql-grid-schema--job)))
  (setq sql-grid-schema--job nil sql-grid-schema--cache nil
        sql-grid-schema--loaded nil sql-grid-schema--error nil)
  (sql-grid-schema-update-browser))

(defun sql-grid-schema-setup ()
  "Enable cached schema completion in current SQL editor buffer."
  (add-hook 'completion-at-point-functions
            #'sql-grid-completion-at-point nil t))

(defvar sql-grid-schema--reserved-regexps (make-hash-table :test #'eq))

(defun sql-grid-schema--reserved-regexp (backend)
  "Return a whole-word regexp for BACKEND's reserved keywords."
  (or (gethash backend sql-grid-schema--reserved-regexps)
      (let* ((keywords
              (pcase backend
                ('sqlite sql-mode-sqlite-font-lock-keywords)
                ('postgres sql-mode-postgres-font-lock-keywords)
                ((or 'mysql 'mariadb) sql-mode-mysql-font-lock-keywords)
                (_ sql-mode-ansi-font-lock-keywords)))
             (patterns
              (delq nil
                    (mapcar (lambda (entry)
                              (and (eq (cdr-safe entry)
                                       'font-lock-keyword-face)
                                   (stringp (car-safe entry))
                                   (car entry)))
                            keywords)))
             (regexp
              (when patterns
                (concat "\\`\\(?:" (mapconcat #'identity patterns "\\|")
                        "\\)\\'"))))
        (puthash backend regexp sql-grid-schema--reserved-regexps)
        regexp)))

(defun sql-grid-schema--reserved-word-p (name backend)
  "Return non-nil when NAME is a reserved word for BACKEND."
  (let ((regexp (sql-grid-schema--reserved-regexp backend)))
    (and regexp
         (let ((case-fold-search t))
           (string-match-p regexp name)))))

(defun sql-grid-schema--identifier (name backend &optional force quote-char)
  "Quote completion NAME for BACKEND, honoring FORCE and QUOTE-CHAR."
  (if (and (not force)
           (or (not (eq backend 'postgres))
               (string= name (downcase name)))
           (string-match-p "\\`[[:alpha:]_][[:alnum:]_$]*\\'" name)
           (not (sql-grid-schema--reserved-word-p name backend)))
      name
    (pcase quote-char
      (?\[ (if (string-match-p "]" name)
               (sql-grid-db--quote-sqlite-id name)
             (concat "[" name "]")))
      (?\" (sql-grid-db--quote-sqlite-id name))
      (?` (sql-grid-db--quote-mysql-id name))
      (_ (if (memq backend '(mysql mariadb))
             (sql-grid-db--quote-mysql-id name)
           (sql-grid-db--quote-sqlite-id name))))))

(defun sql-grid-schema--display-identifier (name backend)
  "Render NAME safely for display as an identifier on BACKEND."
  (if (eq backend 'postgres)
      (sql-grid-db--quote-sqlite-id name)
    name))

(defun sql-grid-schema--word-char-p (char)
  "Return non-nil when CHAR is part of an unquoted SQL identifier."
  (and char
       (or (<= ?a (downcase char) ?z)
           (<= ?0 char ?9)
           (and (>= char 128) (eq (char-syntax char) ?w))
           (memq char '(?_ ?$)))))

(defun sql-grid-schema--scan ()
  "Scan from `point-min' to point for SQL lexical context.
Return (STATE QUOTE-CHAR QUOTE-START PARTIAL-VALUE TOKENS).
STATE is `normal', `ident', `string', `line-comment', or
`block-comment'.  TOKENS is newest first and holds word, dot, and
closed identifier tokens for the current statement."
  (let* ((start (point-min))
         (end (point))
         (text (buffer-substring-no-properties start end))
         (length (length text))
         (backend (and (boundp 'sql-grid--connection)
                       sql-grid--connection
                       (sql-grid-connection-backend sql-grid--connection)))
         (server (memq backend '(mysql mariadb)))
         (i 0)
         (state 'normal)
         (quote-char nil)
         (quote-start nil)
         (ident-value nil)
         (word-start nil)
         (block-depth 0)
         (dollar-delimiter nil)
         (escape-string nil)
         (tokens nil))
    (cl-labels
        ((flush-word (at)
           (when word-start
             (push (list :type 'word
                         :value (substring text word-start at)
                         :start (+ start word-start)
                         :end (+ start at))
                   tokens)
             (setq word-start nil)))
         (break-chain (at)
           (flush-word at)
           (setq tokens nil))
         (push-ident (at)
           (push (list :type 'ident
                       :value ident-value
                       :start (+ start quote-start)
                       :end (+ start at)
                       :quote quote-char)
                 tokens)
           (setq state 'normal
                 quote-char nil
                 quote-start nil
                 ident-value nil)))
      (while (< i length)
        (let ((char (aref text i))
              (next (and (< (1+ i) length) (aref text (1+ i)))))
          (pcase state
            ('normal
             (cond
              ((memq char '(?\s ?\t ?\n ?\r ?\f ?\v))
               (flush-word i))
              ((and (= char ?-) (eq next ?-)
                    (or (not server)
                        (>= (+ i 2) length)
                        (memq (aref text (+ i 2))
                              '(?\s ?\t ?\n ?\r ?\f ?\v))))
               (flush-word i)
               (setq state 'line-comment)
               (setq i (1+ i)))
              ((and server (= char ?#))
               (flush-word i)
               (setq state 'line-comment))
              ((and (= char ?/) (eq next ?*))
               (flush-word i)
               (setq state 'block-comment
                     block-depth 1)
               (setq i (1+ i)))
              ((and (eq backend 'postgres) (= char ?$)
                    (or (= i 0)
                        (not (sql-grid-schema--word-char-p
                              (aref text (1- i)))))
                    (string-match "\\$\\(?:\\|[[:alpha:]_][[:alnum:]_]*\\)\\$"
                                  text i)
                    (= (match-beginning 0) i))
               (break-chain i)
               (setq dollar-delimiter (match-string 0 text)
                     state 'dollar-string
                     i (1- (match-end 0))))
              ((= char ?')
               (break-chain i)
               (setq state 'string quote-char ?' quote-start i
                     escape-string
                     (and (eq backend 'postgres)
                          (> i 0)
                          (memq (aref text (1- i)) '(?e ?E))
                          (or (= i 1)
                              (not (sql-grid-schema--word-char-p
                                    (aref text (- i 2))))))))
              ((= char ?\")
               (if server
                   (progn
                     (break-chain i)
                     (setq state 'string quote-char ?\" quote-start i))
                 (flush-word i)
                 (setq state 'ident quote-char ?\" quote-start i
                       ident-value "")))
              ((= char ?`)
               (flush-word i)
               (setq state 'ident quote-char ?` quote-start i
                     ident-value ""))
              ((and (= char ?\[) (eq backend 'sqlite))
               (flush-word i)
               (setq state 'ident quote-char ?\[ quote-start i
                     ident-value ""))
              ((= char ?.)
               (flush-word i)
               (push (list :type 'dot
                           :start (+ start i)
                           :end (+ start (1+ i)))
                     tokens))
              ((sql-grid-schema--word-char-p char)
               (unless word-start
                 (setq word-start i)))
              (t
               (break-chain i))))
            ('ident
             (cond
              ((and (not (eq quote-char ?\[))
                    (eq char quote-char))
               (if (eq next quote-char)
                   (progn
                     (setq ident-value (concat ident-value (char-to-string char)))
                     (setq i (1+ i)))
                 (push-ident (1+ i))))
              ((and (eq quote-char ?\[) (eq char ?\]))
               (push-ident (1+ i)))
              (t
               (setq ident-value (concat ident-value (char-to-string char))))))
            ('string
             (cond
              ((and (or server escape-string) (eq char ?\\))
               (when next
                 (setq i (1+ i))))
              ((eq char quote-char)
               (if (eq next quote-char)
                   (setq i (1+ i))
                 (setq state 'normal
                       quote-char nil
                       quote-start nil
                       escape-string nil)))))
            ('dollar-string
             (when (and (<= (+ i (length dollar-delimiter)) length)
                        (string= dollar-delimiter
                                 (substring text i (+ i (length dollar-delimiter)))))
               (setq state 'normal
                     i (+ i (1- (length dollar-delimiter))))))
            ('line-comment
             (when (memq char '(?\n ?\r))
               (setq state 'normal)))
            ('block-comment
             (cond
              ((and (eq backend 'postgres) (= char ?/) (eq next ?*))
               (setq block-depth (1+ block-depth)
                     i (1+ i)))
              ((and (= char ?*) (eq next ?/))
               (setq block-depth (1- block-depth)
                     i (1+ i))
               (when (= block-depth 0)
                 (setq state 'normal))))))
          (setq i (1+ i))))
      (cond
       ((eq state 'ident)
        (list state quote-char (+ start quote-start) ident-value tokens))
       ((eq state 'normal)
        (flush-word length)
        (list state nil nil nil tokens))
       (t
        (list state nil nil nil tokens))))))

(defun sql-grid-schema--completion-chain ()
  "Return completion chain for point, or nil inside strings/comments.
The value is (START PREFIX SEGMENTS QUOTE-CHAR STATE), where SEGMENTS
are the unescaped qualifier names before the partial SEGMENTS prefix."
  (pcase-let* ((`(,state ,quote-char ,quote-start ,value ,tokens)
                (sql-grid-schema--scan)))
    (unless (memq state '(string dollar-string line-comment block-comment))
      (let ((start (point))
            (prefix "")
            (segments nil)
            (current-quote nil)
            (rest tokens))
        (cond
         ((eq state 'ident)
          (setq start quote-start
                prefix value
                current-quote quote-char))
         ((and (car rest) (eq (plist-get (car rest) :type) 'dot))
          ;; Cursor follows a qualifier dot; the loop below consumes it.
          nil)
         ((and (car rest)
               (memq (plist-get (car rest) :type) '(word ident))
               (= (plist-get (car rest) :end) (point)))
          (setq start (plist-get (car rest) :start)
                prefix (plist-get (car rest) :value)
                current-quote (plist-get (car rest) :quote)
                rest (cdr rest))))
        (while (and rest (eq (plist-get (car rest) :type) 'dot))
          (let ((token (cadr rest)))
            (if (and token
                     (memq (plist-get token :type) '(word ident)))
                (progn
                  (push (plist-get token :value) segments)
                  (setq rest (cddr rest)))
              (setq rest nil))))
        (list start prefix segments current-quote state)))))

(defun sql-grid-schema--completion-end (state quote-char)
  "Return the end position to replace for STATE and QUOTE-CHAR.
Extend over the rest of an unquoted identifier or quoted identifier,
including a closing quote so completion cannot duplicate a suffix."
  (cond
   ((eq state 'ident)
    (let ((end (point))
          (limit (point-max))
          (done nil))
      (while (and (not done) (< end limit))
        (let ((char (char-after end))
              (next (and (< (1+ end) limit) (char-after (1+ end)))))
          (cond
           ((and (not (eq quote-char ?\[))
                 quote-char
                 (eq char quote-char))
            (if (and next (eq next quote-char))
                (setq end (+ end 2))
              (setq end (1+ end)
                    done t)))
           ((and (eq quote-char ?\[) (eq char ?\]))
            (setq end (1+ end)
                  done t))
           (t
            (setq end (1+ end))))))
      end))
   ((eq state 'normal)
    (let ((end (point))
          (limit (point-max)))
      (while (and (< end limit)
                  (sql-grid-schema--word-char-p (char-after end)))
        (setq end (1+ end)))
      end))
   (t (point))))

(defun sql-grid-schema--ascii-fold (string)
  "Fold ASCII uppercase letters in STRING to lowercase."
  (apply #'string
         (mapcar (lambda (char)
                   (if (<= ?A char ?Z)
                       (+ char (- ?a ?A))
                     char))
                 (string-to-list string))))

(defun sql-grid-schema--name-equal (a b backend)
  "Return non-nil when names A and B match for BACKEND.
SQLite folds ASCII letters only; other backends use case folding."
  (and (stringp a) (stringp b)
       (pcase backend
         ('sqlite
          (string= (sql-grid-schema--ascii-fold a)
                   (sql-grid-schema--ascii-fold b)))
         ('postgres (string= a b))
         (_ (eq t (compare-strings a nil nil b nil nil t))))))

 (defun sql-grid-schema--column-names (table)
  "Return normalized column names for TABLE."
  (delq nil
        (mapcar (lambda (column)
                  (let ((name (plist-get column :name)))
                    (and (stringp name) name)))
                (plist-get table :columns))))

(defun sql-grid-schema--render-identifier (name backend quote-char)
  "Render NAME for BACKEND, forcing QUOTE-CHAR when non-nil."
  (sql-grid-schema--identifier name backend
                                (and quote-char t) quote-char))

(defun sql-grid-schema--completion-candidates (segments backend quote-char)
  "Return cached candidates matching SEGMENTS for BACKEND and QUOTE-CHAR."
  (let ((result nil)
        (render (lambda (name)
                  (sql-grid-schema--render-identifier
                   name backend quote-char))))
    (cl-labels
        ((add (display raw)
           (when display
             (push (cons display (or raw display)) result)))
         (add-columns (table)
           (dolist (column (sql-grid-schema--column-names table))
             (add (funcall render column) column))))
      (cond
       ((null segments)
        (dolist (table sql-grid-schema--cache)
          (let ((name (plist-get table :name))
                (schema (plist-get table :schema)))
            (add (funcall render name) name)
            (when schema
              (add (concat (funcall render schema) "."
                           (funcall render name))
                   (concat schema "." name)))
            (add-columns table))))
       ((= (length segments) 1)
        (let ((qualifier (car segments)))
          (dolist (table sql-grid-schema--cache)
            (when (sql-grid-schema--name-equal
                   qualifier (plist-get table :name) backend)
              (add-columns table))
            (when (sql-grid-schema--name-equal
                   qualifier (plist-get table :schema) backend)
              (let ((name (plist-get table :name)))
                (add (funcall render name) name))))))
       ((= (length segments) 2)
        (let ((schema (nth 0 segments))
              (table-name (nth 1 segments)))
          (dolist (table sql-grid-schema--cache)
            (when (and (sql-grid-schema--name-equal
                        schema (plist-get table :schema) backend)
                       (sql-grid-schema--name-equal
                        table-name (plist-get table :name) backend))
              (add-columns table))))))
      (let ((seen (make-hash-table :test #'equal))
            (unique nil))
        (dolist (candidate result)
          (unless (gethash (car candidate) seen)
            (puthash (car candidate) t seen)
            (push candidate unique)))
        (sort unique
              (lambda (a b) (string< (car a) (car b))))))))

(defun sql-grid-schema--prefix-match-p (prefix string backend)
  "Return non-nil when PREFIX matches STRING for BACKEND."
  (if (eq backend 'sqlite)
      (string-prefix-p (sql-grid-schema--ascii-fold prefix)
                       (sql-grid-schema--ascii-fold string))
    (string-prefix-p (downcase prefix) (downcase string))))

(defun sql-grid-schema--char-equal-p (a b backend)
  "Return non-nil when characters A and B match for BACKEND."
  (if (eq backend 'sqlite)
      (eq (if (<= ?A a ?Z) (+ a (- ?a ?A)) a)
          (if (<= ?A b ?Z) (+ b (- ?a ?A)) b))
    (eq (downcase a) (downcase b))))

(defun sql-grid-schema--common-prefix (strings backend)
  "Return the common prefix of STRINGS for BACKEND."
  (if (null strings)
      nil
    (let ((prefix (car strings)))
      (dolist (string (cdr strings))
        (let ((limit (min (length prefix) (length string)))
              (index 0))
          (while (and (< index limit)
                      (sql-grid-schema--char-equal-p
                       (aref prefix index) (aref string index) backend))
            (setq index (1+ index)))
          (setq prefix (substring prefix 0 index))))
      prefix)))

(defun sql-grid-schema--completion-table (candidates backend)
  "Return a completion table for CANDIDATES on BACKEND.
CANDIDATES are (DISPLAY . RAW) pairs.  DISPLAY is inserted and RAW is
the unquoted form used for matching, so reserved names can be quoted
without hiding them from an unquoted prefix."
  (lambda (string predicate action)
    (cond
     ((eq action 'metadata)
      '(metadata (category . sql-grid-schema)))
     ((eq action 'boundaries)
      nil)
     (t
      (let ((matches
             (cl-remove-if-not
              (lambda (candidate)
                (and (or (sql-grid-schema--prefix-match-p
                          string (car candidate) backend)
                         (sql-grid-schema--prefix-match-p
                          string (cdr candidate) backend))
                     (or (null predicate)
                         (funcall predicate (car candidate)))))
              candidates)))
        (pcase action
          ((or 't 'lambda)
           (mapcar #'car matches))
          ('nil
           (let ((displays (mapcar #'car matches)))
             (cond
              ((null displays) nil)
              ((cl-some (lambda (display) (string= string display))
                        displays)
               t)
              ((cdr displays)
               (sql-grid-schema--common-prefix displays backend))
              (t
               (car displays)))))
          ('test-completion
           (cl-some (lambda (candidate)
                      (or (sql-grid-schema--name-equal
                           string (car candidate) backend)
                          (sql-grid-schema--name-equal
                           string (cdr candidate) backend)))
                    matches))
          (_ nil)))))))

(defun sql-grid-completion-at-point ()
  "Complete cached table and column names without database access."
  (when (and (boundp 'sql-grid--connection) sql-grid--connection)
    (unless (or sql-grid-schema--loaded sql-grid-schema--job
                sql-grid-schema--error)
      (condition-case nil
          (sql-grid-schema-refresh)
        (error nil)))
    (when sql-grid-schema--loaded
      (let ((chain (sql-grid-schema--completion-chain)))
        (when chain
          (pcase-let ((`(,start ,_prefix ,segments ,quote-char ,state)
                       chain))
            (let* ((backend
                    (sql-grid-connection-backend sql-grid--connection))
                   (candidates
                    (sql-grid-schema--completion-candidates
                     segments backend quote-char)))
              (when candidates
                (list start
                      (sql-grid-schema--completion-end state quote-char)
                      (sql-grid-schema--completion-table candidates backend)
                      :exclusive 'no)))))))))

(defun sql-grid-schema-browser-buffer (&optional source)
  "Return SOURCE's live schema browser buffer, or nil.
SOURCE defaults to the current buffer."
  (let* ((source (or source (current-buffer)))
         (browser (and (buffer-live-p source)
                       (buffer-local-value
                        'sql-grid-schema--browser-source source))))
    (and (buffer-live-p browser) browser)))

(defun sql-grid-schema--get-browser-buffer (source)
  "Return the browser buffer associated with SOURCE, creating it."
  (let ((buffer (get-buffer-create
                 (format "*SQL Grid Schema: %s*" (buffer-name source)))))
    (with-current-buffer source
      (setq sql-grid-schema--browser-source buffer)
      (add-hook 'kill-buffer-hook #'sql-grid-schema--source-killed nil t))
    buffer))

(defun sql-grid-schema-update-browser ()
  "Re-render the current editor's live owned schema browser.
Return the browser buffer, or nil when there is nothing to update.
This performs no SQL and schedules no timers.  Run it in the editor
buffer, including after the editor's connection is cleared."
  (interactive)
  (let* ((source (current-buffer))
         (browser (sql-grid-schema-browser-buffer source)))
    (when (and (buffer-live-p browser)
               (eq source
                   (buffer-local-value 'sql-grid-schema--browser-source
                                       browser)))
      (sql-grid-schema--render-browser browser source)
      browser)))

(defun sql-grid-schema--source-killed ()
  "Mark the current source buffer's schema browser as orphaned."
  (let ((browser sql-grid-schema--browser-source))
    (setq sql-grid-schema--browser-source nil)
    (when (buffer-live-p browser)
      (with-current-buffer browser
        (setq sql-grid-schema--browser-source nil)
        (sql-grid-schema--render-browser (current-buffer) nil)))))

(defun sql-grid-browse-schema ()
  "Show schema browser for current connected SQL editor."
  (interactive)
  (unless (and (boundp 'sql-grid--connection) sql-grid--connection)
    (user-error "SQL buffer is not connected"))
  (let* ((source (current-buffer))
         (buffer (sql-grid-schema--get-browser-buffer source))
         (loaded sql-grid-schema--loaded))
    (sql-grid-schema--render-browser buffer source)
    (pop-to-buffer buffer)
    (unless loaded
      (sql-grid-schema-refresh source))))

(defun sql-grid-schema--display-kind (kind)
  "Return a short display form of normalized KIND."
  (if (stringp kind) kind "table"))

(defun sql-grid-schema--render-browser (buffer source)
  "Render the schema browser in BUFFER for SOURCE.
SOURCE nil means that the editor buffer no longer exists."
  (let ((connected (and (buffer-live-p source)
                        (buffer-local-value 'sql-grid--connection source)))
        (backend nil)
        (schema nil)
        (loaded nil)
        (error-text nil)
        (job nil))
    (when source
      (setq backend (and connected
                         (sql-grid-connection-backend connected))
            schema (buffer-local-value 'sql-grid-schema--cache source)
            loaded (buffer-local-value 'sql-grid-schema--loaded source)
            error-text (buffer-local-value 'sql-grid-schema--error source)
            job (buffer-local-value 'sql-grid-schema--job source)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Database schema\n\n")
        (cond
         ((not (buffer-live-p source))
          (insert "Source SQL buffer is gone.\n"))
         ((not connected)
          (insert "Source SQL buffer is disconnected.\n"))
         ((not loaded)
          (insert
           (cond
            (error-text
             (format "Schema discovery failed: %s\nPress g to retry.\n"
                     error-text))
            (job "Schema discovery in progress…\n")
            (t "Schema not available. Press g to refresh.\n"))))
         ((null schema)
          (insert "No tables or views found.\n"))
         (t
          (when error-text
            (insert (format "Schema refresh failed: %s\n\n" error-text)))
          (dolist (table schema)
            (insert (format "%s%s (%s)\n"
                            (if (plist-get table :schema)
                                (concat (sql-grid-schema--display-identifier
                                         (plist-get table :schema) backend)
                                        ".")
                              "")
                            (sql-grid-schema--display-identifier
                             (plist-get table :name) backend)
                            (sql-grid-schema--display-kind
                             (plist-get table :kind))))
            (dolist (column (plist-get table :columns))
              (insert
               (format "  %s %s%s%s%s%s%s%s\n"
                       (sql-grid-schema--display-identifier
                        (plist-get column :name) backend)
                       (or (plist-get column :type) "")
                       (if (plist-get column :nullable) " NULL" " NOT NULL")
                       (if (plist-get column :primary-key) " PRIMARY KEY" "")
                       (pcase (plist-get column :key)
                         ('unique " UNIQUE")
                         ('index " KEY")
                         (_ ""))
                       (if (plist-get column :default)
                           (format " DEFAULT %s" (plist-get column :default))
                         "")
                       (if (plist-get column :extra)
                           (concat " " (plist-get column :extra))
                         "")
                       (if (plist-get column :hidden) " HIDDEN" ""))))
            (dolist (index (plist-get table :indexes))
              (insert
               (format "  INDEX %s %s (%s)%s%s\n"
                       (sql-grid-schema--display-identifier
                        (plist-get index :name) backend)
                       (if (plist-get index :unique)
                           "UNIQUE" "NON-UNIQUE")
                       (mapconcat (lambda (column)
                                    (or column "<expression>"))
                                  (plist-get index :columns)
                                  ", ")
                       (if (plist-get index :include)
                           (format " INCLUDE (%s)"
                                   (mapconcat #'identity
                                              (plist-get index :include)
                                              ", "))
                         "")
                       (cond
                        ((plist-get index :predicate)
                         (format " WHERE %s"
                                 (plist-get index :predicate)))
                        ((plist-get index :partial) " PARTIAL")
                        (t "")))))
            (dolist (fk (plist-get table :foreign-keys))
              (insert
               (format "  FOREIGN KEY %s -> %s%s%s%s\n"
                       (sql-grid-schema--display-identifier
                        (plist-get fk :column) backend)
                       (if (plist-get fk :target-schema)
                           (concat
                            (sql-grid-schema--display-identifier
                             (plist-get fk :target-schema) backend)
                            ".")
                         "")
                       (sql-grid-schema--display-identifier
                        (plist-get fk :target-table) backend)
                       (if (plist-get fk :target-column)
                           (let ((target-column
                                  (sql-grid-schema--display-identifier
                                   (plist-get fk :target-column) backend)))
                             (if (eq backend 'postgres)
                                 (format " (%s)" target-column)
                               (concat "." target-column)))
                         (if (eq backend 'postgres)
                             " (<primary key>)"
                           ".<primary key>"))
                       (if (and (memq backend '(mysql mariadb))
                                (plist-get fk :name))
                           (format " [%s]" (plist-get fk :name))
                         ""))))))))
      (special-mode)
      (local-set-key (kbd "g") #'sql-grid-schema--refresh-from-browser)
      (setq-local sql-grid-schema--browser-source source))))

(defun sql-grid-schema--refresh-from-browser ()
  "Refresh schema for browser's source buffer."
  (interactive)
  (unless (buffer-live-p sql-grid-schema--browser-source)
    (user-error "Source SQL buffer is gone"))
  (sql-grid-schema-refresh sql-grid-schema--browser-source))

(provide 'sql-grid-schema)
;;; sql-grid-schema.el ends here
