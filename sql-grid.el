;;; sql-grid.el --- Execute SQL and view result tables -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: OpenSpec Authors
;; Maintainer: OpenSpec Authors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: databases, sql, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; for SQLite, MySQL, MariaDB, and PostgreSQL.  See README.md for setup and backend semantics.

;;; Code:

(require 'cl-lib)
(require 'sql)
(require 'color)
(require 'pixel-fill)
(require 'subr-x)
(require 'button)
(require 'face-remap)
(require 'sql-grid-db)
(require 'sql-grid-schema)
(require 'sql-grid-selection)


(declare-function evil-declare-motion "evil-common" (command))

(defgroup sql-grid nil
  "Execute SQL and display native-grid tables."
  :group 'sql)

(defcustom sql-grid-connections nil
  "Named database profiles offered by the interactive connection commands.
Each entry is (NAME :backend BACKEND :database DATABASE ...), where NAME
is a nonempty string and BACKEND is `sqlite', `mysql', `mariadb', or
`postgres'.  Server profiles may also specify :host, :port, :user, and
:password.  A string :password, including an empty string, is used as-is;
an omitted or nil :password prompts for a password.  Passwords in this
option are stored in plaintext and persist until removed from the
configuration.  Each selection opens a fresh buffer-owned connection;
disconnecting or killing that buffer clears its connection password but
does not change the profile.  With no profiles, connection commands
prompt for all settings as usual."
  :type '(alist :key-type (string :tag "Name")
                :value-type
                (plist :options
                       ((:backend (choice (const sqlite)
                                          (const mysql)
                                          (const mariadb)
                                          (const postgres)))
                        (:database string)
                        (:host string)
                        (:port (choice (const :tag "Client default" nil)
                                       integer))
                        (:user string)
                        (:password (choice (const :tag "Prompt for password" nil)
                                           string)))))
  :group 'sql-grid)

(defcustom sql-grid-result-format 'grid
  "Default view for newly created result buffers.
Use `grid' for a native Emacs grid or `text' for an aligned text table.
Only grid requires a graphical display.
Existing result buffers keep their own choice; press \\[sql-grid-set-result-format]
there to switch views without executing SQL again."
  :type '(choice (const :tag "Grid" grid)
                 (const :tag "Text table" text))
  :group 'sql-grid)

(defcustom sql-grid-freeze-column-names nil
  "Whether column names stay visible above scrolling result rows.
When non-nil, show status in the tab line and column names in the
header line, in both grid and text views.  Refresh existing results
with `sql-grid-refresh' after changing this option."
  :type 'boolean
  :group 'sql-grid)

(defcustom sql-grid-grid-font-family nil
  "Emacs font family for native grid cells, or nil to inherit.
Use an Emacs family name, not a CSS fallback list."
  :type '(choice (const :tag "Inherit" nil) string)
  :group 'sql-grid)


(defcustom sql-grid-max-rows 1000
  "Maximum rows displayed per page, or nil to display all result rows.
Must be nil or a positive integer.  All returned rows remain retained;
this setting does not limit SQL execution or retrieval.  New results
capture the setting; refresh and navigation keep the captured page size.
Use `sql-grid-set-max-rows' for a temporary result-buffer override."
  :type '(choice (const :tag "All rows (no pagination)" nil)
                 (integer :tag "Rows per page"))
  :group 'sql-grid)

(defun sql-grid--validate-max-rows ()
  "Signal an error unless `sql-grid-max-rows' is nil or positive."
  (unless (or (null sql-grid-max-rows)
              (and (integerp sql-grid-max-rows)
                   (> sql-grid-max-rows 0)))
    (user-error "`sql-grid-max-rows' must be nil or a positive integer")))

(defcustom sql-grid-max-cell-chars 80
  "Maximum cell length in grid and text tables, including trailing dots.
When non-nil, values longer than this number are shortened to fit;
values exactly this long are unchanged.  Set to nil for unlimited text."
  :type '(choice (const :tag "Unlimited" nil) (integer :tag "Characters"))
  :group 'sql-grid)
(defcustom sql-grid-show-cell-tooltips t
  "Whether result cells show their complete values when hovered."
  :type 'boolean
  :group 'sql-grid)
(defcustom sql-grid-cell-display 'buffer
  "Where to show the full original value when inspecting a result cell.
Use `buffer' for a separate read-only buffer, or `minibuffer' for the
echo area without leaving the result window.  This applies to both
keyboard and mouse inspection in every result format."
  :type '(choice (const :tag "Separate buffer" buffer)
                 (const :tag "Minibuffer (echo area)" minibuffer))
  :group 'sql-grid)
(defcustom sql-grid-focus-results t
  "Whether displaying SQL results selects their window.
When nil, show results without replacing the selected source window
or moving its cursor.  This also applies to programmatic result display."
  :type 'boolean
  :group 'sql-grid)

(defcustom sql-grid-result-split-direction 'right
  "Direction to split the selected window when showing new SQL results.
Use `right', `below', `left', or `above'.  An already visible result
window is reused; changing this option does not move existing windows.
This applies to query execution and programmatic result display."
  :type '(choice (const right) (const below) (const left) (const above))
  :group 'sql-grid)

(defvar-local sql-grid--connection nil)
(defvar-local sql-grid--result-buffer nil)
(defvar-local sql-grid--results nil)
(defvar-local sql-grid--result-format nil)
(defvar-local sql-grid--column-orders nil)
(defvar-local sql-grid--row-filter nil)
(defvar-local sql-grid--page-size nil)
(defvar-local sql-grid--pages nil)
(defvar-local sql-grid--max-rows-override nil)
(defvar-local sql-grid--max-rows-override-set nil)
(defvar-local sql-grid--sorts nil)
(defvar-local sql-grid--status-lines nil)
(defvar-local sql-grid--column-name-lines nil)
(defvar-local sql-grid--saved-tab-line-format nil)
(defvar-local sql-grid--cursor-overlay nil)
(defvar-local sql-grid--saved-cursor-type nil)


(defvar-local sql-grid--owns-connection nil)
(defvar-local sql-grid--dedicated-editor nil)

(defvar-local sql-grid--execution-job nil)
(defvar-local sql-grid--execution-state nil)
(defvar-local sql-grid--execution-start nil)
(defvar-local sql-grid--execution-error nil)
(defvar-local sql-grid--execution-generation nil)
(defvar-local sql-grid--execution-source nil)
(defvar-local sql-grid--execution-timer nil)

(defun sql-grid--stop-execution-timer ()
  "Cancel this buffer's live elapsed-time timer, if any."
  (when (timerp sql-grid--execution-timer)
    (cancel-timer sql-grid--execution-timer))
  (setq sql-grid--execution-timer nil))

(defun sql-grid--refresh-execution-display (source)
  "Redraw live execution details for SOURCE and its result buffer."
  (when (buffer-live-p source)
    (with-current-buffer source
      (force-mode-line-update)
      (when (buffer-live-p sql-grid--result-buffer)
        (with-current-buffer sql-grid--result-buffer
          (force-mode-line-update))))))

(defun sql-grid--start-execution-timer (source generation)
  "Refresh elapsed time for SOURCE's running GENERATION until it finishes.
The timer cancels itself when the query finishes, is superseded, or
SOURCE is killed."
  (sql-grid--stop-execution-timer)
  (let (timer)
    (setq timer
          (run-at-time
           0.1 0.1
           (lambda ()
             (if (and (buffer-live-p source)
                      (with-current-buffer source
                        (and (eq generation sql-grid--execution-generation)
                             (eq sql-grid--execution-state 'running))))
                 (sql-grid--refresh-execution-display source)
               (cancel-timer timer)
               (when (buffer-live-p source)
                 (with-current-buffer source
                   (when (eq timer sql-grid--execution-timer)
                     (setq sql-grid--execution-timer nil))))))))
    (setq sql-grid--execution-timer timer)))

(defun sql-grid--sqlite-memory-p (connection)
  "Return non-nil when CONNECTION uses an in-memory SQLite database."
  (and (eq (sql-grid-connection-backend connection) 'sqlite)
       (equal (sql-grid-connection-database connection) ":memory:")))

(defun sql-grid--execution-status ()
  "Describe the source editor's current execution state."
  (let ((source (or sql-grid--execution-source (current-buffer))))
    (when (buffer-live-p source)
      (with-current-buffer source
        (pcase sql-grid--execution-state
          ('running
           (concat "Running"
                   (when sql-grid--execution-start
                     (format " %.3fs"
                             (- (float-time) sql-grid--execution-start)))
                   " | C-c C-k: cancel"
                   (when (and sql-grid--connection
                              (eq (sql-grid-connection-backend
                                   sql-grid--connection)
                                  'sqlite))
                     (if (sql-grid--sqlite-memory-p sql-grid--connection)
                         " (closes connection; discards in-memory database)"
                       " (closes SQLite connection)"))))
          ('cancelled
           (if sql-grid--execution-error
               (concat "Cancelled: " sql-grid--execution-error)
             "Cancelled"))
          ('failed
           (concat "Failed: " (or sql-grid--execution-error "unknown error")))
          ('completed nil))))))

(defun sql-grid--editor-header ()
  "Return editing keys and the current query state."
  (concat (when-let ((status (sql-grid--execution-status)))
            (concat (replace-regexp-in-string "%" "%%" status t t) " | "))
          "C-c C-c: execute   C-c C-r: region   C-c C-s: schema   C-c C-d: disconnect"))

(defun sql-grid-cancel ()
  "Cancel the running query in this editor or its result buffer.
SQLite cancellation closes the connection.  For an in-memory SQLite
database it also discards the entire database, so confirm first;
declining leaves the query running.  Only this command prompts;
`sql-grid-db-cancel' and lifetime teardown stay unprompted."
  (interactive)
  (let ((source (or sql-grid--execution-source (current-buffer))))
    (unless (buffer-live-p source)
      (user-error "The SQL source buffer no longer exists"))
    (with-current-buffer source
      (unless sql-grid--execution-job
        (user-error "No SQL query is running"))
      (if (and sql-grid--connection
               (sql-grid--sqlite-memory-p sql-grid--connection)
               (not (yes-or-no-p
                     "Cancel query? This closes the SQLite connection and discards the entire in-memory database. ")))
          (progn
            (message "SQL query left running")
            nil)
        (sql-grid-db-cancel sql-grid--execution-job)))))

(defun sql-grid--handle-connection-loss (connection reason)
  "Forget CONNECTION after its SQLite worker closed.
REASON describes the closure and whether a transaction rolled back or an
in-memory database was discarded.  Clear cached schema and passwords,
keep retained results, and never reconnect automatically."
  (sql-grid-schema-clear)
  (setf (sql-grid-connection-password connection) nil)
  (setq sql-grid--connection nil
        sql-grid--owns-connection nil)
  (sql-grid-schema-update-browser)
  (when (buffer-live-p sql-grid--result-buffer)
    (with-current-buffer sql-grid--result-buffer
      (setq sql-grid--connection nil)))
  (message "%s.  Reconnect with M-x sql-grid-connect-buffer"
           reason))

(defun sql-grid--execution-cleanup ()
  "Stop and cancel this buffer's query before the buffer goes away.
Detach its result buffer so a late callback cannot overwrite results."
  (let ((job sql-grid--execution-job)
        (connection sql-grid--connection)
        (source (current-buffer))
        (result sql-grid--result-buffer))
    (sql-grid--stop-execution-timer)
    (setq sql-grid--execution-generation nil
          sql-grid--execution-job nil
          sql-grid--execution-state nil
          sql-grid--execution-start nil
          sql-grid--execution-error nil)
    ;; The generation is already invalid, so a synchronous callback is ignored.
    (when job
      (ignore-errors (sql-grid-db-cancel job)))
    (when (buffer-live-p result)
      (with-current-buffer result
        (when (eq sql-grid--execution-source source)
          (setq sql-grid--execution-source nil))
        (when (and connection
                   (eq (sql-grid-connection-backend connection) 'sqlite)
                   (null (sql-grid-connection-handle connection)))
          (setq sql-grid--connection nil))))))

(defun sql-grid--close-connection ()
  "Disconnect the buffer's owned connection and clear its password."
  (when sql-grid--connection
    (sql-grid--stop-execution-timer)
    (sql-grid-schema-clear)
    (setq sql-grid--execution-generation nil
          sql-grid--execution-job nil
          sql-grid--execution-state nil
          sql-grid--execution-start nil
          sql-grid--execution-error nil)
    (sql-grid-db-disconnect sql-grid--connection)
    (setf (sql-grid-connection-password sql-grid--connection) nil)
    (setq sql-grid--connection nil
          sql-grid--owns-connection nil)
    (sql-grid-schema-update-browser)
    (when (buffer-live-p sql-grid--result-buffer)
      (with-current-buffer sql-grid--result-buffer
        (setq sql-grid--connection nil)))))

(defun sql-grid--editor-killed ()
  "Close an owned connection when its buffer is killed or changes mode."
  (when sql-grid--owns-connection
    (sql-grid--close-connection)))

(defun sql-grid--require-grid-display ()
  "Signal an actionable error unless a graphical display is available."
  (unless (display-graphic-p)
    (user-error "Grid results require a graphical Emacs; choose text instead")))

(defun sql-grid--require-result-display (format)
  "Check that result FORMAT is supported on the current display."
  (unless (memq format '(grid text))
    (user-error "Unknown result format: %S" format))
  (when (eq format 'grid)
    (sql-grid--require-grid-display)))

(defun sql-grid--read-connection ()
  "Select a configured profile or prompt for settings, then open a connection."
  (let ((profile
         (when sql-grid-connections
           (let ((name (completing-read
                        "Connection (RET for manual settings): "
                        sql-grid-connections nil t)))
             (unless (string-empty-p name)
               (assoc name sql-grid-connections))))))
    (if (null profile)
        (sql-grid--read-manual-connection)
      (let* ((settings (cdr profile))
             (backend (plist-get settings :backend))
             (database (plist-get settings :database))
             (configured-password (plist-get settings :password)))
        (unless (and (memq backend '(sqlite mysql mariadb postgres))
                     (stringp database))
          (user-error "Connection profile %s requires a valid :backend and :database"
                      (car profile)))
        (when (and (not (eq backend 'sqlite))
                   (not (or (null configured-password)
                            (stringp configured-password))))
          (user-error "Connection profile %s :password must be a string or nil"
                      (car profile)))
        (sql-grid-db-connect
         (sql-grid-connection-create
          :backend backend :database database
          :host (unless (eq backend 'sqlite)
                  (or (plist-get settings :host) "localhost"))
          :port (unless (eq backend 'sqlite) (plist-get settings :port))
          :user (unless (eq backend 'sqlite) (plist-get settings :user))
          :password (unless (eq backend 'sqlite)
                      (if (stringp configured-password)
                          configured-password
                        (read-passwd (format "Password for %s: " (car profile)))))))))))

(defun sql-grid--read-manual-connection ()
  "Prompt for database settings and return an open connection."
  (let* ((backend
          (intern
           (completing-read "Database engine: "
                            '("sqlite" "mysql" "mariadb" "postgres") nil t)))
         (database
          (read-string (if (eq backend 'sqlite)
                           "SQLite file (or :memory:): "
                         "Database: ")))
         (host (unless (eq backend 'sqlite)
                 (read-string "Host [localhost]: " nil nil "localhost")))
         (port (unless (eq backend 'sqlite)
                 (let ((value (read-string "Port [default]: ")))
                   (unless (string-empty-p value)
                     (string-to-number value)))))
         (user (unless (eq backend 'sqlite) (read-string "User: ")))
         (password (unless (eq backend 'sqlite) (read-passwd "Password: ")))
         (connection
          (sql-grid-connection-create
           :backend backend :database database
           :host host :port port :user user :password password)))
    (sql-grid-db-connect connection)))

(defvar sql-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'sql-grid-execute-statement)
    (define-key map (kbd "C-c C-r") #'sql-grid-execute-region)
    (define-key map (kbd "C-c C-d") #'sql-grid-disconnect)
    (define-key map (kbd "C-c C-k") #'sql-grid-cancel)
    (define-key map (kbd "C-c C-s") #'sql-grid-browse-schema)
    map)
  "Keymap for `sql-grid-mode'.")

(define-derived-mode sql-grid-mode sql-mode "SQL-Grid"
  "Major mode for SQL buffers connected through sql-grid.
Use `sql-grid-connect-buffer' to connect the current buffer.
\\{sql-grid-mode-map}"
  (setq-local header-line-format '(:eval (sql-grid--editor-header)))
  (sql-grid-schema-setup))

(defun sql-grid--attach-connection (connection)
  "Make the current buffer own CONNECTION without changing its editing setup."
  (setq-local sql-grid--connection connection)
  (when (eq (sql-grid-connection-backend connection) 'postgres)
    (sql-set-product 'postgres))
  (setq-local sql-grid--owns-connection t)
  (setq-local sql-grid--execution-job nil
              sql-grid--execution-generation nil
              sql-grid--execution-state nil
              sql-grid--execution-start nil
              sql-grid--execution-error nil)
  (let ((source (current-buffer)))
    (setf (sql-grid-connection-on-close connection)
          (lambda (closed)
            (when (buffer-live-p source)
              (with-current-buffer source
                (when (eq closed sql-grid--connection)
                  (sql-grid--stop-execution-timer)
                  (let ((memory (sql-grid--sqlite-memory-p closed)))
                    (setq sql-grid--execution-job nil
                          sql-grid--execution-generation nil
                          sql-grid--execution-start nil
                          sql-grid--execution-state 'failed
                          sql-grid--execution-error
                          (if memory
                              "connection lost; in-memory database discarded; reconnect with M-x sql-grid-connect-buffer"
                            "connection lost; transaction rolled back; reconnect with M-x sql-grid-connect-buffer"))
                    (sql-grid--handle-connection-loss
                     closed
                     (if memory
                         "SQLite connection lost; in-memory database discarded"
                       "SQLite connection lost; uncommitted transaction rolled back"))
                    (sql-grid--refresh-execution-display source))))))))
  (add-hook 'kill-buffer-hook #'sql-grid--editor-killed nil t)
  (add-hook 'change-major-mode-hook #'sql-grid--editor-killed nil t)
  (add-hook 'kill-buffer-hook #'sql-grid--execution-cleanup nil t)
  (add-hook 'change-major-mode-hook #'sql-grid--execution-cleanup nil t))

(defun sql-grid-connect-buffer ()
  "Connect this buffer to SQLite, MySQL, MariaDB, or PostgreSQL in `sql-grid-mode'.
Preserve its contents and enable SQL editing and execution keybindings.
Disconnecting leaves this buffer open in `sql-grid-mode'.  Killing the
buffer or changing its major mode closes its connection."
  (interactive)
  (when sql-grid--connection
    (user-error "This buffer already has a sql-grid connection"))
  (sql-grid--require-result-display sql-grid-result-format)
  (let ((connection (sql-grid--read-connection)))
    (unless (derived-mode-p 'sql-grid-mode)
      (sql-grid-mode))
    (sql-grid--attach-connection connection)))

(defun sql-grid-connect ()
  "Connect to SQLite, MySQL, MariaDB, or PostgreSQL and open an SQL-Grid editor."
  (interactive)
  (sql-grid--require-result-display sql-grid-result-format)
  (let* ((connection (sql-grid--read-connection))
         (buffer (generate-new-buffer
                  (format "*sql-grid %s*"
                          (sql-grid-connection-database connection)))))
    (with-current-buffer buffer
      (sql-grid-mode)
      (sql-grid--attach-connection connection)
      (setq-local sql-grid--dedicated-editor t))
    (pop-to-buffer buffer)))

(defun sql-grid-execute-region (begin end)
  "Execute SQL between BEGIN and END and display returned results."
  (interactive "r")
  (sql-grid--execute begin end))

(defun sql-grid-execute-statement ()
  "Execute the SQL block around point and display returned results.
Blank lines, including lines containing only spaces or tabs, separate
blocks.  Ignore any active region.  Each block must contain one SQL
statement; use `sql-grid-execute-region' for an explicit selection."
  (interactive)
  (unless sql-grid--connection
    (user-error "This buffer has no sql-grid connection"))
  (let (begin end)
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "[ \t\r]*$")
        (user-error "No SQL statement at point"))
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (not (looking-at-p "[ \t\r]*$"))))
        (forward-line -1))
      (setq begin (point))
      (while (and (not (eobp)) (not (looking-at-p "[ \t\r]*$")))
        (forward-line 1))
      (setq end (point)))
    (sql-grid--execute begin end)))

(defun sql-grid--show-result-buffer (buffer focus-results)
  "Show BUFFER in the configured direction and select it if FOCUS-RESULTS.
Reuse an existing result window on the selected frame."
  (let ((window
         (or (get-buffer-window buffer)
             (display-buffer
              buffer
              `((display-buffer-reuse-window display-buffer-in-direction)
                (direction . ,sql-grid-result-split-direction)
                (,(if (memq sql-grid-result-split-direction '(left right))
                      'window-width
                    'window-height)
                 . 0.5)
                (inhibit-same-window . t))))))
    (when (and focus-results (window-live-p window))
      (select-window window))))

(defun sql-grid--prepare-result-buffer (connection source focus-results)
  "Show SOURCE's result buffer while a query runs, keeping old results.
Bind cancellation and update CONNECTION without replacing retained rows."
  (unless (buffer-live-p sql-grid--result-buffer)
    (sql-grid-display-results connection nil))
  (with-current-buffer sql-grid--result-buffer
    (setq-local sql-grid--connection connection)
    (setq-local sql-grid--execution-source source)
    (local-set-key (kbd "C-c C-k") #'sql-grid-cancel))
  (sql-grid--show-result-buffer sql-grid--result-buffer focus-results)
  (sql-grid--refresh-execution-display source))

(defun sql-grid--execution-callback (source generation connection page-size
                                             focus-results results error)
  "Finish GENERATION for SOURCE with RESULTS or ERROR.
Keep prior results when ERROR is non-nil.  If the SQLite worker closed,
invalidate CONNECTION and report the rollback or in-memory discard
without reconnecting."
  (when (and (buffer-live-p source)
             (with-current-buffer source
               (eq generation sql-grid--execution-generation)))
    (with-current-buffer source
      (sql-grid--stop-execution-timer)
      (let* ((sqlite-p (eq (sql-grid-connection-backend connection) 'sqlite))
             (memory (and sqlite-p
                          (sql-grid--sqlite-memory-p connection)))
             (cancelled (and error (eq (car error) 'quit)))
             (connection-lost
              (and error sqlite-p
                   (null (sql-grid-connection-handle connection)))))
        (setq sql-grid--execution-job nil
              sql-grid--execution-generation nil
              sql-grid--execution-start nil
              sql-grid--execution-state
              (cond ((null error) 'completed)
                    (cancelled 'cancelled)
                    (t 'failed))
              sql-grid--execution-error
              (cond ((null error) nil)
                    ((and cancelled (not connection-lost)) nil)
                    (connection-lost
                     (concat
                      (cond
                       (memory
                        (if cancelled
                            "connection closed; in-memory database discarded"
                          "connection lost; in-memory database discarded"))
                       (cancelled
                        "connection closed; transaction rolled back")
                       (t "connection lost; transaction rolled back"))
                      "; reconnect with M-x sql-grid-connect-buffer"))
                    (t (error-message-string error))))
        (when connection-lost
          (sql-grid--handle-connection-loss
           connection
           (cond
            ((and memory cancelled)
             "SQLite query cancelled; in-memory database discarded")
            (memory
             "SQLite worker disconnected; in-memory database discarded")
            (cancelled
             "SQLite query cancelled and connection closed; transaction rolled back")
            (t
             "SQLite worker disconnected; transaction rolled back"))))
        (unless error
          (let ((sql-grid-max-rows page-size)
                (sql-grid-focus-results
                 (and focus-results
                      (memq (window-buffer (selected-window))
                            (list source sql-grid--result-buffer)))))
            (sql-grid-display-results connection results)))
        (when (and error (not connection-lost))
          (message "SQL %s"
                   (if cancelled "query cancelled" sql-grid--execution-error)))
        (sql-grid--refresh-execution-display source)))))

(defun sql-grid--execution-fail (generation error)
  "Stop GENERATION and record ERROR as its failed start or run state."
  (when (eq generation sql-grid--execution-generation)
    (sql-grid--stop-execution-timer)
    (setq sql-grid--execution-state 'failed
          sql-grid--execution-generation nil
          sql-grid--execution-start nil
          sql-grid--execution-error (error-message-string error))))

(defun sql-grid--restore-execution-origin (configuration source)
  "Undo result-window focus after a query failed to start.
CONFIGURATION is the pre-display window state; SOURCE becomes current."
  (when (window-configuration-p configuration)
    (set-window-configuration configuration))
  (when (buffer-live-p source)
    (set-buffer source)))

(defun sql-grid--execute (begin end)
  "Start executing the SQL between BEGIN and END without blocking Emacs."
  (unless sql-grid--connection (user-error "This buffer has no sql-grid connection"))
  (when sql-grid--execution-job (user-error "A SQL query is already running"))
  (sql-grid--validate-max-rows)
  (sql-grid--require-result-display
   (if (buffer-live-p sql-grid--result-buffer)
       (buffer-local-value 'sql-grid--result-format sql-grid--result-buffer)
     sql-grid-result-format))
  (let* ((sql (string-trim (buffer-substring-no-properties begin end)))
         (source (current-buffer))
         (connection sql-grid--connection)
         (generation (make-symbol "sql-grid-execution"))
         (page-size
          (let ((result-buffer sql-grid--result-buffer))
            (if (and (buffer-live-p result-buffer)
                     (buffer-local-value
                      'sql-grid--max-rows-override-set result-buffer))
                (buffer-local-value
                 'sql-grid--max-rows-override result-buffer)
              sql-grid-max-rows)))
         (focus-results sql-grid-focus-results)
         (displayed (current-buffer))
         (origin (current-window-configuration)))
    (when (string-empty-p sql) (user-error "No SQL to execute"))
    ;; Publish running state before the backend can invoke its callback.
    (setq sql-grid--execution-generation generation
          sql-grid--execution-state 'running
          sql-grid--execution-start (float-time)
          sql-grid--execution-error nil
          sql-grid--execution-job nil)
    (condition-case err
        (progn
          (sql-grid--prepare-result-buffer connection source focus-results)
          ;; Focus may have switched buffers; the timer belongs to SOURCE.
          (setq displayed (current-buffer))
          (with-current-buffer source
            (sql-grid--start-execution-timer source generation)
            (let ((job (sql-grid-db-execute-async
                        connection sql
                        (lambda (results error)
                          (sql-grid--execution-callback
                           source generation connection page-size focus-results
                           results error)))))
              ;; A synchronous callback already cleared the job.
              (when (eq sql-grid--execution-state 'running)
                (setq sql-grid--execution-job job)))))
      (error
       (with-current-buffer source
         (sql-grid--execution-fail generation err))
       (sql-grid--restore-execution-origin origin source)
       (sql-grid--refresh-execution-display source)
       (signal (car err) (cdr err))))
    ;; Preserve the focus/window-buffer outcome of result display.
    (when (buffer-live-p displayed) (set-buffer displayed))
    (buffer-local-value 'sql-grid--result-buffer source)))

(defun sql-grid-disconnect ()
  "Disconnect this buffer, closing it only if opened by `sql-grid-connect'."
  (interactive)
  (unless sql-grid--connection (user-error "This buffer has no sql-grid connection"))
  (sql-grid--close-connection)
  (message "SQL connection closed")
  (when sql-grid--dedicated-editor
    (kill-buffer (current-buffer))))

(defun sql-grid--face-color (face attribute fallback)
  "Return FACE's ATTRIBUTE as a usable RGB color, or FALLBACK."
  (let ((color (face-attribute face attribute nil t)))
    (if (and (stringp color) (not (equal color "unspecified")))
        (condition-case nil
            (apply #'color-rgb-to-hex (append (color-name-to-rgb color) '(2)))
          (error fallback))
      fallback)))

(defun sql-grid--unsafe-truncation-boundary-p (text end)
  "Return non-nil if END splits a combining mark, linker, or joiner."
  (let* ((before (and (> end 0) (aref text (1- end))))
         (after (and (< end (length text)) (aref text end)))
         (after-category (and after (get-char-code-property after 'general-category)))
         (before-combining (and before
                                (get-char-code-property before
                                                        'canonical-combining-class))))
    (or (and after (memq after-category '(Mn Mc Me)))
        (and before (= (or before-combining 0) 9))
        (memq before '(#x200c #x200d))
        (memq after '(#x200c #x200d)))))

(defun sql-grid--safe-truncation-prefix-length (text maximum)
  "Return a safe codepoint prefix length no greater than MAXIMUM.
Avoid ending before Unicode marks or after virama/linker and joiner
characters.  This guards common combining sequences, not all grapheme
cluster boundaries."
  (let ((end (min maximum (length text))))
    (while (and (> end 0) (sql-grid--unsafe-truncation-boundary-p text end))
      (setq end (1- end)))
    end))

(defun sql-grid--display-value (value)
  "Convert VALUE to a one-line display string, preserving NULL distinctly."
  (let* ((text (if (null value) "NULL" (format "%s" value)))
         (text (replace-regexp-in-string "[\n\r\t\f\v]+" " " text)))
    (if (and sql-grid-max-cell-chars
             (> (length text) sql-grid-max-cell-chars))
        (let ((limit sql-grid-max-cell-chars))
          (if (<= limit 0)
              ""
            (concat (substring text 0
                               (sql-grid--safe-truncation-prefix-length
                                text (1- limit)))
                    "…")))
      text)))

(defun sql-grid--grid-font-face (frame)
  "Return a native cell font face with buffer text scaling on FRAME."
  (append (when sql-grid-grid-font-family
            (list :family sql-grid-grid-font-family))
          (list :height
                (round (* (face-attribute 'default :height frame t)
                          (if text-scale-mode
                              (expt text-scale-mode-step text-scale-mode-amount)
                            1))))))

(defun sql-grid--column-widths (display-rows frame font-face)
  "Return column widths and measured pixel rows for DISPLAY-ROWS."
  (let ((widths (make-vector (length (car display-rows)) 0))
        (pixel-rows nil))
    (dolist (row display-rows)
      (push (mapcar (lambda (text)
                      (sql-grid--grid-text-pixel-width text frame font-face))
                    row)
            pixel-rows))
    (setq pixel-rows (nreverse pixel-rows))
    (dolist (row pixel-rows)
      (cl-loop for width in row
               for index from 0 do
               (aset widths index (max (aref widths index) width))))
    (let ((char-width (max 1 (frame-char-width frame))))
      (cons (mapcar (lambda (width)
                      (* char-width (ceiling (+ width 17) char-width)))
                    (append widths nil))
            pixel-rows))))

(defun sql-grid--text-column-widths (columns rows headings)
  "Return character widths for COLUMNS and ROWS using HEADINGS."
  (mapcar
   (lambda (index)
     (let ((widest (string-width (nth index headings))))
       (dolist (row rows)
         (setq widest
               (max widest (string-width
                            (sql-grid--display-value (nth index row))))))
       widest))
   (number-sequence 0 (1- (length columns)))))

(defun sql-grid--grid-text-pixel-width (text frame font-face)
  "Return native pixel width of TEXT using FONT-FACE on FRAME."
  (with-selected-frame frame
    (string-pixel-width (propertize text 'face font-face))))

(defun sql-grid--grid-space (width face &optional height)
  "Insert a FACE space WIDTH pixels wide, optionally HEIGHT high."
  (insert (propertize " " 'face face
                      'display `(space :width (,width)
                                       ,@(when height `(:height (,height)))))))

(defun sql-grid--grid-border (width)
  "Insert a horizontal grid border WIDTH pixels wide."
  (let ((face `(:background ,(sql-grid--face-color
                              'shadow :foreground "#8792a2")
                            :height 1)))
    (insert (propertize " " 'face face
                        'display `(space :width (,width) :height (1))
                        'line-height 1)
            (propertize "\n" 'face face 'line-height 1))))

(defun sql-grid--grid-row (values display-values pixel-widths widths row-index header-p
                                   set-index order frame font-face stripe-index)
  "Insert native grid VALUES using prepared DISPLAY-VALUES and PIXEL-WIDTHS.
WIDTHS determine columns. Tag each row in SET-INDEX using its original identity."
  (let* ((background (if header-p
                         (sql-grid--face-color 'header-line :background "#d9e2f2")
                       (if (= (% (or stripe-index row-index) 2) 0)
                           (sql-grid--face-color 'default :background "#ffffff")
                         (sql-grid--face-color 'secondary-selection :background "#eef2f7"))))
         (foreground (sql-grid--face-color 'default :foreground "#202020"))
         (face (append font-face
                       (list :background background :foreground foreground)))
         (border-face `(:background ,(sql-grid--face-color
                                      'shadow :foreground "#8792a2")))
         (row-height
          (max 1 (ceiling (* (max 42 (+ 16 (frame-char-height frame)))
                             (/ (float (plist-get font-face :height))
                                (face-attribute 'default :height frame t)))))))
    (sql-grid--grid-space 1 border-face)
    (cl-loop for value in values
             for display-value in display-values
             for pixels in pixel-widths
             for width in widths
             for index from 0 do
             (let* ((full-value (if (null value) "NULL" (format "%s" value)))
                    (start (point))
                    (padding (max 1 (- width pixels 9)))
                    (shown (if (string-empty-p full-value) " " full-value)))
               (sql-grid--grid-space 8 face row-height)
               (insert (propertize shown 'face face
                                   'display (if (string-empty-p full-value)
                                                ""
                                              (unless (equal shown display-value)
                                                display-value))))
               (sql-grid--grid-space padding face)
               (sql-grid--grid-space 1 border-face)
               (sql-grid--make-cell-button
                start value full-value (list set-index row-index
                                             (nth index order) nil))))
    (insert "\n")
    (sql-grid--grid-border (1+ (apply #'+ widths)))))

(defvar sql-grid--cell-map
  (let ((map (copy-keymap button-map)))
    (define-key map (kbd "M-w") #'sql-grid-copy-cell)
    (define-key map (kbd "y") #'sql-grid-copy-cell)
    (define-key map (kbd "<return>") #'sql-grid-view-cell)
    (define-key map (kbd "TAB") #'sql-grid-next-cell)
    map))

(defvar sql-grid--header-map
  (let ((map (copy-keymap sql-grid--cell-map)))
    (define-key map (kbd "RET") #'sql-grid-sort-column)
    (define-key map (kbd "<return>") #'sql-grid-sort-column)
    map))

(defun sql-grid--sort-header (button)
  "Sort the column identified by header BUTTON."
  (goto-char (button-start button))
  (sql-grid-sort-column))

(defun sql-grid--make-cell-button (start value full-value &optional coordinate)
  "Make text from START inspectable as VALUE at result cell COORDINATE."
  (make-text-button
   start (point)
   ;; Distinct identities keep adjacent grid cells separate buttons.
   'button start
   'action (if (and coordinate (zerop (cadr coordinate)))
               #'sql-grid--sort-header
             #'sql-grid-view-cell)
   'follow-link t
   'keymap (if (and coordinate (zerop (cadr coordinate)))
               sql-grid--header-map
             sql-grid--cell-map)
   'mouse-face 'highlight
   'rear-nonsticky t
   'sql-grid-value value
   'sql-grid-cell coordinate
   'help-echo (and sql-grid-show-cell-tooltips full-value)))

(defun sql-grid--insert-text-cell (value &optional display-value coordinate)
  "Insert searchable VALUE, showing DISPLAY-VALUE at cell COORDINATE."
  (let ((full-value (if (null value) "NULL" (format "%s" value)))
        (start (point)))
    (insert (if (string-empty-p full-value) " " full-value))
    (when (or display-value (string-empty-p full-value))
      (put-text-property start (point) 'display (or display-value "")))
    (sql-grid--make-cell-button start value full-value coordinate)))

(defun sql-grid--ensure-cell-point ()
  "Keep grid point inside a cell, leaving valid interior positions intact.
Prefer a cell on the current row; otherwise use the next or final cell.
Text results and buffers without cells retain normal text motion."
  (when (and (eq sql-grid--result-format 'grid)
             (not (button-at (point))))
    (let* ((next (next-button (point) t))
           (previous (or (and (> (point) (point-min))
                              (button-at (1- (point))))
                         (previous-button (point))))
           (target
            (cond
             ((and next (<= (button-start next) (line-end-position))) next)
             ((and previous (>= (button-start previous) (line-beginning-position)))
              previous)
             (next next)
             (t previous))))
      (when target
        (goto-char (button-start target))))))

(defun sql-grid--align-grid-cursor (window)
  "Highlight WINDOW's grid cell, preserving valid interior text positions."
  (when (or (eq window (selected-window))
            (and (active-minibuffer-window)
                 (eq window (minibuffer-selected-window))))
    (sql-grid--ensure-cell-point)
    (let* ((position (window-point window))
           (cell (and (eq sql-grid--result-format 'grid)
                      (button-at position))))
      (if cell
          (progn
            (when (and sql-grid--saved-cursor-type cursor-type)
              (setcar sql-grid--saved-cursor-type cursor-type))
            (unless sql-grid--saved-cursor-type
              (setq sql-grid--saved-cursor-type (list cursor-type)))
            (setq-local cursor-type nil)
            (unless (and (overlayp sql-grid--cursor-overlay)
                         (overlay-buffer sql-grid--cursor-overlay))
              (setq sql-grid--cursor-overlay
                    (make-overlay (button-start cell) (button-end cell)))
              (overlay-put sql-grid--cursor-overlay 'evaporate t))
            (move-overlay sql-grid--cursor-overlay
                          (button-start cell) (button-end cell))
            (overlay-put sql-grid--cursor-overlay 'window window)
            (overlay-put
             sql-grid--cursor-overlay 'face
             `(:box (:line-width -2
				 :color ,(sql-grid--face-color 'cursor :background "#ff4080")))))
        (when (overlayp sql-grid--cursor-overlay)
          (delete-overlay sql-grid--cursor-overlay))
        (when (and sql-grid--saved-cursor-type cursor-type)
          (setcar sql-grid--saved-cursor-type cursor-type))
        (when sql-grid--saved-cursor-type
          (setq-local cursor-type (car sql-grid--saved-cursor-type))
          (setq sql-grid--saved-cursor-type nil))))))
(defun sql-grid--insert-text-border (widths)
  "Insert a horizontal border using character WIDTHS."
  (insert "+-"
          (mapconcat (lambda (width) (make-string width ?-)) widths "-+-")
          "-+\n"))

(defun sql-grid--insert-text-row (values widths row-index set-index order
                                          &optional headings)
  "Insert VALUES using WIDTHS, tagging ROW-INDEX in SET-INDEX with ORDER.
Use optional HEADINGS as display labels without changing cell values."
  (let ((start (point))
        (last-index (1- (length widths))))
    (insert "| ")
    (cl-loop for value in values
             for width in widths
             for index from 0 do
             (let ((display-value (if headings (nth index headings)
                                    (sql-grid--display-value value))))
               (sql-grid--insert-text-cell
                value display-value
                (list set-index row-index (nth index order) nil))
               (insert (make-string (- width (string-width display-value)) ?\s))
               (insert (if (= index last-index) " |\n" " | "))))
    (when (zerop row-index)
      (add-face-text-property start (point) 'bold t))))

(defun sql-grid--ordered-values (values order)
  "Return VALUES in original-column index ORDER without changing VALUES."
  (mapcar (lambda (index) (nth index values)) order))

(defun sql-grid--visible-row-entries (result)
  "Return (original row number . values) entries matching the row filter."
  (let ((case-fold-search t))
    (cl-loop for row in (sql-grid-result-rows result)
             for index from 1
             when (or (null sql-grid--row-filter)
                      (cl-some
                       (lambda (value)
                         (string-match-p sql-grid--row-filter
                                         (if (null value) "NULL" (format "%s" value))))
                       row))
             collect (cons index row))))

(defun sql-grid--sort-value-less-p (left right)
  "Compare original LEFT and RIGHT values in a consistent ascending order.
NULL precedes numbers, which precede text.  Text compares case-sensitively."
  (cond ((null left) (not (null right)))
        ((null right) nil)
        ((numberp left) (if (numberp right) (< left right) t))
        ((numberp right) nil)
        (t (string-lessp (format "%s" left) (format "%s" right)))))

(defun sql-grid--sort-specifications (set-index)
  "Return sort specifications for SET-INDEX."
  (nth set-index sql-grid--sorts))

(defun sql-grid--sorted-row-entries (result set-index)
  "Return filtered, stably sorted entries for RESULT at SET-INDEX.
Never modify retained rows or their original row numbers."
  (let ((entries (sql-grid--visible-row-entries result))
        (sorts (sql-grid--sort-specifications set-index)))
    (if (null sorts) entries
      (cl-stable-sort
       entries
       (lambda (left right)
         (catch 'decided
           (dolist (sort sorts)
             (let ((a (nth (car sort) (cdr left)))
                   (b (nth (car sort) (cdr right))))
               (cond
                ((sql-grid--sort-value-less-p a b)
                 (throw 'decided (eq (cdr sort) 'asc)))
                ((sql-grid--sort-value-less-p b a)
                 (throw 'decided (eq (cdr sort) 'desc)))))
           nil)))))))

(defun sql-grid--column-headings (columns order set-index)
  "Return display labels for COLUMNS in ORDER, marking active sorts."
  (let ((sorts (sql-grid--sort-specifications set-index)))
    (cl-loop for index in order
             for rank = (cl-position index sorts :key #'car)
             collect (concat
                      (sql-grid--display-value (nth index columns))
                      (when rank
                        (format " %s%d"
                                (if (eq (cdr (nth rank sorts)) 'asc) "↑" "↓")
                                (1+ rank)))))))

(defun sql-grid--page-row-entries (entries set-index)
  "Return the current page of filtered ENTRIES for SET-INDEX."
  (if (null sql-grid--page-size)
      entries
    (let ((start (min (length entries)
                      (* (nth set-index sql-grid--pages) sql-grid--page-size))))
      (cl-subseq entries start (min (length entries) (+ start sql-grid--page-size))))))

(defun sql-grid--insert-result-table (columns rows format set-index order &optional row-numbers)
  "Render COLUMNS and ROWS in FORMAT for SET-INDEX using column ORDER.
ROW-NUMBERS preserves original row identities when filtering."
  (let* ((headings (sql-grid--column-headings columns order set-index))
         (columns (sql-grid--ordered-values columns order))
         (rows (mapcar (lambda (row) (sql-grid--ordered-values row order)) rows))
         (row-numbers (or row-numbers (number-sequence 1 (length rows))))
         (header-start (point)))
    (pcase format
      ('grid
       (let* ((window (or (get-buffer-window (current-buffer) t)
                          (selected-window)))
              (frame (window-frame window))
              (font-face (sql-grid--grid-font-face frame))
              (display-rows
               (cons headings
                     (mapcar (lambda (row)
                               (mapcar #'sql-grid--display-value row))
                             rows)))
              (metrics (sql-grid--column-widths display-rows frame font-face))
              (widths (car metrics))
              (pixel-rows (cdr metrics)))
         (sql-grid--grid-row columns (car display-rows) (car pixel-rows)
                             widths 0 t set-index order frame font-face nil)
         (sql-grid--remember-column-name-line set-index header-start)
         (cl-loop for row in rows
                  for display-row in (cdr display-rows)
                  for pixel-row in (cdr pixel-rows)
                  for index in row-numbers
                  for stripe-index from 1 do
                  (sql-grid--grid-row row display-row pixel-row widths index
                                      nil set-index order frame font-face
                                      stripe-index))))
      ('text
       (let ((widths (sql-grid--text-column-widths columns rows headings)))
         (sql-grid--insert-text-border widths)
         (sql-grid--insert-text-row columns widths 0 set-index order headings)
         (sql-grid--remember-column-name-line set-index header-start)
         (sql-grid--insert-text-border widths)
         (cl-loop for row in rows for index in row-numbers do
                  (sql-grid--insert-text-row row widths index set-index order))
         (when rows (sql-grid--insert-text-border widths)))))
    (unless rows (insert "(0 rows)\n"))))

(defun sql-grid--remember-column-name-line (set-index start)
  "Move the just-inserted heading at START into the fixed bar for SET-INDEX."
  (when sql-grid-freeze-column-names
    (save-excursion
      (forward-line (if (eq sql-grid--result-format 'text) -1 -2))
      (let ((position (line-beginning-position))
            (end (line-end-position))
            chunks)
        (while (< position end)
          (let* ((next (next-single-property-change position 'display nil end))
                 (display (get-text-property position 'display)))
            (push (if (stringp display)
                      (propertize display 'face (get-text-property position 'face))
                    (buffer-substring position next))
                  chunks)
            (setq position next)))
        (let ((line (apply #'concat (nreverse chunks))))
          ;; Header strings are display-only, not buffer-position buttons.
          (remove-text-properties
           0 (length line)
           '(button nil category nil keymap nil action nil mouse-face nil) line)
          (setf (nth set-index sql-grid--column-name-lines) line))))
    (delete-region start (point))))

(defun sql-grid--frozen-column-names ()
  "Return current headings, clipped to the displaying window's scroll."
  (let* ((line (nth (sql-grid--copy-set-index) sql-grid--column-name-lines))
         (window (selected-window))
         (scroll (window-hscroll window)))
    (when line
      (let* ((graphic (display-graphic-p (window-frame window)))
             (pixels (* scroll (frame-char-width (window-frame window))))
             (low 0)
             (high (length line))
             (width 0))
        (if (not graphic)
            (setq line (truncate-string-to-width line (string-width line) scroll))
          ;; Find the first visible character without scanning every prefix.
          (while (< low high)
            (let* ((middle (/ (+ low high 1) 2))
                   (size (string-pixel-width (substring line 0 middle))))
              (if (<= size pixels)
                  (setq low middle width size)
                (setq high (1- middle)))))
          (let ((rest (substring line low)))
            (when (and (> pixels width) (< low (length line)))
              (setq rest
                    (concat
                     (propertize " " 'display
                                 `(space :width
                                         (,(- (string-pixel-width
                                               (substring line 0 (1+ low)))
                                              pixels))))
                     (substring line (1+ low)))))
            (setq line
                  (concat
                   (propertize " " 'display
                               `(space :width
                                       (,(+ (or (car (window-fringes window)) 0)
                                            (* (or (car (window-margins window)) 0)
                                               (frame-char-width
                                                (window-frame window)))))))
                   rest))))
        (replace-regexp-in-string "%" "%%" line t t)))))



(defun sql-grid-view-cell (&optional button)
  "Show the full raw value of BUTTON's cell using `sql-grid-cell-display'.
At a cell's end, inspect that cell so search matches remain inspectable."
  (interactive)
  (let* ((cell (or button (sql-grid--cell-at-point)))
         (value (and cell (button-get cell 'sql-grid-value))))
    (unless cell (user-error "Point is not on a result cell"))
    (let ((text (if (null value) "NULL" (format "%s" value))))
      (if (eq sql-grid-cell-display 'minibuffer)
          (message "%s" text)
        (let ((buffer (get-buffer-create "*sql-grid cell value*")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert text)
              (special-mode)))
          (pop-to-buffer buffer))))))

(defun sql-grid-copy-cell ()
  "Copy the cell's full original value, or selected cells as plain text."
  (interactive)
  (if (sql-grid--selection-active-p)
      (sql-grid-copy-selection)
    (let ((cell (sql-grid--cell-at-point)))
      (unless cell (user-error "Point is not on a result cell"))
      (sql-grid--copy-values "cell value"
                              (list (button-get cell 'sql-grid-value))))))

(defun sql-grid-activate-cell ()
  "Run the action of the result cell at point.
Header cells sort; data cells inspect their full original value."
  (interactive)
  (let ((cell (sql-grid--cell-at-point)))
    (unless cell (user-error "Point is not on a result cell"))
    (push-button (button-start cell))))

(defun sql-grid--preserve-evil-search-point ()
  "Keep Evil's search match inside the full text behind a grid cell."
  (when (and (eq sql-grid--result-format 'grid)
             (symbolp this-command)
             (or (memq this-command '(isearch-exit exit-minibuffer))
                 (string-prefix-p "evil-search-" (symbol-name this-command))
                 (string-prefix-p "evil-ex-search-" (symbol-name this-command))))
    ;; Otherwise the command loop snaps an interior image position forward
    ;; into the next cell.  Leave native adjustment intact for other motion.
    ;; Recursive search returns with its exit command as `this-command'.
    (setq disable-point-adjustment t)))

(defun sql-grid--evil-bind-result-keys ()
  "Use cell navigation and plain-text selection copying in Evil results."
  (when (and (featurep 'evil)
             (fboundp 'evil-local-set-key)
             (local-variable-p 'sql-grid--results))
    (add-hook 'post-command-hook #'sql-grid--preserve-evil-search-point nil t)
    (dolist (command '(sql-grid-next-cell sql-grid-previous-cell
					   sql-grid-next-row sql-grid-previous-row))
      (funcall #'evil-declare-motion command))
    (dolist (binding '(("M-f" . sql-grid-next-cell)
                       ("TAB" . sql-grid-next-cell)
                       ("<backtab>" . sql-grid-previous-cell)
                       ("M-w" . sql-grid-copy-cell)
                       ("y" . sql-grid-copy-cell)
                       ("RET" . sql-grid-activate-cell)
                       ("<return>" . sql-grid-activate-cell)
                       ("s" . sql-grid-sort-column)
                       ("S" . sql-grid-add-sort-column)
                       ("e" . sql-grid-export-csv)
                       ("]" . sql-grid-next-page)
                       ("[" . sql-grid-previous-page)))
      (funcall #'evil-local-set-key 'normal (kbd (car binding)) (cdr binding)))
    (dolist (binding '(("M-w" . sql-grid-copy-selection)
                       ("y" . sql-grid-copy-selection)
                       ("TAB" . sql-grid-next-cell)
                       ("<backtab>" . sql-grid-previous-cell)
                       ("M-f" . sql-grid-next-cell)))
      (funcall #'evil-local-set-key 'visual (kbd (car binding)) (cdr binding)))
    ;; Both states move by cell in the grid; text keeps native Evil motion.
    (dolist (state '(normal visual))
      (dolist (binding '(("<right>" . sql-grid-next-cell)
                         ("C-f" . sql-grid-next-cell)
                         ("l" . sql-grid-next-cell)
                         ("<left>" . sql-grid-previous-cell)
                         ("C-b" . sql-grid-previous-cell)
                         ("h" . sql-grid-previous-cell)
                         ("<down>" . sql-grid-next-row)
                         ("C-n" . sql-grid-next-row)
                         ("j" . sql-grid-next-row)
                         ("<up>" . sql-grid-previous-row)
                         ("C-p" . sql-grid-previous-row)
                         ("k" . sql-grid-previous-row)))
        (funcall #'evil-local-set-key state (kbd (car binding))
                 (and (eq sql-grid--result-format 'grid) (cdr binding)))))))

(with-eval-after-load 'evil
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (sql-grid--evil-bind-result-keys))))



(defun sql-grid-next-cell ()
  "Move point to the next result cell."
  (interactive)
  (let* ((cell (sql-grid--cell-at-point))
         (next (next-button (if cell (button-end cell) (point)) t)))
    (when next
      (goto-char (button-start next)))))

(defun sql-grid-previous-cell ()
  "Move point to the previous result cell."
  (interactive)
  (let* ((cell (sql-grid--cell-at-point))
         (previous
          (previous-button (if cell (button-start cell) (point)))))
    (when previous
      (goto-char (button-start previous)))))

(defun sql-grid--cell-at-point ()
  "Return the result button at point, including a cell's trailing boundary."
  (or (button-at (point))
      (and (> (point) (point-min)) (button-at (1- (point))))))

(defun sql-grid--move-vertically (direction)
  "Move to adjacent visible grid cell in the same column by DIRECTION."
  (let ((cell (and (eq sql-grid--result-format 'grid)
                   (sql-grid--cell-at-point))))
    (if (not cell)
        (line-move direction)
      (let* ((coordinate (button-get cell 'sql-grid-cell))
             (set-index (car coordinate))
             (column (nth 2 coordinate))
             (label (nth 3 coordinate))
             (candidate (if (> direction 0)
                            (next-button (button-end cell) t)
                          (previous-button (button-start cell))))
             target
             done)
        (while (and candidate (not target) (not done))
          (let ((next-coordinate (button-get candidate 'sql-grid-cell)))
            (cond
             ((not next-coordinate)
              (setq done t))
             ((not (equal (car next-coordinate) set-index))
              (setq done t))
             ((and (equal (nth 2 next-coordinate) column)
                   (equal (nth 3 next-coordinate) label))
              (setq target candidate)))
            (unless (or target done)
              (setq candidate
                    (if (> direction 0)
                        (next-button (button-end candidate) t)
                      (previous-button (button-start candidate)))))))
        (when target
          (goto-char (button-start target)))))))

(defun sql-grid-next-row ()
  "Move down one visible result row."
  (interactive)
  (sql-grid--move-vertically 1))

(defun sql-grid-previous-row ()
  "Move up one visible result row."
  (interactive)
  (sql-grid--move-vertically -1))

(defun sql-grid--current-cell-coordinate ()
  "Return the current cell's set, row, original column, and label flag."
  (let* ((cell (sql-grid--cell-at-point))
         (coordinate (and cell (button-get cell 'sql-grid-cell))))
    (unless coordinate (user-error "Point is not on a result cell"))
    coordinate))

(defun sql-grid--restore-cell (coordinate offset)
  "Restore point to COORDINATE plus OFFSET after rendering.
Fall back to the same column when the new format has no matching heading."
  (let ((cell (next-button (point-min) t))
        match fallback)
    (while (and cell (not match))
      (let ((candidate (button-get cell 'sql-grid-cell)))
        (when (equal candidate coordinate) (setq match cell))
        (when (and (not fallback)
                   (equal (car candidate) (car coordinate))
                   (equal (nth 2 candidate) (nth 2 coordinate))
                   (or (equal (cadr candidate) (cadr coordinate))
                       (zerop (cadr coordinate))))
          (setq fallback cell)))
      (setq cell (next-button (button-end cell) t)))
    (when-let* ((target (or match fallback)))
      (goto-char (+ (button-start target)
                    (min offset (1- (- (button-end target)
                                       (button-start target)))))))))

(defun sql-grid--reveal-cell ()
  "Scroll horizontally to show the selected result cell in its window."
  (let* ((cell (sql-grid--cell-at-point))
         (selected (selected-window))
         (window (if (and (window-live-p selected)
                          (eq (window-buffer selected) (current-buffer)))
                     selected
                   (get-buffer-window (current-buffer)))))
    (when (and cell (window-live-p window))
      (let* ((start (button-start cell))
             (line-start (save-excursion (goto-char start) (line-beginning-position)))
             (left (car (window-text-pixel-size window line-start start t)))
             (right (car (window-text-pixel-size
                          window line-start (button-end cell) t)))
             (char-width (frame-char-width (window-frame window)))
             (scroll (* (window-hscroll window) char-width))
             (width (window-body-width window t)))
        (cond
         ((or (< left scroll) (> (- right left) width))
          (set-window-hscroll window (floor left char-width)))
         ((> right (+ scroll width))
          (set-window-hscroll window (ceiling (- right width) char-width))))))))

(defun sql-grid-move-column (position)
  "Move the column under point to one-based POSITION in its result set.
Keep original SQL results unchanged and retain point on the selected cell."
  (interactive
   (let* ((coordinate (sql-grid--current-cell-coordinate))
          (order (nth (car coordinate) sql-grid--column-orders)))
     (list (read-number
            (format "Move column to position (1-%d): " (length order))
            (1+ (cl-position (nth 2 coordinate) order))))))
  (let* ((coordinate (sql-grid--current-cell-coordinate))
         (set-index (car coordinate))
         (column (nth 2 coordinate))
         (order (nth set-index sql-grid--column-orders)))
    (unless (and (integerp position) (<= 1 position (length order)))
      (user-error "Column position must be an integer from 1 to %d" (length order)))
    (let ((remaining (remq column order)))
      (setf (nth set-index sql-grid--column-orders)
            (append (cl-subseq remaining 0 (1- position))
                    (list column) (nthcdr (1- position) remaining))))
    (sql-grid-refresh)
    (sql-grid--reveal-cell)))

(defun sql-grid-move-column-left ()
  "Move the column under point one position left without wrapping."
  (interactive)
  (let* ((coordinate (sql-grid--current-cell-coordinate))
         (order (nth (car coordinate) sql-grid--column-orders))
         (index (cl-position (nth 2 coordinate) order)))
    (when (zerop index) (user-error "Already at first column"))
    (sql-grid-move-column index)))

(defun sql-grid-move-column-right ()
  "Move the column under point one position right without wrapping."
  (interactive)
  (let* ((coordinate (sql-grid--current-cell-coordinate))
         (order (nth (car coordinate) sql-grid--column-orders))
         (index (cl-position (nth 2 coordinate) order)))
    (when (= index (1- (length order))) (user-error "Already at last column"))
    (sql-grid-move-column (+ index 2))))

(defun sql-grid-reset-column-order ()
  "Restore original SQL column order in the result set under point."
  (interactive)
  (let* ((coordinate (sql-grid--current-cell-coordinate))
         (set-index (car coordinate))
         (result (nth set-index sql-grid--results)))
    (setf (nth set-index sql-grid--column-orders)
          (number-sequence 0 (1- (length (sql-grid-result-columns result)))))
    (sql-grid-refresh)
    (sql-grid--reveal-cell)))

(defun sql-grid-display-results (connection results)
  "Render RESULTS from CONNECTION without re-executing SQL.
This is also the programmatic entry point for displaying stored results.
New result buffers use `sql-grid-result-format'; existing buffers keep
their selected format and temporary row-limit override.  New results reset
column order, filtering, sorting, and pages.
Use `sql-grid-max-rows' rows per page unless overridden locally; nil shows all.
Select the result window only when `sql-grid-focus-results' is non-nil."
  (sql-grid--display-results connection results nil))

(defun sql-grid--result-forward-char (&optional arg)
  "Move one character in text results or one cell in grid results."
  (interactive "^p")
  (if (eq sql-grid--result-format 'grid)
      (sql-grid-next-cell)
    (forward-char arg)))

(defun sql-grid--result-backward-char (&optional arg)
  "Move one character backward in text results or one cell in grid results."
  (interactive "^p")
  (if (eq sql-grid--result-format 'grid)
      (sql-grid-previous-cell)
    (backward-char arg)))


(defun sql-grid--result-next-line (&optional arg)
  "Move one line in text results or one row in grid results."
  (interactive "^p")
  (if (eq sql-grid--result-format 'grid)
      (sql-grid-next-row)
    (line-move (or arg 1))))

(defun sql-grid--result-previous-line (&optional arg)
  "Move one line backward in text results or one row in grid results."
  (interactive "^p")
  (if (eq sql-grid--result-format 'grid)
      (sql-grid-previous-row)
    (line-move (- (or arg 1)))))

(defvar sql-grid-result-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-c C-k") #'sql-grid-cancel)
    (define-key map (kbd "C-c C-f") #'sql-grid-filter-rows)
    (define-key map (kbd "C-c C-v") #'sql-grid-set-result-format)
    (define-key map (kbd "C-c C-l") #'sql-grid-set-max-rows)
    (define-key map (kbd "f") #'sql-grid-set-result-format)
    (define-key map (kbd "g") #'sql-grid-refresh)
    (define-key map (kbd "/") #'sql-grid-filter-rows)
    (define-key map (kbd "s") #'sql-grid-sort-column)
    (define-key map (kbd "S") #'sql-grid-add-sort-column)
    (define-key map (kbd "M-w") #'sql-grid-copy-cell)
    (define-key map (kbd "y") #'sql-grid-copy-cell)
    (define-key map (kbd "e") #'sql-grid-export-csv)
    (define-key map (kbd "]") #'sql-grid-next-page)
    (define-key map (kbd "[") #'sql-grid-previous-page)
    (define-key map (kbd "q") #'sql-grid-quit)
    (define-key map (kbd "c") #'sql-grid-copy-row)
    (define-key map (kbd "C") #'sql-grid-copy-column)
    (define-key map (kbd "RET") #'sql-grid-view-cell)
    (define-key map (kbd "<return>") #'sql-grid-view-cell)
    (define-key map (kbd "TAB") #'sql-grid-next-cell)
    (define-key map (kbd "<backtab>") #'sql-grid-previous-cell)
    (define-key map (kbd "M-f") #'sql-grid-next-cell)
    (define-key map (kbd "<right>") #'sql-grid--result-forward-char)
    (define-key map (kbd "<left>") #'sql-grid--result-backward-char)
    (define-key map (kbd "C-f") #'sql-grid--result-forward-char)
    (define-key map (kbd "C-b") #'sql-grid--result-backward-char)
    (define-key map (kbd "<down>") #'sql-grid--result-next-line)
    (define-key map (kbd "<up>") #'sql-grid--result-previous-line)
    (define-key map (kbd "C-n") #'sql-grid--result-next-line)
    (define-key map (kbd "C-p") #'sql-grid--result-previous-line)
    (define-key map [remap next-line] #'sql-grid--result-next-line)
    (define-key map [remap previous-line] #'sql-grid--result-previous-line)
    (define-key map (kbd "M-<left>") #'sql-grid-move-column-left)
    (define-key map (kbd "M-<right>") #'sql-grid-move-column-right)
    (define-key map (kbd "M-h") #'sql-grid-move-column-left)
    (define-key map (kbd "M-l") #'sql-grid-move-column-right)
    (define-key map (kbd "m") #'sql-grid-move-column)
    map)
  "Keymap for `sql-grid-result-mode'.")

(define-derived-mode sql-grid-result-mode special-mode "SQL-Grid-Results"
  "Major mode for retained SQL results."
  (sql-grid--setup-selection)
  (add-hook 'post-command-hook #'sql-grid--ensure-cell-point t t))

(defun sql-grid--display-results (connection results redraw)
  "Display CONNECTION's RESULTS, preserving view state only for REDRAW."
  (let* ((source (current-buffer))
         (previous (and (buffer-live-p sql-grid--result-buffer)
                        sql-grid--result-buffer))
         (format (or (and previous
                          (buffer-local-value 'sql-grid--result-format previous))
                     sql-grid-result-format))
         (cell (and redraw (sql-grid--cell-at-point)))
         (coordinate (and cell (button-get cell 'sql-grid-cell)))
         (offset (and cell (- (point) (button-start cell))))
         (position (point))
         buffer)
    (sql-grid--require-result-display format)
    (unless redraw (sql-grid--validate-max-rows))
    (setq buffer (or previous (generate-new-buffer "*sql-grid results*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'sql-grid-result-mode)
        (sql-grid-result-mode))
      (sql-grid--clear-selection)
      (deactivate-mark)
      (add-hook 'text-scale-mode-hook #'sql-grid--rescale-results nil t)
      (add-hook 'pre-redisplay-functions #'sql-grid--align-grid-cursor nil t)
      (unless redraw
        (setq-local sql-grid--row-filter nil)
        (setq-local sql-grid--page-size
                    (if sql-grid--max-rows-override-set
                        sql-grid--max-rows-override
                      sql-grid-max-rows))
        (setq-local sql-grid--pages (make-list (length results) 0))
        (setq-local sql-grid--sorts (make-list (length results) nil))
        (setq-local sql-grid--column-orders
                    (mapcar (lambda (result)
                              (number-sequence
                               0 (1- (length (sql-grid-result-columns result)))))
                            results)))
      (setq-local sql-grid--result-buffer buffer)
      (setq-local sql-grid--connection connection)
      (unless redraw
        (setq-local sql-grid--execution-source source))
      (setq-local sql-grid--results results)
      (setq-local sql-grid--result-format format)
      (setq-local sql-grid--status-lines nil)
      (setq-local sql-grid--column-name-lines
                  (when sql-grid-freeze-column-names
                    (make-list (length results) nil)))
      (if sql-grid-freeze-column-names
          (progn
            (unless sql-grid--saved-tab-line-format
              (setq sql-grid--saved-tab-line-format (list tab-line-format)))
            (setq-local tab-line-format '(:eval (sql-grid--header-line))))
        (when sql-grid--saved-tab-line-format
          (setq-local tab-line-format (car sql-grid--saved-tab-line-format))
          (setq sql-grid--saved-tab-line-format nil)))
      (setq-local header-line-format
                  (if sql-grid-freeze-column-names
                      '(:eval (sql-grid--frozen-column-names))
                    '(:eval (sql-grid--header-line))))
      (sql-grid--evil-bind-result-keys)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local truncate-lines t)
        (if (null results)
            (insert "No result sets.\n")
          (cl-loop for result in results for set-index from 1 do
                   (let* ((columns (sql-grid-result-columns result))
                          (entries (sql-grid--sorted-row-entries result (1- set-index)))
                          (total (length entries))
                          (page (nth (1- set-index) sql-grid--pages))
                          (entries (sql-grid--page-row-entries entries (1- set-index)))
                          (rows (mapcar #'cdr entries))
                          (start (point)))
                     (when (> (length results) 1)
                       (insert (format "Result set %d\n" set-index)))
                     (push
                      (format "%sRecords: %d%s%s | Page %d/%d"
                              (if (> (length results) 1)
                                  (format "Set %d/%d | " set-index (length results)) "")
                              (length (sql-grid-result-rows result))
                              (if sql-grid--row-filter
                                  (format " | Matched: %d" total)
                                "")
                              (if sql-grid--row-filter
                                  (format " | Filter: %s"
                                          (replace-regexp-in-string
                                           "[\n\r\t]" " " sql-grid--row-filter))
                                "")
                              (1+ page)
                              (if sql-grid--page-size
                                  (max 1 (ceiling total sql-grid--page-size)) 1))
                      sql-grid--status-lines)
                     (if columns
                         (sql-grid--insert-result-table
                          columns rows format (1- set-index)
                          (nth (1- set-index) sql-grid--column-orders)
                          (mapcar #'car entries))
                       (insert
                        (format "%s\n"
                                (or (sql-grid-result-message result)
                                    (if (or (null connection)
                                            (memq (sql-grid-connection-backend connection)
                                                  '(mysql mariadb)))
                                        "0 rows (column metadata unavailable)."
                                      "Statement completed (no rows returned).")))))
                     (insert "\n")
                     (put-text-property start (point) 'sql-grid-result-set (1- set-index))))))
      (setq sql-grid--status-lines (nreverse sql-grid--status-lines))
      (if redraw
          (progn
            (goto-char (min position (point-max)))
            (when coordinate (sql-grid--restore-cell coordinate offset)))
        (when-let* ((first (next-button (point-min) t)))
          (goto-char (button-start first)))))
    (when (buffer-live-p source)
      (with-current-buffer source (setq sql-grid--result-buffer buffer)))
    (sql-grid--show-result-buffer buffer sql-grid-focus-results)
    (unless redraw
      (let ((position (with-current-buffer buffer (point))))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window position))))
    (when (and (eq buffer (window-buffer (selected-window)))
               (eq (buffer-local-value 'sql-grid--result-format buffer) 'grid))
      (with-current-buffer buffer
        (sql-grid--align-grid-cursor (selected-window))))
    buffer))

(defun sql-grid-set-max-rows (limit)
  "Set this result buffer's LIMIT without executing SQL.
LIMIT must be nil or a positive integer.  The override persists for
refreshes and later results in this buffer."
  (interactive
   (list
    (let ((input (read-string "Maximum rows per page (positive count or all): ")))
      (if (string-equal (downcase input) "all")
          nil
        (let ((number (string-to-number input)))
          (unless (and (string-match-p "\\`[0-9]+\\'" input) (> number 0))
            (user-error "Enter a positive count or all"))
          number)))))
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (unless (or (null limit) (and (integerp limit) (> limit 0)))
    (user-error "Maximum rows must be nil or a positive integer"))
  (setq-local sql-grid--max-rows-override limit
              sql-grid--max-rows-override-set t
              sql-grid--page-size limit
              sql-grid--pages (make-list (length sql-grid--results) 0))
  (sql-grid-refresh))

(defun sql-grid-set-result-format (format)
  "Switch this result buffer to FORMAT without executing SQL again.
Choose `grid' or `text'.  The choice is local to this result buffer
and survives refresh and subsequent executions from its source."
  (interactive
   (list (intern (completing-read
                  "Result format: " '("grid" "text") nil t nil nil
                  (symbol-name (or sql-grid--result-format
                                   sql-grid-result-format))))))
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (sql-grid--require-result-display format)
  (setq-local sql-grid--result-format format)
  (sql-grid-refresh))

(defun sql-grid-refresh ()
  "Rerender the retained result data; never rerun SQL."
  (interactive)
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (sql-grid--display-results sql-grid--connection sql-grid--results t))

(defun sql-grid--change-page (direction)
  "Move the result set under point by DIRECTION pages without executing SQL."
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (unless sql-grid--page-size
    (user-error "Pagination is disabled for these results"))
  (let* ((cell (sql-grid--cell-at-point))
         (coordinate (and cell (button-get cell 'sql-grid-cell)))
         (offset (and cell (- (point) (button-start cell))))
         (set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results))
         (entries (and result (sql-grid--sorted-row-entries result set-index)))
         (old-page-entries (and entries (sql-grid--page-row-entries entries set-index)))
         (row-rank (and coordinate
                        (> (cadr coordinate) 0)
                        (cl-position (cadr coordinate)
                                     (mapcar #'car old-page-entries))))
         (total (if result (length (sql-grid--visible-row-entries result)) 0))
         (page (+ (or (nth set-index sql-grid--pages) 0) direction))
         (count (max 1 (ceiling total sql-grid--page-size))))
    (unless (and (>= page 0) (< page count))
      (user-error (if (< page 0) "Already on the first page" "Already on the last page")))
    (setf (nth set-index sql-grid--pages) page)
    (sql-grid-refresh)
    (when coordinate
      (let* ((new-entries (sql-grid--page-row-entries entries set-index))
             (row-index
              (if (zerop (cadr coordinate))
                  0
                (if new-entries
                    (car (nth (min (or row-rank 0) (1- (length new-entries)))
                              new-entries))
                  0))))
        (sql-grid--restore-cell
         (list set-index row-index (nth 2 coordinate) (nth 3 coordinate))
         offset)))
    (when-let* ((window (get-buffer-window (current-buffer)))
                (start (text-property-any
                        (point-min) (point-max) 'sql-grid-result-set set-index)))
      (set-window-start window start t))))

(defun sql-grid-next-page ()
  "Show the next page of the result set under point without executing SQL."
  (interactive)
  (sql-grid--change-page 1))

(defun sql-grid-previous-page ()
  "Show the previous page of the result set under point without executing SQL."
  (interactive)
  (sql-grid--change-page -1))

(defun sql-grid-filter-rows (regexp)
  "Show retained rows with a cell matching case-insensitive REGEXP.
Match full original values, not truncated display text.  Empty REGEXP
clears the filter.  Filtering never executes SQL."
  (interactive (list (read-string "Filter rows (regexp, empty to clear): " nil 'regexp-history)))
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (condition-case error-data
      (string-match-p regexp "")
    (invalid-regexp (user-error "Invalid row filter: %s" (error-message-string error-data))))
  (setq-local sql-grid--row-filter (unless (string-empty-p regexp) regexp))
  (setq-local sql-grid--pages (make-list (length sql-grid--results) 0))
  (sql-grid-refresh))

(defun sql-grid-sort-column (&optional column reset)
  "Sort by original COLUMN, replacing existing sort precedence.
Repeating the primary column toggles its direction.  Prefix RESET clears
all sort columns."
  (interactive (list nil current-prefix-arg))
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (let* ((set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results))
         (columns (and result (sql-grid-result-columns result)))
         (order (nth set-index sql-grid--column-orders))
         (cell (sql-grid--cell-at-point))
         (coordinate (and cell (button-get cell 'sql-grid-cell)))
         (previous (sql-grid--sort-specifications set-index)))
    (unless columns (user-error "No result columns to sort"))
    (setq column
          (or column (nth 2 coordinate)
              (unless reset
                (let* ((choices
                        (cl-loop for index in order for position from 1
                                 collect (cons (format "%s [%d]"
                                                       (nth index columns) position)
                                               index))))
                  (cdr (assoc (completing-read "Sort column: " choices nil t)
                              choices))))))
    (unless (or reset
                (and (integerp column) (<= 0 column) (< column (length columns))))
      (user-error "Invalid result column: %S" column))
    (setf (nth set-index sql-grid--sorts)
          (unless reset
            (list (cons column
                        (if (and (eq column (caar previous))
                                 (eq (cdar previous) 'asc))
                            'desc 'asc)))))
    (setf (nth set-index sql-grid--pages) 0)
    (sql-grid-refresh)
    (when (and column coordinate)
      (sql-grid--restore-cell (list set-index 0 column nil) 0))
    (sql-grid--reveal-cell)))

(defun sql-grid-add-sort-column (&optional column)
  "Append COLUMN as a secondary sort, or toggle its current direction."
  (interactive)
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (let* ((set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results))
         (columns (and result (sql-grid-result-columns result)))
         (order (nth set-index sql-grid--column-orders))
         (cell (sql-grid--cell-at-point))
         (coordinate (and cell (button-get cell 'sql-grid-cell)))
         (previous (sql-grid--sort-specifications set-index)))
    (unless columns (user-error "No result columns to sort"))
    (setq column
          (or column (nth 2 coordinate)
              (let* ((choices
                      (cl-loop for index in order for position from 1
                               collect (cons (format "%s [%d]"
                                                     (nth index columns) position)
                                             index))))
                (cdr (assoc (completing-read "Add sort column: " choices nil t)
                            choices)))))
    (unless (and (integerp column) (<= 0 column) (< column (length columns)))
      (user-error "Invalid result column: %S" column))
    (let ((current (assq column previous)))
      (setf (nth set-index sql-grid--sorts)
            (if current
                (mapcar (lambda (sort)
                          (if (eq (car sort) column)
                              (cons column (if (eq (cdr sort) 'asc) 'desc 'asc))
                            sort))
                        previous)
              (append previous (list (cons column 'asc))))))
    (setf (nth set-index sql-grid--pages) 0)
    (sql-grid-refresh)
    (sql-grid--restore-cell (list set-index 0 column nil) 0)
    (sql-grid--reveal-cell)))

(defun sql-grid--header-line ()
  "Return status and execution details for the result set under point."
  (let* ((set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results))
         (source (or sql-grid--execution-source (current-buffer)))
         (running (and (buffer-live-p source)
                       (eq (buffer-local-value 'sql-grid--execution-state source)
                           'running)))
         (elapsed (and result (not running)
                       (sql-grid-result-elapsed result)))
         (affected (and result (not running)
                        (sql-grid-result-affected-rows result))))
    (replace-regexp-in-string
     "%" "%%"
     (concat
      (when-let ((status (sql-grid--execution-status))) (concat status " | "))
      (when elapsed (format "Elapsed: %.3fs | " elapsed))
      (when (integerp affected)
        (format "Affected: %d | " affected))
      (or (nth set-index sql-grid--status-lines)
          "Records: 0 | Page 1/1"))
     t t)))

(defun sql-grid--rescale-results ()
  "Resize retained grid results after a buffer text-scale change."
  (when (and (local-variable-p 'sql-grid--results)
             (eq sql-grid--result-format 'grid))
    (sql-grid-refresh)))

(defun sql-grid--refresh-grid-font (&optional frame)
  "Refresh visible native grids after font metrics change on FRAME."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (local-variable-p 'sql-grid--results)
                 (eq sql-grid--result-format 'grid)
                 (or (null frame) (get-buffer-window buffer frame)))
        (sql-grid-refresh)))))

(add-hook 'after-setting-font-hook #'sql-grid--refresh-grid-font)

(defun sql-grid--copy-values (description values)
  (kill-new (mapconcat (lambda (value) (if (null value) "NULL" (format "%s" value))) values "\t"))
  (message "Copied full %s (untruncated)" description))

(defun sql-grid--copy-set-index ()
  "Return the result set under point, defaulting to the first set."
  (let ((cell (sql-grid--cell-at-point)))
    (or (and cell (car (button-get cell 'sql-grid-cell)))
        (get-text-property (point) 'sql-grid-result-set)
        0)))

(defun sql-grid-copy-row ()
  "Copy a selected row in visible column order from the current result set."
  (interactive)
  (let* ((set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results))
         (order (nth set-index sql-grid--column-orders))
         (rows (mapcar (lambda (row) (sql-grid--ordered-values row order))
                       (mapcar #'cdr (sql-grid--page-row-entries
                                      (sql-grid--sorted-row-entries result set-index) set-index)))))
    (unless rows (user-error "No result rows to copy"))
    (let* ((choices (cl-loop for row in rows for index from 1
                             collect (cons (format "Row %d: %s" index
                                                   (mapconcat #'sql-grid--display-value row " | "))
                                           row)))
           (row (cdr (assoc (completing-read "Copy row: " choices nil t) choices))))
      (sql-grid--copy-values "row" row))))

(defun sql-grid-copy-column ()
  "Copy a selected column's full values from visible result rows."
  (interactive)
  (let* ((set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results))
         (order (nth set-index sql-grid--column-orders))
         (columns (sql-grid-result-columns result))
         (rows (mapcar #'cdr (sql-grid--page-row-entries
                              (sql-grid--sorted-row-entries result set-index) set-index))))
    (unless columns (user-error "No result columns to copy"))
    (let* ((choices (cl-loop for original-index in order for position from 1
                             collect (cons (format "%s [%d]"
                                                   (nth original-index columns)
                                                   position)
                                           original-index)))
           (index (cdr (assoc (completing-read "Copy column: " choices nil t) choices))))
      (sql-grid--copy-values (nth index columns)
                              (mapcar (lambda (row) (nth index row)) rows)))))

(defun sql-grid--csv-field (value)
  "Encode VALUE as one RFC 4180 CSV field."
  (if (null value)
      ""
    (let* ((text (format "%s" value))
           (quoted (or (string-empty-p text)
                       (equal text "NULL")
                       (string-match-p "[,\"\r\n]" text))))
      (if quoted
          (concat "\"" (replace-regexp-in-string "\"" "\"\"" text t t) "\"")
        text))))

(defun sql-grid-export-csv (file)
  "Write the current retained result set to CSV FILE.
Export all filtered rows, in current sort and column order, without truncation."
  (interactive
   (list (read-file-name "Export CSV to: " nil nil nil)))
  (unless (local-variable-p 'sql-grid--results)
    (user-error "No retained SQL results"))
  (let* ((set-index (sql-grid--copy-set-index))
         (result (nth set-index sql-grid--results)))
    (unless result
      (user-error "No result set at point"))
    (let ((columns (sql-grid-result-columns result)))
      (unless columns
        (user-error "Result set has no column metadata"))
      (let* ((order (nth set-index sql-grid--column-orders))
             (rows (sql-grid--sorted-row-entries result set-index))
             (records
              (cons (mapconcat #'sql-grid--csv-field
                               (sql-grid--ordered-values columns order) ",")
                    (mapcar
                     (lambda (entry)
                       (mapconcat #'sql-grid--csv-field
                                  (sql-grid--ordered-values (cdr entry) order) ","))
                     rows)))
             (content (concat (mapconcat #'identity records "\r\n") "\r\n")))
        (when (and (called-interactively-p 'interactive)
                   (file-exists-p file)
                   (not (yes-or-no-p (format "Overwrite %s? " file))))
          (user-error "CSV export cancelled"))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil file nil 'silent))
        (message "Exported %d rows to %s" (length rows) file)))))

(defun sql-grid-quit ()
  "Close this result buffer while leaving its editor connection open."
  (interactive)
  (quit-window t))

(provide 'sql-grid)
;;; sql-grid.el ends here
