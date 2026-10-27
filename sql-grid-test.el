;;; sql-grid-test.el --- Tests for sql-grid -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Run backend tests with `emacs -Q --batch -L sql-grid -l sql-grid-test.el -f ert-run-tests-batch-and-exit'.
;; Graphical result-cell tests are skipped in batch mode; run the same command
;; in a graphical Emacs session (or under Xvfb with GDK_BACKEND=x11) to include them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sql-grid)
(require 'face-remap)

(defun sql-grid-test--with-sqlite (function)
  "Call FUNCTION with a connected temporary-file SQLite database."
  (let* ((database (make-temp-file "sql-grid-test-"))
         (connection (sql-grid-connection-create
                      :backend 'sqlite :database database)))
    (unwind-protect
        (progn
          (sql-grid-db-connect connection)
          (funcall function connection))
      (sql-grid-db-disconnect connection)
      (when (file-exists-p database) (delete-file database)))))

(defun sql-grid-test--with-memory-sqlite (function)
  "Call FUNCTION with a connected in-memory SQLite database.
Run in a temporary directory so a non-native fallback cannot touch the
repository's preexisting `:memory:' artifact."
  (let* ((directory (make-temp-file "sql-grid-memory-" t))
         (default-directory directory)
         (connection (sql-grid-connection-create
                      :backend 'sqlite :database ":memory:")))
    (unwind-protect
        (progn
          (sql-grid-db-connect connection)
          (funcall function connection))
      (sql-grid-db-disconnect connection)
      (delete-directory directory t))))

(defun sql-grid-test--wait-for (predicate &optional timeout)
  "Wait until PREDICATE is non-nil, or TIMEOUT seconds have elapsed.
PREDICATE runs in the current buffer after accepting process output."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun sql-grid-test--slow-query ()
  "Return a deliberately expensive read-only SQLite query.
The persistent SQLite worker evaluates it outside the main Emacs loop,
so tests can observe the running state and cancel it."
  (concat "WITH RECURSIVE counter(x) AS (VALUES (1) "
          "UNION ALL SELECT x + 1 FROM counter WHERE x < 100000000) "
          "SELECT count(*), sum(x) FROM counter"))

(defun sql-grid-test--await-results (buffer)
  "Wait at most ten seconds for BUFFER's source query to finish."
  (let* ((source (buffer-local-value 'sql-grid--execution-source buffer))
         (deadline (+ (float-time) 10)))
    (when (buffer-live-p source)
      (while (and (buffer-local-value 'sql-grid--execution-job source)
                  (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (when (buffer-local-value 'sql-grid--execution-job source)
        (ert-fail "SQL execution did not finish within ten seconds"))
      (when (eq (buffer-local-value 'sql-grid--execution-state source) 'failed)
        (ert-fail (buffer-local-value 'sql-grid--execution-error source))))))

(defun sql-grid-test--button-with-help (buffer help)
  "Return the position of a standard result button with consumer-visible HELP."
  (sql-grid-test--await-results buffer)
  (with-current-buffer buffer
    (let ((position (point-min)) found)
      (while (and (< position (point-max)) (not found))
        (when (and (button-at position)
                   (equal (get-text-property position 'help-echo) help))
          (setq found position))
        (setq position (1+ position)))
      found)))

(defun sql-grid-test--help-echo-at (buffer position)
  "Return visible help-echo text at POSITION in BUFFER."
  (with-current-buffer buffer
    (get-text-property position 'help-echo)))

(defun sql-grid-test--button-visible-label (buffer help)
  "Return the rendered label of BUFFER's button with consumer-visible HELP."
  (let ((position (sql-grid-test--button-with-help buffer help)))
    (when position
      (with-current-buffer buffer
        (let* ((button (button-at position))
               (end (button-end button))
               chunks)
          (while (< position end)
            (let* ((next (next-single-property-change position 'display nil end))
                   (display (get-text-property position 'display)))
              (cond
               ((stringp display) (push display chunks))
               ((null display)
                (push (buffer-substring-no-properties position next) chunks)))
              (setq position next)))
          (apply #'concat (nreverse chunks)))))))

(defun sql-grid-test--buffer-contains-p (buffer text)
  "Return non-nil when BUFFER contains TEXT, ignoring point position."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward text nil t))))

(defun sql-grid-test--messages-during (function)
  "Call FUNCTION and return messages emitted during it, oldest first."
  (let ((original (symbol-function 'message))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (when format-string
                   (push (if args
                             (apply #'format-message format-string args)
                           format-string)
                         messages))
                 (apply original format-string args))))
      (funcall function))
    (nreverse messages)))

(defun sql-grid-test--completion-matches (completion &optional prefix)
  "Return standard `all-completions' matches for CAPF COMPLETION.
Use PREFIX when non-nil instead of COMPLETION's buffer prefix."
  (when completion
    (all-completions
     (or prefix
         (buffer-substring-no-properties (nth 0 completion) (point)))
     (nth 2 completion))))


(defun sql-grid-test--press-in-buffer (buffer position key)
  "Select BUFFER, move to POSITION, and send KEY as a keyboard event."
  (let ((window (get-buffer-window buffer)))
    (should window)
    (select-window window)
    (goto-char position)
    (let ((noninteractive nil))
      (execute-kbd-macro (kbd key)))
    (current-buffer)))

(defun sql-grid-test--result-windows (buffer)
  "Return BUFFER's live windows ordered left to right."
  (sort (get-buffer-window-list buffer nil t)
        (lambda (a b)
          (< (car (window-edges a)) (car (window-edges b))))))

(defun sql-grid-test--result-window-width (buffer)
  "Return the combined total width of BUFFER's result windows."
  (let ((windows (sql-grid-test--result-windows buffer)))
    (if windows
        (apply #'+ (mapcar #'window-total-width windows))
      0)))

(ert-deftest sql-grid-test-result-split-direction-and-focus ()
  "Place new results beside the source and reuse their window on refresh."
  (dolist (direction '(right below left above))
    (dolist (focus '(nil t))
      (save-window-excursion
        (delete-other-windows)
        (let ((source (generate-new-buffer " *sql-grid-test split*"))
              (sql-grid-result-format 'text)
              (sql-grid-result-split-direction direction)
              (sql-grid-focus-results focus)
              buffer)
          (unwind-protect
              (progn
                (switch-to-buffer source)
                (insert "SELECT 1")
                (let ((source-window (selected-window))
                      (source-point (point)))
                  (setq buffer
                        (sql-grid-display-results
                         nil (list (sql-grid-result-create
                                    :columns '("id") :rows '((1))))))
                  (let* ((result-window (get-buffer-window buffer))
                         (source-edges (window-edges source-window))
                         (result-edges (window-edges result-window)))
                    (should (eq (window-buffer source-window) source))
                    (should (= (window-point source-window) source-point))
                    (should (eq (selected-window)
                                (if focus result-window source-window)))
                    (pcase direction
                      ('right (should (>= (nth 0 result-edges) (nth 2 source-edges))))
                      ('left (should (<= (nth 2 result-edges) (nth 0 source-edges))))
                      ('below (should (>= (nth 1 result-edges) (nth 3 source-edges))))
                      ('above (should (<= (nth 3 result-edges) (nth 1 source-edges)))))
                    (with-current-buffer buffer (sql-grid-refresh))
                    (should (eq (get-buffer-window buffer) result-window))
                    (should (= (length (window-list)) 2)))))
            (dolist (item (list buffer source))
              (when (buffer-live-p item) (kill-buffer item)))))))))


(ert-deftest sql-grid-test-frozen-column-names-follow-scrolling-and-view-changes ()
  "Keep literal headings and status visible while retained rows scroll."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test frozen headings*"))
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        (sql-grid-freeze-column-names nil)
        (sql-grid-max-rows nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("identifier" "100% name")
                              :rows (cl-loop for i from 1 to 100
                                             collect (list i (format "Customer %d" i))))
                             (sql-grid-result-create
                              :columns '("other heading") :rows '(("other value")))))))
          (select-window (get-buffer-window buffer))
          (delete-other-windows)
          (with-current-buffer buffer
            (should (string-match-p "Records: 100"
                                    (format-mode-line header-line-format)))
            (should-not tab-line-format)
            (should (sql-grid-test--buffer-contains-p buffer "identifier"))
            (setq sql-grid-freeze-column-names t)
            (sql-grid-refresh)
            (redisplay t)
            (should-not (sql-grid-test--buffer-contains-p buffer "identifier"))
            (should-not (sql-grid-test--buffer-contains-p buffer "100% name"))
            (let ((start (window-start)))
              (dotimes (_ 40)
                (execute-kbd-macro (kbd "C-n")))
              (redisplay t)
              (should (> (window-start) start)))
            (let ((heading (format-mode-line header-line-format)))
              (should (string-match-p "identifier" heading))
              (should (string-match-p "100% name" heading)))
            (should (string-match-p "Records: 100"
                                    (format-mode-line tab-line-format)))
            (sql-grid-sort-column 1)
            (should (string-match-p "100% name ↑1"
                                    (format-mode-line header-line-format)))
            (sql-grid-move-column-left)
            (should (string-match-p "100% name.*identifier"
                                    (format-mode-line header-line-format)))
            (sql-grid-set-result-format 'text)
            (should (string-match-p "100% name.*identifier"
                                    (format-mode-line header-line-format)))
            (should-not (sql-grid-test--buffer-contains-p buffer "identifier"))
            (should-not (sql-grid-test--buffer-contains-p buffer "100% name"))
            (goto-char (sql-grid-test--button-with-help buffer "other value"))
            (should (string-match-p "other heading"
                                    (format-mode-line header-line-format)))
            (should (string-match-p "Set 2/2.*Records: 1"
                                    (format-mode-line tab-line-format)))
            (setq sql-grid-freeze-column-names nil)
            (sql-grid-refresh)
            (should-not tab-line-format)
            (should (sql-grid-test--buffer-contains-p buffer "other heading"))
            (should-not (string-match-p "other heading"
                                        (format-mode-line header-line-format)))))
      (dolist (item (list buffer source))
        (when (buffer-live-p item) (kill-buffer item))))))

(ert-deftest sql-grid-test-graphical-header-status-tracks-current-set ()
  "Show literal filter text and current set counts and page in the header."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test status*"))
        (sql-grid-result-format 'text)
        (sql-grid-focus-results t)
        (sql-grid-max-rows 1)
        buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create :columns '("first")
                                                      :rows '(("other")))
                             (sql-grid-result-create :columns '("second")
                                                      :rows '(("100% x") ("other") ("100% y")))))))
          (with-current-buffer buffer
            (should-not (string-match-p "\\`SQL results" (buffer-string)))
            (sql-grid-filter-rows "100%")
            (goto-char (sql-grid-test--button-with-help buffer "second"))
            (sql-grid-next-page)
            (let ((header (format-mode-line header-line-format nil (selected-window))))
              (should (string-match-p "Set 2/2" header))
              (should (string-match-p "Records: 3" header))
              (should (string-match-p "Matched: 2" header))
              (should (string-match-p "Filter: 100%" header))
              (should (string-match-p "Page 2/2" header))
              (should-not (string-match-p "Rows " header)))
            (goto-char (sql-grid-test--button-with-help buffer "second"))
            (sql-grid-filter-rows "")
            (let ((header (format-mode-line header-line-format nil (selected-window))))
              (should (string-match-p "Set 2/2" header))
              (should (string-match-p "Records: 3" header))
              (should-not (string-match-p "Matched:" header))
              (should-not (string-match-p "Filter:" header))
              (should (string-match-p "Page 1/3" header))
              (should-not (string-match-p "Completed" header))
              (should-not (string-match-p "Rows " header)))))
      (dolist (item (list buffer source))
        (when (buffer-live-p item) (kill-buffer item))))))

(ert-deftest sql-grid-test-sort-retained-columns-before-pagination ()
  "Sort full values stably before paging, retaining SQL values and row identity."
  (let* ((source (generate-new-buffer " *sql-grid-test sorting*"))
         (sql-grid-result-format 'text)
         (sql-grid-focus-results nil)
         (sql-grid-max-rows 2)
         (sql-grid-max-cell-chars 4)
         (kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil)
         (rows '((10 "ten") (2 "first two") (nil "missing")
                 (2 "second two") ("" "empty")))
         (results (list (sql-grid-result-create
                         :columns '("number" "label") :rows rows)))
         buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer (sql-grid-display-results nil results)))
          (with-current-buffer buffer
            (sql-grid-sort-column 0)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "label [2]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "missing\tfirst two"))
            (sql-grid-next-page)
            (goto-char (sql-grid-test--button-with-help buffer "second two"))
            (sql-grid-copy-cell)
            (should (equal (car kill-ring) "second two"))
            (sql-grid-refresh)
            (sql-grid-copy-cell)
            (should (equal (car kill-ring) "second two"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "label [2]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "second two\tten"))
            (sql-grid-sort-column 0)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "label [2]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "empty\tten"))
            (sql-grid-filter-rows "two")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "label [2]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "first two\tsecond two"))
            (sql-grid-filter-rows "")
            (sql-grid-sort-column nil t)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "label [2]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "ten\tfirst two"))
            (should (equal (sql-grid-result-rows (car results)) rows))
            (should (equal (sql-grid-result-columns (car results))
                           '("number" "label")))))
      (dolist (item (list buffer source))
        (when (buffer-live-p item) (kill-buffer item))))))

(ert-deftest sql-grid-test-sort-isolates-sets-and-new-results ()
  "Keep sort state per set and reset it for new results."
  (let ((source (generate-new-buffer " *sql-grid-test sort sets*"))
        (sql-grid-result-format 'text)
        (sql-grid-focus-results nil)
        (sql-grid-max-rows nil)
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (results (list (sql-grid-result-create :columns '("left")
                                                :rows '(("b") ("a")))
                       (sql-grid-result-create :columns '("right")
                                                :rows '(("z") ("x")))))
        buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer (sql-grid-display-results nil results)))
          (with-current-buffer buffer
            (goto-char (sql-grid-test--button-with-help buffer "right"))
            (sql-grid-sort-column)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (caar choices))))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "x\tz"))
            (goto-char (sql-grid-test--button-with-help buffer "left"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (caar choices))))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "b\ta")))
          (with-current-buffer source (sql-grid-display-results nil results))
          (with-current-buffer buffer
            (goto-char (sql-grid-test--button-with-help buffer "right"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (caar choices))))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "z\tx"))))
      (dolist (item (list buffer source))
        (when (buffer-live-p item) (kill-buffer item))))))

(ert-deftest sql-grid-test-sqlite-public-api-preserves-values-and-errors ()
  "Exercise real SQLite values, updates, transaction state, and SQL errors."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection
                           "CREATE TABLE values_test (id INTEGER PRIMARY KEY, val TEXT)")
     (sql-grid-db-execute
      connection
      (concat "INSERT INTO values_test (val) VALUES "
              "(NULL), (''), ('ភាសាខ្មែរជាភាសាជាតិ'), ('line one\nline two')"))
     (let* ((result (car (sql-grid-db-execute
                          connection "SELECT id, val FROM values_test ORDER BY id")))
            (rows (sql-grid-result-rows result)))
       (should (equal (sql-grid-result-columns result) '("id" "val")))
       (should (equal (nth 0 (nth 0 rows)) 1))
       (should (null (nth 1 (nth 0 rows))))
       (should (equal (nth 1 (nth 1 rows)) ""))
       (should (equal (nth 1 (nth 2 rows)) "ភាសាខ្មែរជាភាសាជាតិ"))
       (should (equal (nth 1 (nth 3 rows)) "line one\nline two")))
     (sql-grid-db-execute connection "UPDATE values_test SET val = 'updated' WHERE id = 1")
     (let ((updated (car (sql-grid-db-execute
                          connection "SELECT val FROM values_test WHERE id = 1"))))
       (should (equal (caar (sql-grid-result-rows updated)) "updated")))
     (should-error (sql-grid-db-execute connection "SELECT 1; SELECT 2"))
     (should-error (sql-grid-db-execute connection "SELECT missing_column FROM values_test"))
     (sql-grid-db-execute connection "BEGIN")
     (sql-grid-db-execute connection "INSERT INTO values_test (val) VALUES ('rollback')")
     (sql-grid-db-execute connection "ROLLBACK")
     (let ((count (car (sql-grid-db-execute connection
                                             "SELECT count(*) FROM values_test"))))
       (should (= (caar (sql-grid-result-rows count)) 4))))))

(ert-deftest sql-grid-test-sqlite-empty-select-retains-column-metadata ()
  "A zero-row SQLite SELECT still exposes its column names."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE empty_test (id INTEGER, val TEXT)")
     (let ((result (car (sql-grid-db-execute connection "SELECT id, val FROM empty_test"))))
       (should (equal (sql-grid-result-columns result) '("id" "val")))
       (should-not (sql-grid-result-rows result))))))

(ert-deftest sql-grid-test-sqlite-scalars-blob-null-and-empty-wire ()
  "Preserve full SQLite scalars, BLOB bytes, and NULL versus empty text."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE wire (kind TEXT, value)")
     (sql-grid-db-execute
      connection
      (concat "INSERT INTO wire VALUES "
              "('null', NULL), ('empty', ''), ('blob', X'0041FF80'), "
              "('text', 'line one\nline two'), ('unicode', 'ខ្មែរ')"))
     (let* ((result (car (sql-grid-db-execute
                         connection
                         "SELECT kind, value, typeof(value) AS storage FROM wire ORDER BY rowid")))
            (rows (sql-grid-result-rows result)))
       (should (equal (sql-grid-result-columns result)
                      '("kind" "value" "storage")))
       (should (numberp (sql-grid-result-elapsed result)))
       (should (>= (sql-grid-result-elapsed result) 0))
       (should (= (length rows) 5))
       (should (equal (nth 0 rows) '("null" nil "null")))
       (should (equal (nth 1 rows) '("empty" "" "text")))
       (should (equal (nth 0 (nth 2 rows)) "blob"))
       (should (equal (nth 1 (nth 2 rows)) (unibyte-string 0 65 255 128)))
       (should (= (length (nth 1 (nth 2 rows))) 4))
       (should-not (multibyte-string-p (nth 1 (nth 2 rows))))
       (should (equal (nth 2 (nth 2 rows)) "blob"))
       (should (equal (nth 1 (nth 3 rows)) "line one\nline two"))
       (should (equal (nth 1 (nth 4 rows)) "ខ្មែរ"))
       (should (multibyte-string-p (nth 1 (nth 4 rows)))))
     ;; Native SQLite exposes CAST(x'80' AS TEXT) as a multibyte raw-byte
     ;; string; the worker wire must preserve that exact value.
     (let ((native (sqlite-open nil))
           (actual (caar (sql-grid-result-rows
                          (car (sql-grid-db-execute
                                connection
                                "SELECT CAST(x'80' AS TEXT) AS raw"))))))
       (unwind-protect
           (let ((expected (caar (sqlite-select
                                  native "SELECT CAST(x'80' AS TEXT)"))))
             (should (equal actual expected))
             (should (multibyte-string-p actual)))
         (sqlite-close native)))
     (let* ((result (car (sql-grid-db-execute
                         connection
                         (concat "SELECT 9223372036854775807 AS big, "
                                 "-12.5 AS real, NULL AS missing, '' AS empty"))))
            (row (car (sql-grid-result-rows result))))
       (should (equal (nth 0 row) 9223372036854775807))
       (should (equal (nth 1 row) -12.5))
       (should-not (nth 2 row))
       (should (equal (nth 3 row) ""))))))

(ert-deftest sql-grid-test-sqlite-with-dml-returning-and-affected-counts ()
  "Count affected rows for WITH-prefixed and RETURNING data changes."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute
      connection "CREATE TABLE tally (id INTEGER PRIMARY KEY, note TEXT)")
     (let ((result (car (sql-grid-db-execute
                         connection
                         (concat "WITH rows(id, note) AS (VALUES (1, 'a'), (2, 'b')) "
                                 "INSERT INTO tally SELECT id, note FROM rows")))))
       (should (equal (sql-grid-result-affected-rows result) 2))
       (should-not (sql-grid-result-rows result)))
     (let ((result (car (sql-grid-db-execute
                         connection
                         (concat "WITH target AS (SELECT 2 AS id) "
                                 "UPDATE tally SET note = 'changed' "
                                 "WHERE id = (SELECT id FROM target)")))))
       (should (equal (sql-grid-result-affected-rows result) 1)))
     (let ((result (car (sql-grid-db-execute
                         connection
                         (concat "WITH doomed AS (SELECT 1 AS id) "
                                 "DELETE FROM tally "
                                 "WHERE id IN (SELECT id FROM doomed)")))))
       (should (equal (sql-grid-result-affected-rows result) 1)))
     (let ((result (car (sql-grid-db-execute
                         connection
                         (concat "WITH missing AS (SELECT 99 AS id) "
                                 "DELETE FROM tally "
                                 "WHERE id IN (SELECT id FROM missing)")))))
       (should (integerp (sql-grid-result-affected-rows result)))
       (should (equal (sql-grid-result-affected-rows result) 0)))
     (let ((result (car (sql-grid-db-execute
                         connection
                         "INSERT INTO tally (id, note) VALUES (3, 'c') RETURNING id, note"))))
       (should (equal (sql-grid-result-columns result) '("id" "note")))
       (should (equal (sql-grid-result-rows result) '((3 "c"))))
       (should (equal (sql-grid-result-affected-rows result) 1)))
     (let ((result (car (sql-grid-db-execute
                         connection
                         (concat "WITH more(id, note) AS (VALUES (4, 'd'), (5, 'e')) "
                                 "INSERT INTO tally SELECT id, note FROM more "
                                 "RETURNING id")))))
       (should (equal (sql-grid-result-columns result) '("id")))
       (should (equal (sql-grid-result-rows result) '((4) (5))))
       (should (equal (sql-grid-result-affected-rows result) 2)))
     (let ((result (car (sql-grid-db-execute
                         connection
                         "WITH counts AS (SELECT count(*) AS n FROM tally) SELECT n FROM counts"))))
       (should (equal (sql-grid-result-rows result) '((4))))
       (should-not (sql-grid-result-affected-rows result))))))

(ert-deftest sql-grid-test-copy-cell-with-emacs-binding ()
  "Copy full cell values through standard Emacs `M-w'."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test copy cell*"))
           (result-buffer nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (sql-grid-result-format 'text)
           (sql-grid-show-cell-tooltips t))
       (unwind-protect
           (progn
             (with-current-buffer editor
               (setq result-buffer
                     (sql-grid-display-results
                      connection
                      (sql-grid-db-execute
                       connection
                       "SELECT 'long full value' UNION ALL SELECT NULL UNION ALL SELECT ''"))))
             (dolist (case '(("long full value" "long full value")
                             ("NULL" "NULL")
                             ("" "")))
               (let ((position
                      (sql-grid-test--button-with-help result-buffer (car case))))
                 (should position)
                 (with-current-buffer result-buffer
                   (goto-char position)
                   (dolist (key '("M-w" "y"))
                     (should (eq (key-binding (kbd key)) #'sql-grid-copy-cell))
                     (call-interactively (key-binding (kbd key)))
                     (should (equal (current-kill 0) (cadr case))))))))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

 (ert-deftest sql-grid-test-cell-motion-lands-inside-adjacent-button ()
   "Cell motion lands inside each adjacent button after one command."
   (with-temp-buffer
     (insert-text-button "first value")
     (insert " ")
     (insert-text-button "second value")
     (goto-char (point-min))
     (sql-grid-next-cell)
     (should (button-at (point)))
     (should (equal (button-label (button-at (point))) "second value"))
     (sql-grid-previous-cell)
     (should (button-at (point)))
     (should (equal (button-label (button-at (point))) "first value"))))

(ert-deftest sql-grid-test-graphical-grid-cell-motion-visits-every-column ()
  "Each `M-f' selects the next cell, regardless of its word count."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test cell navigation*"))
        result-buffer
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t)
        (kill-ring nil)
        (kill-ring-yank-pointer nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "value")
                              :rows '((1 "first value") (2 "second value")))))))
          (select-window (get-buffer-window result-buffer))
          (goto-char (sql-grid-test--button-with-help result-buffer "id"))
          (dolist (value '("value" "1" "first value" "2" "second value"))
            (sql-grid-test--press-in-buffer result-buffer (point) "M-f")
            (redisplay t)
            (call-interactively #'sql-grid-copy-cell)
            (should (equal (current-kill 0) value))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (kill-buffer source))))

(ert-deftest sql-grid-test-evil-grid-cell-motion-visits-every-column ()
  "Evil's `M-f' selects each heading and value with exactly one press."
  (skip-unless (and (display-graphic-p)
                    (require 'evil nil t)))
  (let ((source (generate-new-buffer " *sql-grid-test Evil navigation*"))
        result-buffer
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (kill-ring nil)
        (kill-ring-yank-pointer nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "customer name" "note")
                              :rows '((1 "Alice Smith" "first note")
                                      (2 "Bob Jones" "")
                                      (3 nil "last note")))))))
          (select-window (get-buffer-window result-buffer))
          (funcall (intern "evil-local-mode") 1)
          (funcall (intern "evil-normal-state"))
          (goto-char (sql-grid-test--button-with-help result-buffer "id"))
          (call-interactively #'sql-grid-copy-cell)
          (should (equal (current-kill 0) "id"))
          (dolist (value '("customer name" "note"
                           "1" "Alice Smith" "first note"
                           "2" "Bob Jones" "" "3" "NULL" "last note"))
            (sql-grid-test--press-in-buffer result-buffer (point) "M-f")
            (redisplay t)
            (call-interactively #'sql-grid-copy-cell)
            (should (equal (current-kill 0) value))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (kill-buffer source))))

(ert-deftest sql-grid-test-evil-grid-navigation-keys-move-one-cell ()
  "Documented navigation keys move one cell per press in Evil normal state."
  (skip-unless (and (display-graphic-p)
                    (require 'evil nil t)))
  (let ((source (generate-new-buffer " *sql-grid-test Evil keys*"))
        result-buffer
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "customer name" "note")
                              :rows '((1 "Alice Smith" "first note")
                                      (2 "Bob Jones" "")
                                      (3 nil "last note")))))))
          (select-window (get-buffer-window result-buffer))
          (funcall (intern "evil-local-mode") 1)
          (funcall (intern "evil-normal-state"))
          (cl-flet ((move (value key expected)
                      (let ((position
                             (or (sql-grid-test--button-with-help
                                  result-buffer value)
                                 (ert-fail (format "missing cell %S" value)))))
                        (sql-grid-test--press-in-buffer
                         result-buffer position key)
                        (redisplay t)
                        (let ((cell (sql-grid--cell-at-point)))
                          (should (eq (current-buffer) result-buffer))
                          (should cell)
                          (should (equal (button-get cell 'sql-grid-value)
                                         expected))
                          (should (and (overlayp sql-grid--cursor-overlay)
                                       (= (overlay-start
                                           sql-grid--cursor-overlay)
                                          (button-start cell))
                                       (= (overlay-end
                                           sql-grid--cursor-overlay)
                                          (button-end cell))))))))
            (dolist (key '("<right>" "C-f" "TAB" "l"))
              (move "id" key "customer name"))
            (dolist (key '("<left>" "C-b" "<backtab>" "h"))
              (move "customer name" key "id"))
            (dolist (key '("<down>" "C-n" "j"))
              (move "Alice Smith" key "Bob Jones"))
            (dolist (key '("<up>" "C-p" "k"))
              (move "Bob Jones" key "Alice Smith"))
            (dolist (movement '(("customer name" "id")
                                ("Alice Smith" 1)
                                ("Bob Jones" 2)))
              (move (car movement) "0" (cadr movement))
              (sql-grid-copy-cell)
              (should (equal (current-kill 0) (format "%s" (cadr movement)))))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (kill-buffer source))))


(ert-deftest sql-grid-test-graphical-cell-navigation-and-inspection ()
  "Exercise cell tooltips and raw-value inspection through actual key events."
  (skip-unless (display-graphic-p))
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test interaction*"))
           (result-buffer nil)
           (detail-buffer nil)
           (sql-grid-max-cell-chars 9)
           (sql-grid-show-cell-tooltips t))
       (unwind-protect
           (progn
             (sql-grid-db-execute connection "CREATE TABLE counter (n INTEGER)")
             (sql-grid-db-execute connection "INSERT INTO counter VALUES (0)")
             (let* ((mutation
                     "UPDATE counter SET n = n + 1 RETURNING 'ភាសាខ្មែរជាភាសាជាតិ' AS val")
                    (update-results (sql-grid-db-execute connection mutation)))
               (with-current-buffer editor
                 (setq result-buffer
                       (sql-grid-display-results connection update-results)))
               (with-current-buffer result-buffer
                 (sql-grid-refresh))
               (let ((update-button
                      (sql-grid-test--button-with-help
                       result-buffer "ភាសាខ្មែរជាភាសាជាតិ")))
                 (should update-button)
                 (setq detail-buffer
                       (sql-grid-test--press-in-buffer result-buffer
                                                        update-button "RET"))
                 (with-current-buffer detail-buffer
                   (should (equal (buffer-string) "ភាសាខ្មែរជាភាសាជាតិ"))))
               (let ((counter (car (sql-grid-db-execute connection "SELECT n FROM counter"))))
                 (should (= (caar (sql-grid-result-rows counter)) 1))))
             (sql-grid-db-execute connection
                                   "CREATE TABLE cells (label TEXT, val TEXT)")
             (sql-grid-db-execute
              connection
              (concat "INSERT INTO cells VALUES "
                      "('long', 'ភាសាខ្មែរជាភាសាជាតិ'), "
                      "('multi', 'line one\nline two'), "
                      "('empty', ''), ('null', NULL), ('exact', 'ភាសាខ្មែរ')"))
             (let* ((select "SELECT label, val FROM cells")
                    (results (sql-grid-db-execute connection select)))
               (with-current-buffer editor
                 (setq result-buffer (sql-grid-display-results connection results)))
               (let* ((long-button
                       (sql-grid-test--button-with-help
                        result-buffer "ភាសាខ្មែរជាភាសាជាតិ"))
                      (multi-button
                       (sql-grid-test--button-with-help result-buffer "line one\nline two"))
                      (null-button (sql-grid-test--button-with-help result-buffer "NULL"))
                      (long-position long-button))
                 (should long-button)
                 (should multi-button)
                 (should null-button)
                 (should (equal (sql-grid-test--help-echo-at result-buffer long-button)
                                "ភាសាខ្មែរជាភាសាជាតិ"))
                 (should (equal (sql-grid-test--help-echo-at result-buffer multi-button)
                                "line one\nline two"))
                 (should (equal (sql-grid-test--help-echo-at result-buffer null-button)
                                "NULL"))
                 (dolist (event '("RET" "<return>"))
                   (dolist (position (list long-button (1+ long-button)))
                     (let ((source-point position)
                           (detail-buffer
                            (sql-grid-test--press-in-buffer
                             result-buffer position event)))
                       (should (buffer-live-p detail-buffer))
                       (should (= (with-current-buffer result-buffer (point))
                                  source-point))
                       (should (equal (with-current-buffer detail-buffer
                                        (buffer-string))
                                      "ភាសាខ្មែរជាភាសាជាតិ")))))
                 (let ((buffer (sql-grid-test--press-in-buffer
                                result-buffer long-button "TAB")))
                   (should (eq buffer result-buffer))
                   (with-current-buffer result-buffer
                     (should (equal (sql-grid-test--help-echo-at
                                     result-buffer (point)) "multi"))))
                 (sql-grid-test--press-in-buffer
                  result-buffer (with-current-buffer result-buffer (point)) "<backtab>")
                 (with-current-buffer result-buffer
                   (should (equal (sql-grid-test--help-echo-at result-buffer (point))
                                  "ភាសាខ្មែរជាភាសាជាតិ")))
                 (setq detail-buffer
                       (sql-grid-test--press-in-buffer result-buffer
                                                        long-button "RET"))
                 (with-current-buffer detail-buffer
                   (should buffer-read-only)
                   (should (equal (buffer-string) "ភាសាខ្មែរជាភាសាជាតិ")))
                 (setq detail-buffer
                       (sql-grid-test--press-in-buffer result-buffer
                                                        multi-button "RET"))
                 (with-current-buffer detail-buffer
                   (should (equal (buffer-string) "line one\nline two")))
                 (setq detail-buffer
                       (sql-grid-test--press-in-buffer result-buffer
                                                        null-button "RET"))
                 (with-current-buffer detail-buffer
                   (should (equal (buffer-string) "NULL")))
                 (let ((sql-grid-show-cell-tooltips nil))
                   (with-current-buffer result-buffer (sql-grid-refresh))
                   (should (with-current-buffer result-buffer
                             (button-at long-position)))
                   (should-not (sql-grid-test--help-echo-at
                                result-buffer long-position))
                   (setq detail-buffer
                         (sql-grid-test--press-in-buffer result-buffer
                                                          long-position "RET"))
                   (with-current-buffer detail-buffer
                     (should (equal (buffer-string) "ភាសាខ្មែរជាភាសាជាតិ"))))
                 (let ((counter (car (sql-grid-db-execute connection "SELECT n FROM counter"))))
                   (should (= (caar (sql-grid-result-rows counter)) 1))))))
         (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-graphical-arrow-cell-inspection ()
  "Arrow movement inspects the cell under the visible cursor, not its neighbor."
  (skip-unless (display-graphic-p))
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test arrow inspection*"))
           result-buffer detail-buffer
           (sql-grid-result-format 'grid)
           (sql-grid-cell-display 'buffer)
           (sql-grid-show-cell-tooltips t))
       (unwind-protect
           (progn
             (with-current-buffer editor
               (setq result-buffer
                     (sql-grid-display-results
                      connection
                      (sql-grid-db-execute
                       connection
                       "SELECT 'Alice' AS name, 'alice@example.com' AS email"))))
             (dolist (event '("<return>" "RET"))
               (dolist (movement '(("Alice" "C-f" "alice@example.com")
                                   ("alice@example.com" "C-b" "Alice")
                                   ("Alice" "C-s Alice RET" "Alice")))
                 (let ((position (sql-grid-test--button-with-help
                                  result-buffer (nth 0 movement))))
                   (should position)
                   (sql-grid-test--press-in-buffer
                    result-buffer position (nth 1 movement))
                   (setq detail-buffer
                         (sql-grid-test--press-in-buffer
                          result-buffer
                          (with-current-buffer result-buffer (point))
                          event))
                   (should (equal (with-current-buffer detail-buffer
                                    (buffer-string))
                                  (nth 2 movement)))))))
         (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-graphical-grid-cursor-alignment ()
  "Keep cell focus aligned without changing raw point."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test cursor*"))
        result-buffer detail-buffer positions
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t)
        (sql-grid-cell-display 'buffer)
        (sql-grid-max-cell-chars 10))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "email" "city")
                              :rows '((1 "cust1@example.com" "New York")
                                      (2 "cust2@example.com" "Tokyo")
                                      (3 "cust3@example.com" "London")))))))
          (select-window (get-buffer-window result-buffer))
          (goto-char (point-min))
          (setq-local cursor-type 'bar)
          (redisplay t)
          (dolist (value '("cust1@example.com" "cust2@example.com"
                           "cust3@example.com" "New York"))
            (let ((start (sql-grid-test--button-with-help result-buffer value)))
              (push (list value start (posn-x-y (posn-at-point start))) positions)))
          (cl-labels
              ((check-cell (value)
                 (let ((raw-point (point)))
                   (redisplay t)
                   (should (= (point) raw-point)))
                 (should (equal (get-text-property (point) 'sql-grid-value)
                                value))
                 (dolist (entry positions)
                   (let ((face (get-char-property (nth 1 entry) 'face)))
                     (if (equal (car entry) value)
                         (should (and (listp face) (plist-get face :box)))
                       (should-not (and (listp face) (plist-get face :box)))))
                   ;; An outline must not add width or height to any slice.
                   (should (equal (posn-x-y (posn-at-point (nth 1 entry)))
                                  (nth 2 entry))))))
            ;; Interior search positions used to draw on the next slice,
            ;; or beyond the table when the match was in the final column.
            (dolist (case '(("C-s cust1 RET" "cust1@example.com")
                            ("C-s New RET" "New York")))
              (sql-grid-test--press-in-buffer
               result-buffer (point-min) (car case))
              (check-cell (cadr case))
              (let ((raw-point (point)))
                (setq detail-buffer
                      (sql-grid-test--press-in-buffer result-buffer raw-point "RET"))
                (should (equal (buffer-string) (cadr case)))
                (select-window (get-buffer-window result-buffer))
                (should (= (point) raw-point))))
            (goto-char (sql-grid-test--button-with-help
                        result-buffer "cust1@example.com"))
            (dolist (step '(("<down>" "cust2@example.com")
                            ("<down>" "cust3@example.com")
                            ("<up>" "cust2@example.com")
                            ("<up>" "cust1@example.com")))
              (sql-grid-test--press-in-buffer result-buffer (point) (car step))
              (check-cell (cadr step)))
            (sql-grid-set-result-format 'text)
            (redisplay t)
            (should (eq cursor-type 'bar))))
      (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))


(ert-deftest sql-grid-test-existing-buffer-execution ()
  "Enable SQL editing keys while preserving source text and buffer lifetime."
  (let ((editor (generate-new-buffer " *sql-grid-test existing buffer*"))
        (result-buffer nil)
        (sql-grid-connections nil)
        (sql-grid-result-format 'text)
        (sql-grid-focus-results nil)
        (contents "Not SQL outside the block\n\nSELECT 6 * 7 AS answer;\n\nMore non-SQL text\n"))
    (unwind-protect
        (with-current-buffer editor
          (text-mode)
          (insert contents)
          (local-set-key (kbd "C-c C-c") #'ignore)
          (let ((modified (buffer-modified-p))
                (position (point)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "sqlite"))
                      ((symbol-function 'read-string)
                       (lambda (&rest _) ":memory:")))
              (sql-grid-connect-buffer))
            (should (eq major-mode 'sql-grid-mode))
            (should (derived-mode-p 'sql-mode))
            (should (eq (buffer-modified-p) modified))
            (should (= (point) position))
            (should (equal (buffer-string) contents))
            (should-error (sql-grid-connect-buffer) :type 'user-error)
            (goto-char (point-min))
            (forward-line 2)
            (setq result-buffer
                  (call-interactively (key-binding (kbd "C-c C-c"))))
            (should (sql-grid-test--button-with-help result-buffer "42"))
            (call-interactively (key-binding (kbd "C-c C-d")))
            (should (buffer-live-p editor))
            (should (equal (buffer-string) contents))
            (should-error (sql-grid-execute-statement) :type 'user-error)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "sqlite"))
                      ((symbol-function 'read-string)
                       (lambda (&rest _) ":memory:")))
              (sql-grid-connect-buffer))
            (set-mark (line-end-position))
            (setq result-buffer
                  (call-interactively (key-binding (kbd "C-c C-r"))))
            (should (sql-grid-test--button-with-help result-buffer "42"))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p editor) (kill-buffer editor)))))

(ert-deftest sql-grid-test-graphical-named-connections ()
  "Select distinct databases through both commands, or enter manual settings."
  (skip-unless (display-graphic-p))
  (sql-grid-test--with-sqlite
   (lambda (first)
     (sql-grid-test--with-sqlite
      (lambda (second)
        (sql-grid-db-execute
         first "CREATE TABLE profile_data AS SELECT 'first database' AS label")
        (sql-grid-db-execute
         second "CREATE TABLE profile_data AS SELECT 'second database' AS label")
        (let ((sql-grid-connections
               `(("First" :backend sqlite
                  :database ,(sql-grid-connection-database first))
                 ("Second" :backend sqlite
                  :database ,(sql-grid-connection-database second))))
              (source (generate-new-buffer " *sql-grid-test profiles*"))
              (editors nil)
              (results nil))
          (unwind-protect
              (dolist (case '(("First" sql-grid-connect-buffer "first database")
                              ("Second" sql-grid-connect "second database")
                              ("" sql-grid-connect-buffer "first database")))
                (let* ((name (nth 0 case))
                       (command (nth 1 case))
                       (answers (if (string-empty-p name)
                                    '("" "sqlite")
                                  (list name)))
                       (editor
                        (with-current-buffer source
                          (cl-letf (((symbol-function 'completing-read)
                                     (lambda (&rest _) (pop answers)))
                                    ((symbol-function 'read-string)
                                     (lambda (&rest _)
                                       (sql-grid-connection-database first))))
                            (if (eq command 'sql-grid-connect)
                                (funcall command)
                              (funcall command)
                              source)))))
                  (push editor editors)
                  (with-current-buffer editor
                    (erase-buffer)
                    (insert "SELECT label FROM profile_data")
                    (let ((result (sql-grid-execute-statement)))
                      (push result results)
                      (should (sql-grid-test--button-with-help
                               result (nth 2 case))))))
                (with-current-buffer (car editors)
                  (sql-grid-disconnect)))
            (dolist (buffer (append results editors (list source)))
              (when (buffer-live-p buffer) (kill-buffer buffer))))))))))

(ert-deftest sql-grid-test-blank-line-block-selection ()
  "Select the block at point without including notes or splitting multiple SQL."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE block_counter AS SELECT 0 AS n")
     (let ((sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           (sql-grid-connections
            `(("Blocks" :backend sqlite
               :database ,(sql-grid-connection-database connection))))
           (editor (generate-new-buffer " *sql-grid-test blocks*"))
           (result-buffer nil))
       (unwind-protect
           (progn
             (with-current-buffer editor
               (insert ";; Scratch notes, not SQL\n;; More notes\n\n"
                       "/tmp/not-sql.sqlite\n \t\n"
                       "SELECT\n 'first;value' AS val\n \t\n"
                       "UPDATE block_counter SET n = 77; "
                       "UPDATE block_counter SET n = 88;\n\n"
                       "SELECT\n n FROM block_counter")
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "Blocks")))
                 (sql-grid-connect-buffer))
               (goto-char (point-min))
               (search-forward "first;value")
               (setq result-buffer (sql-grid-execute-statement)))
             (should (sql-grid-test--button-with-help result-buffer "first;value"))
             (with-current-buffer editor
               (search-forward "SET n = 77")
               (should-error (sql-grid-execute-statement) :type 'user-error)
               (goto-char (point-min))
               (search-forward "\n \t\n")
               (backward-char 1)
               (should-error (sql-grid-execute-statement) :type 'user-error)
               (goto-char (point-max))
               (setq result-buffer (sql-grid-execute-statement)))
             (should (sql-grid-test--button-with-help result-buffer "0")))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-compound-statements-rejected ()
  "Reject a whole compound statement without executing its SQL body."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE counter (n INTEGER)")
     (sql-grid-db-execute connection "INSERT INTO counter VALUES (0)")
     (let ((sql (concat "CREATE /* note */ TEMP TRIGGER change_counter "
                        "AFTER INSERT ON counter BEGIN "
                        "UPDATE counter SET n = 99; "
                        "UPDATE counter SET n = 100; END;")))
       (should-error (sql-grid-db-execute connection sql) :type 'user-error))
     (should (equal (sql-grid-result-rows
                     (car (sql-grid-db-execute connection "SELECT n FROM counter")))
                    '((0)))))))

(ert-deftest sql-grid-test-graphical-current-statement-execution ()
  "Execute multiline SQL at point without executing adjacent statements."
  (skip-unless (display-graphic-p))
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute
      connection "CREATE TABLE statement_counter AS SELECT 0 AS n")
     (let ((sql-grid-connections
            `(("Statement tests" :backend sqlite
               :database ,(sql-grid-connection-database connection))))
           (editor (generate-new-buffer " *sql-grid-test current statement*"))
           (result-buffer nil))
       (unwind-protect
           (progn
             (with-current-buffer editor
               (text-mode)
               (insert ";; Scratch instructions\n;; Not SQL\n\n"
                       "/tmp/database-path.sqlite\n \t\n"
                       "SELECT name\n"
                       "FROM sqlite_schema\n"
                       "WHERE type = 'table'\n"
                       "  AND name NOT LIKE 'sqlite_%'\n"
                       "ORDER BY name\n\n"
                       "UPDATE statement_counter\n"
                       "SET n = n + 1 -- ; comment\n"
                       "WHERE n = 0\n"
                       "RETURNING 'inside;''quote' AS [semi;colon], n;\n\n"
                       "DELETE FROM statement_counter;\n\n"
                       "SELECT\n n FROM statement_counter")
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "Statement tests")))
                 (sql-grid-connect-buffer))
               (goto-char (point-min))
               (search-forward "AND")
               (setq result-buffer (sql-grid-execute-statement)))
             (should (sql-grid-test--button-with-help
                      result-buffer "statement_counter"))
             (with-current-buffer editor
               (goto-char (point-min))
               (search-forward "\n\n")
               (backward-char 1)
               (should-error (sql-grid-execute-statement) :type 'user-error)
               (search-forward "WHERE n")
               (let ((transient-mark-mode t))
                 (set-mark (point-max))
                 (setq mark-active t)
                 (setq result-buffer
                       (call-interactively #'sql-grid-execute-statement))))
             (should (sql-grid-test--button-with-help
                      result-buffer "inside;'quote"))
             (should (sql-grid-test--button-with-help result-buffer "1"))
             (with-current-buffer editor
               (should-error
                (sql-grid-execute-region (point-min) (point-max))
                :type 'user-error)
               (goto-char (point-max))
               (setq result-buffer (sql-grid-execute-statement)))
             (should (sql-grid-test--button-with-help result-buffer "1")))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-graphical-result-focus-preference ()
  "Show query results while either retaining source point or selecting results."
  (skip-unless (display-graphic-p))
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((sql-grid-connections
            `(("Focus tests" :backend sqlite
               :database ,(sql-grid-connection-database connection))))
           (sql-grid-focus-results nil)
           (editor (generate-new-buffer " *sql-grid-test focus*"))
           (result-buffer nil))
       (unwind-protect
           (save-window-excursion
             (pop-to-buffer editor)
             (insert "SELECT\n 41 + 1 AS answer;\n\nSELECT\n 7 * 8 AS answer;")
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) "Focus tests")))
               (sql-grid-connect-buffer))
             (goto-char (point-min))
             (forward-line 1)
             (let ((source-window (selected-window))
                   (position (point)))
               (setq result-buffer (sql-grid-execute-statement))
               (should (eq (selected-window) source-window))
               (should (eq (window-buffer source-window) editor))
               (should (= (point) position))
               (should (get-buffer-window result-buffer))
               (should (sql-grid-test--button-with-help result-buffer "42"))
               (setq sql-grid-focus-results t)
               (search-forward "SELECT")
               (setq result-buffer
                     (sql-grid-execute-region (match-beginning 0) (point-max)))
               (should (eq (window-buffer (selected-window)) result-buffer))
               (should (sql-grid-test--button-with-help result-buffer "56"))
               (setq sql-grid-focus-results nil)
               (select-window source-window)
               (setq position (point))
               (setq result-buffer (sql-grid-execute-statement))
               (should (eq (selected-window) source-window))
               (should (eq (current-buffer) editor))
               (should (= (point) position))
               (should (get-buffer-window result-buffer))
               (should (sql-grid-test--button-with-help result-buffer "56"))))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-graphical-isearch-full-cell-values ()
  "Find truncated and multiline values with ordinary incremental search."
  (skip-unless (display-graphic-p))
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test search*"))
           (result-buffer nil)
           (detail-buffer nil)
           (sql-grid-focus-results t)
           (sql-grid-max-cell-chars 8))
       (unwind-protect
           (progn
             (sql-grid-db-execute connection "CREATE TABLE search_values (id INTEGER, val TEXT)")
             (sql-grid-db-execute
              connection
              (concat "INSERT INTO search_values VALUES "
                      "(1, 'prefix-hidden-needle-tail'), "
                      "(2, 'other-hidden-needle-tail'), "
                      "(3, 'line one' || char(10) || 'second-line-target'), "
                      "(4, ''), (5, NULL)"))
             (with-current-buffer editor
               (setq result-buffer
                     (sql-grid-display-results
                      connection
                      (sql-grid-db-execute
                       connection "SELECT id, val FROM search_values ORDER BY id"))))
             (dolist (case '(("C-s needle-tail RET" "prefix-hidden-needle-tail")
                             ("C-s needle-tail C-s RET" "other-hidden-needle-tail")
                             ("C-s second-line-target RET" "line one\nsecond-line-target")))
                 (sql-grid-test--press-in-buffer
                  result-buffer (with-current-buffer result-buffer (point-min))
                  (car case))
                 (setq detail-buffer
                       (sql-grid-test--press-in-buffer
                        result-buffer (with-current-buffer result-buffer (point)) "RET"))
                 (with-current-buffer detail-buffer
                   (should (equal (buffer-string) (cadr case)))))
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (should-not (search-forward "1prefix" nil t))))
         (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
         (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-graphical-grid-isearch-vertical-cell-motion ()
  "Keep vertical motion in searched grid column across truncated values."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test vertical search*"))
        result-buffer detail-buffer
        (sql-grid-result-format 'grid)
        (sql-grid-max-cell-chars 10)
        (sql-grid-focus-results t)
        (sql-grid-cell-display 'buffer)
        (kill-ring nil)
        (interprogram-cut-function nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil
                   (list
                    (sql-grid-result-create
                     :columns '("id" "name" "email" "city" "created")
                     :rows (cl-loop for n from 1 to 15
                                    collect (list n (format "Customer %d" n)
                                                  (format "cust%d@example.com" n)
                                                  "New York" "2023-03-15")))))))
          (sql-grid-test--press-in-buffer result-buffer (point-min) "C-s cust9 RET")
          (sql-grid-test--press-in-buffer result-buffer (point) "<down> M-w")
          (should (equal (car kill-ring) "cust10@example.com"))
          (sql-grid-test--press-in-buffer result-buffer (point) "C-n M-w")
          (should (equal (car kill-ring) "cust11@example.com"))
          (sql-grid-test--press-in-buffer result-buffer (point) "C-p M-w")
          (should (equal (car kill-ring) "cust10@example.com"))
          (with-current-buffer result-buffer
            (sql-grid-filter-rows "Customer \\(9\\|11\\)$"))
          (sql-grid-test--press-in-buffer
           result-buffer (with-current-buffer result-buffer (point-min))
           "C-s cust9 RET")
          (sql-grid-test--press-in-buffer result-buffer (point) "M-w")
          (should (equal (car kill-ring) "cust9@example.com"))
          (sql-grid-test--press-in-buffer result-buffer (point) "<down>")
          (sql-grid-test--press-in-buffer result-buffer (point) "M-w")
          (should (equal (car kill-ring) "cust11@example.com"))
          (sql-grid-test--press-in-buffer result-buffer (point) "<up>")
          (sql-grid-test--press-in-buffer result-buffer (point) "M-w")
          (should (equal (car kill-ring) "cust9@example.com"))
          (setq detail-buffer
                (sql-grid-test--press-in-buffer
                 result-buffer (point) "RET"))
          (should (equal (with-current-buffer detail-buffer (buffer-string))
                         "cust9@example.com")))
      (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest sql-grid-test-text-retains-results-per-buffer ()
  "Retain full values through refresh and executions in independent buffers."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE format_counter AS SELECT 0 AS n")
     (let ((sql-grid-result-format 'text)
           (sql-grid-focus-results t)
           (sql-grid-max-cell-chars 8)
           (sql-grid-show-cell-tooltips t)
           (sql-grid-connections
            `(("Formats" :backend sqlite
               :database ,(sql-grid-connection-database connection))))
           (editor (generate-new-buffer " *sql-grid-test formats*"))
           (other-editor (generate-new-buffer " *sql-grid-test other format*"))
           (result-buffer nil)
           (other-result nil)
           (detail-buffer nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil))
       (unwind-protect
           (progn
             (with-current-buffer editor
               (insert "UPDATE format_counter SET n = n + 1 RETURNING n, "
                       "'prefix-hidden-needle-tail' AS val, "
                       "'line one' || char(10) || 'second-line-target' AS multi, "
                       "NULL AS missing, '' AS empty;")
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "Formats")))
                 (sql-grid-connect-buffer))
               (goto-char (point-min))
               (setq result-buffer (sql-grid-execute-statement)))
             (sql-grid-test--await-results result-buffer)
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (should (re-search-forward "^| n +" nil t))
               (should (search-forward "needle-tail" nil t))
               (sql-grid-refresh)
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "val [2]")))
                 (sql-grid-copy-column))
               (should (equal (car kill-ring) "prefix-hidden-needle-tail")))
             (dolist (value '("" "NULL"))
               (with-current-buffer result-buffer
                 (let ((position (sql-grid-test--button-with-help result-buffer value)))
                   (should position)
                   (setq detail-buffer (sql-grid-view-cell (button-at position)))))
               (with-current-buffer detail-buffer
                 (should (equal (buffer-string) value))))
             (should (equal (sql-grid-result-rows
                             (car (sql-grid-db-execute
                                   connection "SELECT n FROM format_counter")))
                            '((1))))
             (with-current-buffer editor
               (erase-buffer)
               (insert "SELECT 99 AS n;")
               (should (eq result-buffer (sql-grid-execute-statement))))
             (sql-grid-test--await-results result-buffer)
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (should (re-search-forward "^| n +" nil t)))
             (with-current-buffer other-editor
               (setq other-result
                     (sql-grid-display-results
                      connection (sql-grid-db-execute connection "SELECT 7 AS other"))))
             (with-current-buffer other-result
               (should (string-match-p "^| other +" (buffer-string)))
               (when (display-graphic-p)
                 (sql-grid-set-result-format 'grid)))
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (should (re-search-forward "^| n +" nil t)))
             (with-current-buffer other-editor
               (sql-grid-display-results
                connection
                (sql-grid-db-execute connection "SELECT n FROM format_counter WHERE 0")))
             (should (sql-grid-test--button-with-help other-result "n"))
             (with-current-buffer other-result (sql-grid-refresh))
             (unless (display-graphic-p)
               (with-current-buffer other-result
                 (let ((before (buffer-string)))
                   (should-error (sql-grid-set-result-format 'grid) :type 'user-error)
                   (should (equal before (buffer-string)))
                   (sql-grid-refresh)))))
         (dolist (buffer (list detail-buffer result-buffer other-result editor other-editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-column-moves-retain-cell-and-values ()
  "Move columns without changing retained values or losing the selected cell."
  (let ((source (generate-new-buffer " *sql-grid-test column moves*"))
        result-buffer
        (sql-grid-result-format 'text)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t)
        (sql-grid-max-cell-chars 8)
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (results (list (sql-grid-result-create
                        :columns '("id" "name" "email" "missing" "empty")
                        :rows '((1 "Ada" "ada@example.com" nil "")
                                (2 "Lin" "lin@example.com" nil ""))))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer (sql-grid-display-results nil results)))
          (with-current-buffer result-buffer
            (goto-char (sql-grid-test--button-with-help
                        result-buffer "lin@example.com"))
            (sql-grid-move-column-left)
            (should (equal (button-get (button-at (point)) 'help-echo)
                           "lin@example.com"))
            (goto-char (point-min))
            (should (re-search-forward "^| id +| email +| name +|" nil t))
            (should (re-search-forward "^| 1 +| ada@example.com +| Ada +|" nil t))
            (goto-char (sql-grid-test--button-with-help
                        result-buffer "lin@example.com"))
            (sql-grid-move-column 1)
            (should-error (sql-grid-move-column-left) :type 'user-error)
            (sql-grid-move-column-right)
            (sql-grid-refresh)
            (should (equal (button-get (button-at (point)) 'help-echo)
                           "lin@example.com"))
            (sql-grid-set-result-format 'text)
            (save-excursion
              (goto-char (point-min))
              (should (re-search-forward "^| id +| email +| name +| missing +| empty +|" nil t)))
            (should (equal (button-get (button-at (point)) 'help-echo)
                           "lin@example.com"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (caar choices))))
              (sql-grid-copy-row))
            (should (equal (car kill-ring) "1\tada@example.com\tAda\tNULL\t"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "email [2]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "ada@example.com\tlin@example.com"))
            (sql-grid-reset-column-order)
            (save-excursion
              (goto-char (point-min))
              (should (re-search-forward "^| id +| name +| email +|" nil t)))
            (should (equal (button-get (button-at (point)) 'help-echo)
                           "lin@example.com"))
            (goto-char (point-min))
            (should-error (sql-grid-move-column-right) :type 'user-error))
          (should (equal (sql-grid-result-columns (car results))
                         '("id" "name" "email" "missing" "empty")))
          (should (equal (sql-grid-result-rows (car results))
                         '((1 "Ada" "ada@example.com" nil "")
                           (2 "Lin" "lin@example.com" nil "")))))
      (dolist (buffer (list result-buffer source))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sql-grid-test-column-order-isolates-result-sets ()
  "Distinguish duplicate names and reorder empty results independently."
  (let ((source (generate-new-buffer " *sql-grid-test column sets*"))
        result-buffer
        (sql-grid-result-format 'text)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t)
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("same" "same")
                              :rows '(("first" "second")))
                             (sql-grid-result-create
                              :columns '("left" "right") :rows nil)))))
          (with-current-buffer result-buffer
            (goto-char (sql-grid-test--button-with-help result-buffer "second"))
            (should-error (sql-grid-move-column-right) :type 'user-error)
            (dolist (position '(0 3 1.5))
              (should-error (sql-grid-move-column position) :type 'user-error))
            (sql-grid-move-column-left)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "same [1]")))
              (sql-grid-copy-column))
            (should (equal (car kill-ring) "second"))
            (goto-char (sql-grid-test--button-with-help result-buffer "right"))
            (sql-grid-move-column-left)
            (goto-char (point-min))
            (should (re-search-forward "^| second +| first +|" nil t))
            (should (re-search-forward "^| right +| left +|" nil t))
            (goto-char (sql-grid-test--button-with-help result-buffer "right"))
            (sql-grid-set-result-format 'text)
            (should (equal (button-get (button-at (point)) 'help-echo) "right"))
            (sql-grid-reset-column-order)
            (goto-char (point-min))
            (should (re-search-forward "^| left +| right +|" nil t))))
      (dolist (buffer (list result-buffer source))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sql-grid-test-column-order-new-execution-resets ()
  "Redraw never reruns SQL, but fresh execution restores SQL column order."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE moves AS SELECT 0 AS n")
     (let ((source (generate-new-buffer " *sql-grid-test column execution*"))
           result-buffer
           (sql-grid-result-format 'text)
           (sql-grid-focus-results t)
           (sql-grid-show-cell-tooltips t))
       (unwind-protect
           (progn
             (with-current-buffer source
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "UPDATE moves SET n = n + 1 RETURNING n, 'tail' AS label;")
               (setq result-buffer (sql-grid-execute-statement)))
             (with-current-buffer result-buffer
               (goto-char (sql-grid-test--button-with-help result-buffer "tail"))
               (sql-grid-move-column-left)
               (sql-grid-refresh)
              (goto-char (point-min))
              (should (re-search-forward "^| label +| n +|" nil t)))
             (should (equal (sql-grid-result-rows
                             (car (sql-grid-db-execute connection "SELECT n FROM moves")))
                            '((1))))
             (with-current-buffer source (sql-grid-execute-statement))
             (sql-grid-test--await-results result-buffer)
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (should (re-search-forward "^| n +| label +|" nil t))
               (should (re-search-forward "^| 2 +| tail +|" nil t))))
         (dolist (buffer (list result-buffer source))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-graphical-grid-column-moves ()
  "Move grid columns while preserving focused values and horizontal scrolling."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test grid columns*"))
        result-buffer
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "name" "email")
                              :rows '((1 "Ada" "ada@example.com")
                                      (2 "Lin" "lin@example.com")))))))
          (sql-grid-test--press-in-buffer
           result-buffer
           (sql-grid-test--button-with-help result-buffer "lin@example.com")
           "M-<left>")
          (with-current-buffer result-buffer
            (should (equal (button-get (button-at (point)) 'help-echo)
                           "lin@example.com"))
            (text-scale-set 2)
            (sql-grid-refresh)
            (redisplay t)
            (let ((positions (mapcar
                              (lambda (value)
                                (posn-x-y
                                 (posn-at-point
                                  (sql-grid-test--button-with-help
                                   result-buffer value))))
                              '("email" "ada@example.com" "lin@example.com"))))
              (should (apply #'= (mapcar #'car positions)))
              (should (< (cdar positions) (cdadr positions) (cdaddr positions)))))
          (sql-grid-test--press-in-buffer result-buffer (point) "M-<right>")
          (sql-grid-test--press-in-buffer result-buffer (point) "m 1 RET")
          (with-current-buffer result-buffer
            (should (equal (button-get (button-at (point)) 'help-echo)
                           "lin@example.com"))
            (save-window-excursion
              (delete-other-windows)
              (split-window-right 20)
              (sql-grid-move-column 3)
              (redisplay t)
              (should (> (window-hscroll) 0))
              (let ((x (car (posn-x-y (posn-at-point (point))))))
                (should (<= 0 x))
                (should (< x (window-body-width nil t))))
              (sql-grid-move-column 1)
              (redisplay t)
              (should (= (window-hscroll) 0)))
            (sql-grid-set-result-format 'text)
            (goto-char (point-min))
            (should (re-search-forward "^| email +| id +| name +|" nil t))))
      (dolist (buffer (list result-buffer source))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sql-grid-test-page-limit-preserves-query-and-write-semantics ()
  "Retain every returned row while preserving SQL limits and write effects."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((query "WITH RECURSIVE nums(n) AS (VALUES(1) UNION ALL SELECT n + 1 FROM nums WHERE n < 1002) SELECT n FROM nums")
           (sql-grid-max-rows 1000))
       (let ((result (car (sql-grid-db-execute connection query))))
         (should (equal (sql-grid-result-columns result) '("n")))
         (should (= (length (sql-grid-result-rows result)) 1002))
         (should (equal (car (last (sql-grid-result-rows result))) '(1002))))
       (let ((sql-grid-max-rows 2))
         (should (equal (sql-grid-result-rows
                         (car (sql-grid-db-execute connection "SELECT 7 UNION ALL SELECT 8 LIMIT 1")))
                        '((7))))
         (sql-grid-db-execute connection "CREATE TABLE limit_writes (n)")
         (sql-grid-db-execute connection "INSERT INTO limit_writes VALUES (1), (2), (3), (4)")
         (should (equal (sql-grid-result-rows
                         (car (sql-grid-db-execute connection "UPDATE limit_writes SET n = n + 10 RETURNING n")))
                        '((11) (12) (13) (14))))
         (should (equal (sql-grid-result-rows
                         (car (sql-grid-db-execute connection "SELECT count(*), sum(n) FROM limit_writes")))
                        '((4 50))))
         (let ((empty (car (sql-grid-db-execute connection "SELECT n FROM limit_writes WHERE 0"))))
           (should (equal (sql-grid-result-columns empty) '("n")))
           (should-not (sql-grid-result-rows empty)))
         (should (equal (sql-grid-result-rows
                         (car (sql-grid-db-execute connection "SELECT NULL, ''")))
                        '((nil ""))))
         (should-error (sql-grid-db-execute connection "SELECT missing FROM limit_writes")))))))

(ert-deftest sql-grid-test-row-filter-retains-values-and-state ()
  "Filter full retained values, preserving selection, copies, and SQL effects."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE filter_values (id, val, n)")
     (sql-grid-db-execute
      connection
      "INSERT INTO filter_values VALUES (1, 'prefix-HIDDEN-tail', 0), (2, 'line one' || char(10) || 'hidden target', 0), (3, NULL, 0), (4, '', 0)")
     (let ((source (generate-new-buffer " *sql-grid-test filters*"))
           result-buffer
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           (sql-grid-max-cell-chars 6)
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil))
       (unwind-protect
           (progn
             (with-current-buffer source
               (setq result-buffer
                     (sql-grid-display-results
                      connection
                      (sql-grid-db-execute connection
                                            "UPDATE filter_values SET n = n + 1 RETURNING id, val, n"))))
             (with-current-buffer result-buffer
               (goto-char (sql-grid-test--button-with-help result-buffer "line one\nhidden target"))
               (sql-grid-filter-rows "HIDDEN")
               (sql-grid-copy-cell)
               (should (equal (car kill-ring) "line one\nhidden target"))
               (should-not (sql-grid-test--button-with-help result-buffer "NULL"))
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "val [2]")))
                 (sql-grid-copy-column))
               (should (equal (car kill-ring) "prefix-HIDDEN-tail\tline one\nhidden target"))
               (sql-grid-set-result-format 'text)
               (sql-grid-refresh)
               (sql-grid-copy-cell)
               (should (equal (car kill-ring) "line one\nhidden target"))
               (let ((before (buffer-string)))
                 (should-error (sql-grid-filter-rows "[") :type 'user-error)
                 (should (equal before (buffer-string))))
               (sql-grid-filter-rows "^NULL$")
               (should (sql-grid-test--button-with-help result-buffer "NULL"))
               (should-not (sql-grid-test--button-with-help result-buffer ""))
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (_prompt choices &rest _) (caar choices))))
                 (sql-grid-copy-row))
               (should (equal (car kill-ring) "3\tNULL\t1"))
               (sql-grid-filter-rows "no-such-value")
               (should (string-match-p "(0 rows)" (buffer-string)))
               (should-error (sql-grid-copy-row) :type 'user-error)
               (sql-grid-filter-rows "")
               (should (sql-grid-test--button-with-help result-buffer ""))
               (should (sql-grid-test--button-with-help result-buffer "prefix-HIDDEN-tail")))
             (should (equal (sql-grid-result-rows
                             (car (sql-grid-db-execute connection "SELECT n FROM filter_values ORDER BY id")))
                            '((1) (1) (1) (1))))
             (with-current-buffer result-buffer
               (sql-grid-filter-rows "no-such-value"))
             (with-current-buffer source
               (sql-grid-display-results connection
                                          (sql-grid-db-execute connection "SELECT 'fresh' AS val")))
             (should (sql-grid-test--button-with-help result-buffer "fresh")))
         (dolist (buffer (list result-buffer source))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-limited-result-pagination ()
  "Page retained rows without losing values, filters, view state, or SQL effects."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE page_values (id, val, n)")
     (sql-grid-db-execute
      connection
      "INSERT INTO page_values VALUES (1, 'first-full-value', 0), (2, 'second' || char(10) || 'line', 0), (3, NULL, 0), (4, '', 0), (5, 'last-full-value', 0)")
     (let ((source (generate-new-buffer " *sql-grid-test pagination*"))
           result-buffer
           (sql-grid-max-rows 2)
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           (sql-grid-show-cell-tooltips t)
           (sql-grid-max-cell-chars 8)
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil))
       (unwind-protect
           (progn
             (with-current-buffer source
               (setq result-buffer
                     (sql-grid-display-results
                      connection
                      (sql-grid-db-execute
                       connection "UPDATE page_values SET n = n + 1 RETURNING id, val, n"))))
             (with-current-buffer result-buffer
               (should (sql-grid-test--button-with-help result-buffer "first-full-value"))
               (should (sql-grid-test--button-with-help result-buffer "second\nline"))
               (should-not (sql-grid-test--button-with-help result-buffer "NULL"))
               (should-not (sql-grid-test--button-with-help result-buffer "last-full-value"))
               (should-error (sql-grid-previous-page) :type 'user-error)
               (sql-grid-next-page)
               (should (sql-grid-test--button-with-help result-buffer "NULL"))
               (should (sql-grid-test--button-with-help result-buffer ""))
               (should-not (sql-grid-test--button-with-help result-buffer "first-full-value"))
               (goto-char (sql-grid-test--button-with-help result-buffer "NULL"))
               (sql-grid-move-column-left)
               (dolist (format (if (display-graphic-p) '(text grid) '(text)))
                 (sql-grid-set-result-format format)
                 (sql-grid-refresh)
                 (sql-grid-copy-cell)
                 (should (equal (car kill-ring) "NULL"))
                 (should (sql-grid-test--button-with-help result-buffer ""))
                 (should-not (sql-grid-test--button-with-help result-buffer "last-full-value")))
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (_prompt choices &rest _) (caar choices))))
                 (sql-grid-copy-row)
                 (should (equal (car kill-ring) "NULL\t3\t1"))
                 (sql-grid-copy-column)
                 (should (equal (car kill-ring) "NULL\t")))
               (sql-grid-next-page)
               (should (sql-grid-test--button-with-help result-buffer "last-full-value"))
               (should-not (sql-grid-test--button-with-help result-buffer "NULL"))
               (should-error (sql-grid-next-page) :type 'user-error)
               (let ((sql-grid-max-rows nil))
                 (sql-grid-refresh)
                 (sql-grid-previous-page)
                 (should (sql-grid-test--button-with-help result-buffer "NULL"))
                 (should (sql-grid-test--button-with-help result-buffer "")))
               (sql-grid-filter-rows "first-full\\|last-full")
               (should (sql-grid-test--button-with-help result-buffer "first-full-value"))
               (should (sql-grid-test--button-with-help result-buffer "last-full-value"))
               (should-not (sql-grid-test--button-with-help result-buffer "second\nline"))
               (should-error (sql-grid-next-page) :type 'user-error)
               (sql-grid-filter-rows "no-match")
               (should-error (sql-grid-next-page) :type 'user-error)
               (should-error (sql-grid-previous-page) :type 'user-error)
               (should-error (sql-grid-copy-row) :type 'user-error)
               (sql-grid-filter-rows "")
               (should (sql-grid-test--button-with-help result-buffer "first-full-value"))
               (should (sql-grid-test--button-with-help result-buffer "second\nline"))
               (sql-grid-next-page))
             (should (equal (sql-grid-result-rows
                             (car (sql-grid-db-execute
                                   connection "SELECT n FROM page_values ORDER BY id")))
                            '((1) (1) (1) (1) (1))))
             (with-current-buffer source
               (sql-grid-display-results
                connection
                (sql-grid-db-execute connection "SELECT val FROM page_values ORDER BY id")))
             (with-current-buffer result-buffer
               (should (sql-grid-test--button-with-help result-buffer "first-full-value"))
               (should-not (sql-grid-test--button-with-help result-buffer "NULL"))))
         (dolist (buffer (list result-buffer source))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-unfocused-results-start-at-first-cell ()
  "New results reset the result window's cell without moving editor point."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test unfocused cursor*"))
        result-buffer
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results nil))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (insert "-- editor position")
          (goto-char 6)
          (let ((source-window (selected-window)))
            (dotimes (_ 2)
              (setq result-buffer
                    (sql-grid-display-results
                     nil (list (sql-grid-result-create
                                :columns '("id" "name")
                                :rows '((1 "Alice") (2 "Bob"))))))
              (should (eq (selected-window) source-window))
              (should (= (point) 6))
              (let ((window (get-buffer-window result-buffer)))
                (should window)
                (with-current-buffer result-buffer
                  (should (equal (get-text-property (window-point window)
                                                    'sql-grid-cell)
                                 '(0 0 0 nil))))
                (sql-grid-test--press-in-buffer
                 result-buffer (window-point window) "TAB")
                (select-window source-window)))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (kill-buffer source))))

(ert-deftest sql-grid-test-pagination-preserves-cell-position ()
  "Keep page-local row and original column across pagination in each format."
  (let ((source (generate-new-buffer " *sql-grid-test page cursor*"))
        result-buffer
        (sql-grid-max-rows 2)
        (sql-grid-focus-results t)
        (sql-grid-show-cell-tooltips t)
        (sql-grid-result-format (if (display-graphic-p) 'grid 'text))
        (formats (if (display-graphic-p) '(text grid) '(text))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("left" "middle" "right")
                              :rows '((1 "a" "A") (2 "b" "B")
                                      (3 "c" "C") (4 "d" "D")
                                      (5 "e" "E")))))))
          (with-current-buffer result-buffer
            (let ((cell (button-at (point))))
              (should cell)
              (should (equal (button-get cell 'sql-grid-cell) '(0 0 0 nil)))))
          (dolist (format formats)
            (with-current-buffer result-buffer
              (sql-grid-set-result-format format)
              (sql-grid-set-max-rows 2)
              (let ((first (sql-grid-test--button-with-help result-buffer "left")))
                (should first)
                (should (equal (button-get (button-at first) 'sql-grid-cell)
                               '(0 0 0 nil)))
                (when (eq format 'grid)
                  (redisplay t)
                  (let ((cell (button-at (point))))
                    (should cell)
                    (should (= (overlay-start sql-grid--cursor-overlay)
                               (button-start cell)))
                    (should (= (overlay-end sql-grid--cursor-overlay)
                               (button-end cell)))))
                (goto-char (sql-grid-test--button-with-help result-buffer "b"))
                (sql-grid-test--press-in-buffer result-buffer (point) "]")
                (should (equal (button-get (button-at (point)) 'sql-grid-value) "d"))
                (should (= (nth 2 (button-get (button-at (point)) 'sql-grid-cell)) 1))
                (sql-grid-test--press-in-buffer result-buffer (point) "[")
                (should (equal (button-get (button-at (point)) 'sql-grid-value) "b"))
                (goto-char (sql-grid-test--button-with-help result-buffer "middle"))
                (sql-grid-test--press-in-buffer result-buffer (point) "]")
                (should (equal (button-get (button-at (point)) 'sql-grid-value) "middle"))
                (should (= (nth 2 (button-get (button-at (point)) 'sql-grid-cell)) 1))
                (goto-char (sql-grid-test--button-with-help result-buffer "d"))
                (sql-grid-test--press-in-buffer result-buffer (point) "]")
                (should (equal (button-get (button-at (point)) 'sql-grid-value) "e"))
                (should (= (nth 2 (button-get (button-at (point)) 'sql-grid-cell)) 1))
                (when (eq format 'grid)
                  (redisplay t)
                  (let ((cell (button-at (point))))
                    (should cell)
                    (should (= (overlay-start sql-grid--cursor-overlay)
                               (button-start cell)))
                    (should (= (overlay-end sql-grid--cursor-overlay)
                               (button-end cell)))))))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest sql-grid-test-pagination-isolates-result-sets-and-buffers ()
  "Keep independent page positions, with all rows visible when the limit is nil."
  (let ((source (generate-new-buffer " *sql-grid-test page sets*"))
        (other-source (generate-new-buffer " *sql-grid-test page buffers*"))
        result-buffer other-buffer
        (sql-grid-max-rows 2)
        (sql-grid-result-format 'text)
        (sql-grid-focus-results nil)
        (sql-grid-show-cell-tooltips t)
        (results (list (sql-grid-result-create :columns '("left")
                                                :rows '(("a") ("b") ("c")))
                       (sql-grid-result-create :columns '("right")
                                                :rows '(("x") ("y") ("z"))))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer (sql-grid-display-results nil results)))
          (with-current-buffer other-source
            (setq other-buffer (sql-grid-display-results nil results)))
          (with-current-buffer result-buffer
            (goto-char (sql-grid-test--button-with-help result-buffer "x"))
            (sql-grid-next-page)
            (should (sql-grid-test--button-with-help result-buffer "z"))
            (should-not (sql-grid-test--button-with-help result-buffer "x"))
            (should (sql-grid-test--button-with-help result-buffer "a"))
            (sql-grid-previous-page)
            (should (sql-grid-test--button-with-help result-buffer "x"))
            (goto-char (sql-grid-test--button-with-help result-buffer "a"))
            (sql-grid-next-page)
            (should (sql-grid-test--button-with-help result-buffer "c"))
            (should (sql-grid-test--button-with-help result-buffer "x")))
          (should (sql-grid-test--button-with-help other-buffer "a"))
          (should-not (sql-grid-test--button-with-help other-buffer "c"))
          (with-current-buffer source
            (let ((sql-grid-max-rows nil))
              (sql-grid-display-results nil results)))
          (with-current-buffer result-buffer
            (dolist (value '("a" "b" "c" "x" "y" "z"))
              (should (sql-grid-test--button-with-help result-buffer value)))
            (should-error (sql-grid-next-page) :type 'user-error))
          (with-current-buffer source
            (sql-grid-display-results
             nil (list (sql-grid-result-create :columns '("empty")))))
          (with-current-buffer result-buffer
            (should (sql-grid-test--button-with-help result-buffer "empty"))
            (should-error (sql-grid-next-page) :type 'user-error)
            (should-error (sql-grid-previous-page) :type 'user-error))
          (with-current-buffer source (sql-grid-display-results nil nil))
          (with-current-buffer result-buffer
            (should-error (sql-grid-next-page) :type 'user-error)))
      (dolist (buffer (list result-buffer other-buffer source other-source))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sql-grid-test-invalid-page-limit-does-not-execute ()
  "Reject invalid page sizes before running a write or replacing results."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE invalid_page_limit (n)")
     (let ((source (generate-new-buffer " *sql-grid-test invalid page limit*"))
           result-buffer
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           (sql-grid-connections
            `(("Page limit" :backend sqlite
               :database ,(sql-grid-connection-database connection)))))
       (unwind-protect
           (progn
             (with-current-buffer source
               (insert "INSERT INTO invalid_page_limit VALUES (1);")
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "Page limit")))
                 (sql-grid-connect-buffer))
               (setq result-buffer
                     (sql-grid-display-results
                      connection (list (sql-grid-result-create
                                        :columns '("val") :rows '(("original"))))))
               (dolist (invalid '(0 -1 1.5 "3"))
                 (let ((sql-grid-max-rows invalid))
                   (should-error (sql-grid-execute-statement) :type 'user-error)
                   (should-error
                    (sql-grid-display-results
                     nil (list (sql-grid-result-create :columns '("val") :rows '(("new")))))
                    :type 'user-error))))
             (should (sql-grid-test--button-with-help result-buffer "original"))
             (should-not (sql-grid-test--button-with-help result-buffer "new"))
             (should (equal (sql-grid-result-rows
                             (car (sql-grid-db-execute
                                   connection "SELECT count(*) FROM invalid_page_limit")))
                            '((0)))))
         (dolist (buffer (list result-buffer source))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-csv-export-retains-full-filtered-sorted-values ()
  "Export original values across filtered pages in visible column and sort order."
  (let* ((sql-grid-max-rows 1)
         (sql-grid-result-format 'text)
         (sql-grid-focus-results nil)
         (source (generate-new-buffer " *sql-grid-test csv export*"))
         (path (make-temp-file "sql-grid-csv-" nil ".csv"))
         (long-value (concat "keep,\"Khmer: ខ្មែរ\"\r\n" (make-string 100 ?x)))
         result-buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil
                   (list (sql-grid-result-create
                          :columns '("rank" "payload" "nullable")
                          :rows `((4 "keep-empty" "")
                                  (3 "keep,third" "NULL")
                                  (1 "skip" "")
                                  (2 ,long-value nil)))))))
          (with-current-buffer result-buffer
            (sql-grid-filter-rows "keep")
            (sql-grid-sort-column 0)
            (goto-char (sql-grid-test--button-with-help result-buffer "rank"))
            (sql-grid-move-column 3)
            (goto-char (sql-grid-test--button-with-help result-buffer "nullable"))
            (sql-grid-move-column 1)
            (sql-grid-export-csv path))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (should
             (equal
              (buffer-string)
              (encode-coding-string
               (concat "nullable,payload,rank\r\n"
                       ",\"keep,\"\"Khmer: ខ្មែរ\"\"\r\n"
                       (make-string 100 ?x)
                       "\",2\r\n"
                       "\"NULL\",\"keep,third\",3\r\n"
                       "\"\",keep-empty,4\r\n")
               'utf-8-unix)))))
      (when (file-exists-p path) (delete-file path))
      (dolist (buffer (list result-buffer source))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sql-grid-test-multi-sort-keeps-key-precedence-and-stable-ties ()
  "Sort status then date across pages, keeping original order for equal keys."
  (let* ((source (generate-new-buffer " *sql-grid-test multi sort*"))
         (sql-grid-result-format 'text)
         (sql-grid-focus-results nil)
         (sql-grid-max-rows 2)
         (path (make-temp-file "sql-grid-multi-sort-"))
         buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("status" "date" "id")
                              :rows '(("open" "2026-10-02" 1)
                                      ("closed" "2026-10-03" 2)
                                      ("open" "2026-10-01" 3)
                                      ("open" "2026-10-01" 4)
                                      ("closed" "2026-10-01" 5)))))))
          (with-current-buffer buffer
            (sql-grid-sort-column 0)
            (sql-grid-add-sort-column 1)
            (should (equal (sql-grid-test--button-visible-label buffer "status")
                           "status ↑1"))
            (should (equal (sql-grid-test--button-visible-label buffer "date")
                           "date ↑2"))
            (sql-grid-export-csv path)
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal (buffer-string)
                             (concat "status,date,id\n"
                                     "closed,2026-10-01,5\n"
                                     "closed,2026-10-03,2\n"
                                     "open,2026-10-01,3\n"
                                     "open,2026-10-01,4\n"
                                     "open,2026-10-02,1\n"))))
            (sql-grid-add-sort-column 1)
            (should (equal (sql-grid-test--button-visible-label buffer "status")
                           "status ↑1"))
            (should (equal (sql-grid-test--button-visible-label buffer "date")
                           "date ↓2"))
            (sql-grid-export-csv path)
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal (buffer-string)
                             (concat "status,date,id\n"
                                     "closed,2026-10-03,2\n"
                                     "closed,2026-10-01,5\n"
                                     "open,2026-10-02,1\n"
                                     "open,2026-10-01,3\n"
                                     "open,2026-10-01,4\n"))))
            ;; Activating the primary header replaces precedence with one
            ;; descending key, which the export must reflect.
            (button-activate
             (button-at (sql-grid-test--button-with-help buffer "status")))
            (should (equal (sql-grid-test--button-visible-label buffer "status")
                           "status ↓1"))
            (should (equal (sql-grid-test--button-visible-label buffer "date")
                           "date"))
            (sql-grid-export-csv path)
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal (buffer-string)
                             (concat "status,date,id\n"
                                     "open,2026-10-02,1\n"
                                     "open,2026-10-01,3\n"
                                     "open,2026-10-01,4\n"
                                     "closed,2026-10-03,2\n"
                                     "closed,2026-10-01,5\n"))))
            ;; Appending date restores a numbered secondary ascending key.
            (sql-grid-add-sort-column 1)
            (should (equal (sql-grid-test--button-visible-label buffer "status")
                           "status ↓1"))
            (should (equal (sql-grid-test--button-visible-label buffer "date")
                           "date ↑2"))
            (sql-grid-export-csv path)
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal (buffer-string)
                             (concat "status,date,id\n"
                                     "open,2026-10-01,3\n"
                                     "open,2026-10-01,4\n"
                                     "open,2026-10-02,1\n"
                                     "closed,2026-10-01,5\n"
                                     "closed,2026-10-03,2\n"))))))
      (delete-file path)
      (dolist (item (list buffer source))
        (when (buffer-live-p item) (kill-buffer item))))))

(ert-deftest sql-grid-test-sqlite-async-shares-transaction-and-counts-writes ()
  "Keep asynchronous writes and synchronous reads in one SQLite transaction."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TEMP TABLE async_values (id INTEGER)")
     (sql-grid-db-execute connection "BEGIN")
     (let (done results failure)
       (sql-grid-db-execute-async
        connection "INSERT INTO async_values VALUES (10), (20) RETURNING id"
        (lambda (value error-data)
          (setq results value failure error-data done t)))
       (should-not done)
       (let ((deadline (+ (float-time) 10)))
         (while (and (not done) (< (float-time) deadline))
           (accept-process-output nil 0.01)))
       (should done)
       (should-not failure)
       (should (equal (sql-grid-result-rows (car results)) '((10) (20))))
       (should (= (sql-grid-result-affected-rows (car results)) 2))
       (should (>= (sql-grid-result-elapsed (car results)) 0)))
     (should (equal (sql-grid-result-rows
                     (car (sql-grid-db-execute connection "SELECT id FROM async_values")))
                    '((10) (20))))
     (let ((result (car (sql-grid-db-execute
                        connection "CREATE TEMP TABLE not_a_write (id INTEGER)"))))
       (should-not (equal (sql-grid-result-affected-rows result) 2)))
     (sql-grid-db-execute connection "ROLLBACK")
     (should (equal (sql-grid-result-rows
                     (car (sql-grid-db-execute connection "SELECT count(*) FROM async_values")))
                    '((0)))))))

(ert-deftest sql-grid-test-sqlite-async-leading-invalid-input-stays-usable ()
  "Report leading invalid SQL once and keep the worker usable."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (dolist (sql '("(" ".tables"))
       (let ((calls 0) done results failure)
         (sql-grid-db-execute-async
          connection sql
          (lambda (value error-data)
            (cl-incf calls)
            (setq results value failure error-data done t)))
         (should (sql-grid-test--wait-for (lambda () done) 10))
         (should (= calls 1))
         (should-not results)
         (should failure)
         (should-not (eq (car failure) 'quit))))
     (let ((calls 0) done results failure)
       (sql-grid-db-execute-async
        connection "SELECT 41 + 1 AS answer"
        (lambda (value error-data)
          (cl-incf calls)
          (setq results value failure error-data done t)))
       (should (sql-grid-test--wait-for (lambda () done) 10))
       (should (= calls 1))
       (should-not failure)
       (should (equal (sql-grid-result-rows (car results)) '((42))))))))

(ert-deftest sql-grid-test-sqlite-async-cancel-callback-once-and-rollback ()
  "Cancel a running SQLite job once, invalidate the connection, and roll back."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE marker (id INTEGER)")
     (sql-grid-db-execute connection "BEGIN")
     (sql-grid-db-execute connection "INSERT INTO marker VALUES (1)")
     (let ((calls 0) (results :unset) (failure :unset))
       (let ((job (sql-grid-db-execute-async
                   connection (sql-grid-test--slow-query)
                   (lambda (value error-data)
                     (cl-incf calls)
                     (setq results value failure error-data)))))
         (accept-process-output nil 0.05)
         (should (= calls 0))
         (sql-grid-db-cancel job)
         (should (= calls 1))
         (should-not results)
         (should (eq (car failure) 'quit))
         ;; Cancelling or disconnecting again must not invoke the callback.
         (sql-grid-db-cancel job)
         (sql-grid-db-disconnect connection)
         (should (= calls 1))
         (should-error (sql-grid-db-execute connection "SELECT 1")))
       ;; Closing the worker discarded the uncommitted transaction.
       (let ((reconnected (sql-grid-connection-create
                           :backend 'sqlite
                           :database (sql-grid-connection-database connection))))
         (unwind-protect
             (progn
               (sql-grid-db-connect reconnected)
               (should (equal
                        (sql-grid-result-rows
                         (car (sql-grid-db-execute
                               reconnected "SELECT count(*) FROM marker")))
                        '((0))))
               ;; Cancelling an already completed job does nothing.
               (let ((completed-calls 0) done)
                 (let ((job (sql-grid-db-execute-async
                             reconnected "SELECT 1"
                             (lambda (_results _error)
                               (cl-incf completed-calls)
                               (setq done t)))))
                   (should (sql-grid-test--wait-for (lambda () done) 10))
                   (should (= completed-calls 1))
                   (sql-grid-db-cancel job)
                   (should (= completed-calls 1)))))
           (sql-grid-db-disconnect reconnected)))))))

(ert-deftest sql-grid-test-sqlite-disconnect-settles-callbacks-and-reconnects ()
  "Reject callback-reentrant work, settle later callbacks, and reconnect."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((first-calls 0) (second-calls 0)
           connect-rejection query-rejection first-job second-job)
       (setq first-job
             (sql-grid-db-execute-async
              connection (sql-grid-test--slow-query)
              (lambda (_result _failure)
                (cl-incf first-calls)
                (setq connect-rejection
                      (condition-case error-data
                          (progn (sql-grid-db-connect connection) nil)
                        (user-error error-data))
                      query-rejection
                      (condition-case error-data
                          (progn
                            (sql-grid-db-execute-async
                             connection "SELECT 2" #'ignore)
                            nil)
                        (user-error error-data))))))
       (setq second-job
             (sql-grid-db-execute-async
              connection "SELECT 3"
              (lambda (_result _failure) (cl-incf second-calls))))
       (should first-job)
       (should second-job)
       (sql-grid-db-disconnect connection)
       (should (= first-calls 1))
       (should (= second-calls 1))
       (should (sql-grid-job-completed first-job))
       (should (sql-grid-job-completed second-job))
       (should (eq (car connect-rejection) 'user-error))
       (should (eq (car query-rejection) 'user-error))
       (should-not (sql-grid-connection-handle connection))
       (sql-grid-db-connect connection)
       (should (equal
                (sql-grid-result-rows
                 (car (sql-grid-db-execute connection "SELECT 7")))
                '((7))))
       (sql-grid-db-disconnect connection)))))

(ert-deftest sql-grid-test-sqlite-disconnect-callback-error-does-not-strand-jobs ()
  "Finish teardown and later pending callbacks when one callback throws."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((calls 0) callback-error first-job second-job)
       (setq first-job
             (sql-grid-db-execute-async
              connection (sql-grid-test--slow-query)
              (lambda (_result _failure)
                (cl-incf calls)
                (error "SQL_GRID_TEST_CALLBACK_FAILURE"))))
       (setq second-job
             (sql-grid-db-execute-async
              connection "SELECT 3"
              (lambda (_result _failure)
                (cl-incf calls)
                (error "SQL_GRID_TEST_SECOND_CALLBACK_FAILURE"))))
       (should first-job)
       (should second-job)
       (condition-case error-data
           (sql-grid-db-disconnect connection)
         (error (setq callback-error error-data)))
       (should callback-error)
       (should (= calls 2))
       (should (sql-grid-job-completed first-job))
       (should (sql-grid-job-completed second-job))
       (should-not (sql-grid-connection-handle connection))
       (sql-grid-db-connect connection)
       (should (equal
                (sql-grid-result-rows
                 (car (sql-grid-db-execute connection "SELECT 9")))
                '((9))))))))

(ert-deftest sql-grid-test-async-running-cancel-retains-results-and-rolls-back ()
  "Keep Emacs responsive while a SQLite query runs, then cancel and roll back."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE keep (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO keep VALUES ('old value')")
     (let ((editor (generate-new-buffer " *sql-grid-test cancel*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result timer ticked)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT v FROM keep")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "old value")))
             ;; Leave an uncommitted row so cancellation must close the
             ;; worker connection rather than commit it.
             (sql-grid-db-execute connection "BEGIN")
             (sql-grid-db-execute connection "INSERT INTO keep VALUES ('uncommitted')")
             (with-current-buffer editor
               (erase-buffer)
               (insert (sql-grid-test--slow-query))
               (setq timer
                     (run-at-time 0.05 nil (lambda () (setq ticked t))))
               (should (eq (sql-grid-execute-statement) result))
               (should (eq (current-buffer) editor))
               (let ((window (get-buffer-window result)))
                 (should window)
                 (should (eq (window-buffer window) result)))
               (should (sql-grid-test--wait-for (lambda () ticked) 5))
               (should (eq sql-grid--execution-state 'running))
               (should sql-grid--execution-job)
               (should (timerp sql-grid--execution-timer))
               (should (string-suffix-p
                        " | C-c C-k: cancel (closes SQLite connection)"
                        (sql-grid--execution-status)))
               (should-not (buffer-local-value 'sql-grid--execution-job result))
               (let ((prompted 0) messages)
                 (setq messages
                       (sql-grid-test--messages-during
                        (lambda ()
                          (cl-letf (((symbol-function 'yes-or-no-p)
                                     (lambda (_prompt)
                                       (cl-incf prompted)
                                       t)))
                            (sql-grid-cancel)))))
                 (should (= prompted 0))
                 (should (equal
                          (car (last messages))
                          "SQLite query cancelled and connection closed; transaction rolled back.  Reconnect with M-x sql-grid-connect-buffer")))
               (should-not sql-grid--execution-job)
               (should-not sql-grid--execution-timer)
               (should (eq sql-grid--execution-state 'cancelled))
               (should-not sql-grid--connection)
               (should (equal
                        sql-grid--execution-error
                        "connection closed; transaction rolled back; reconnect with M-x sql-grid-connect-buffer")))
             (with-current-buffer result
               (should (sql-grid-test--button-with-help result "old value"))
               (should-not sql-grid--connection)
               (should (string-prefix-p
                        "Cancelled: connection closed; transaction rolled back"
                        (sql-grid--header-line))))
             ;; The public reconnect path reaches the same database and sees
             ;; only committed rows.
             (with-current-buffer editor
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "sqlite"))
                         ((symbol-function 'read-string)
                          (lambda (&rest _)
                            (sql-grid-connection-database connection))))
                 (sql-grid-connect-buffer))
               (should sql-grid--connection)
               (erase-buffer)
               (insert "SELECT count(*) AS remaining FROM keep")
               (setq result (sql-grid-execute-statement)))
             (should (sql-grid-test--button-with-help result "1")))
         (when (timerp timer) (cancel-timer timer))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-async-focus-results-selects-and-cancels-from-both ()
  "Select the result buffer on execute and cancel from result or source."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test focus*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results t)
           result timer ticked)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT 7 AS answer")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "7")))
             (with-current-buffer editor
               (erase-buffer)
               (insert (sql-grid-test--slow-query))
               (setq timer
                     (run-at-time 0.05 nil (lambda () (setq ticked t))))
               (setq result (sql-grid-execute-statement))
               (should (eq (current-buffer) result))
               (should (eq (window-buffer (selected-window)) result)))
             (should (sql-grid-test--wait-for (lambda () ticked) 5))
             (with-current-buffer editor
               (should (eq sql-grid--execution-state 'running))
               (should sql-grid--execution-job)
               (should (timerp sql-grid--execution-timer)))
             (should-not (buffer-local-value 'sql-grid--execution-job result))
             ;; The result buffer owns cancellation dispatch but not the job.
             (with-current-buffer result (sql-grid-cancel))
             (with-current-buffer editor
               (should (eq sql-grid--execution-state 'cancelled))
               (should-not sql-grid--execution-job)
               (should-not sql-grid--execution-timer)
               (should-not sql-grid--connection))
             (with-current-buffer result
               (should (sql-grid-test--button-with-help result "7")))
             (when (timerp timer) (cancel-timer timer))
             (setq timer nil ticked nil)
             (with-current-buffer editor
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "sqlite"))
                         ((symbol-function 'read-string)
                          (lambda (&rest _)
                            (sql-grid-connection-database connection))))
                 (sql-grid-connect-buffer))
               (should sql-grid--connection)
               (erase-buffer)
               (insert (sql-grid-test--slow-query))
               (setq timer
                     (run-at-time 0.05 nil (lambda () (setq ticked t))))
               (setq result (sql-grid-execute-statement))
               (should (eq (current-buffer) result)))
             (should (sql-grid-test--wait-for (lambda () ticked) 5))
             (with-current-buffer editor
               (should (eq sql-grid--execution-state 'running))
               (sql-grid-cancel)
               (should (eq sql-grid--execution-state 'cancelled))
               (should-not sql-grid--execution-job)
               (should-not sql-grid--connection)))
         (when (timerp timer) (cancel-timer timer))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-idle-worker-loss-invalidates-editor ()
  "Invalidate an idle editor and retain old rows after worker loss."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE keep (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO keep VALUES ('kept value')")
     (let ((editor (generate-new-buffer " *sql-grid-test worker loss*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT v FROM keep")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "kept value"))
               (sql-grid-schema-refresh)
               (should (sql-grid-test--wait-for
                        (lambda ()
                          (with-current-buffer editor
                            (null sql-grid-schema--job)))))
               (should (with-current-buffer editor sql-grid-schema--loaded)))
             (let ((socket (sql-grid-connection-handle connection))
                   settled messages)
               (should socket)
               (setq messages
                     (sql-grid-test--messages-during
                      (lambda ()
                        (delete-process socket)
                        (setq settled
                              (sql-grid-test--wait-for
                               (lambda ()
                                 (with-current-buffer editor
                                   (null sql-grid--connection)))
                               10)))))
               (should settled)
               (should (equal
                        (car (last messages))
                        "SQLite connection lost; uncommitted transaction rolled back.  Reconnect with M-x sql-grid-connect-buffer")))
             (with-current-buffer editor
               (should-not sql-grid--connection)
               (should-not sql-grid--owns-connection)
               (should-not sql-grid--execution-job)
               (should-not sql-grid--execution-generation)
               (should-not sql-grid--execution-start)
               (should-not sql-grid--execution-timer)
               (should (eq sql-grid--execution-state 'failed))
               (should (equal sql-grid--execution-error
                              "connection lost; transaction rolled back; reconnect with M-x sql-grid-connect-buffer"))
               (should-not sql-grid-schema--loaded)
               (should-not (sql-grid-completion-at-point)))
             (with-current-buffer result
               (should-not sql-grid--connection)
               (should (sql-grid-test--buffer-contains-p result "kept value"))
               (should (string-match-p "Failed: connection lost"
                                       (sql-grid--header-line))))
             (should-not (sql-grid-connection-handle connection)))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-memory-idle-worker-loss-discards-database ()
  "Report an in-memory database loss after an unexpected worker close."
  (sql-grid-test--with-memory-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE keep (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO keep VALUES ('kept value')")
     (let ((editor (generate-new-buffer " *sql-grid-test memory loss*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT v FROM keep")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "kept value")))
             (let (settled messages)
               (setq messages
                     (sql-grid-test--messages-during
                      (lambda ()
                        (delete-process (sql-grid-connection-handle connection))
                        (setq settled
                              (sql-grid-test--wait-for
                               (lambda ()
                                 (with-current-buffer editor
                                   (null sql-grid--connection)))
                               10)))))
               (should settled)
               (should (equal
                        (car (last messages))
                        "SQLite connection lost; in-memory database discarded.  Reconnect with M-x sql-grid-connect-buffer")))
             (with-current-buffer editor
               (should (eq sql-grid--execution-state 'failed))
               (should (equal
                        sql-grid--execution-error
                        "connection lost; in-memory database discarded; reconnect with M-x sql-grid-connect-buffer")))
             (with-current-buffer result
               (should-not sql-grid--connection)
               (should (sql-grid-test--buffer-contains-p result "kept value"))
               (should (string-match-p "Failed: connection lost"
                                       (sql-grid--header-line)))))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-active-query-worker-loss-reports-single-failure ()
  "Report one failed state when the worker dies during an active query."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE keep (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO keep VALUES ('kept value')")
     (let ((editor (generate-new-buffer " *sql-grid-test active loss*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT v FROM keep")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "kept value"))
               (erase-buffer)
               (insert (sql-grid-test--slow-query))
               (sql-grid-execute-statement)
               (should (eq sql-grid--execution-state 'running))
               (should sql-grid--execution-job))
             (let (settled messages)
               (setq messages
                     (sql-grid-test--messages-during
                      (lambda ()
                        (delete-process (sql-grid-connection-handle connection))
                        (setq settled
                              (sql-grid-test--wait-for
                               (lambda ()
                                 (with-current-buffer editor
                                   (eq sql-grid--execution-state 'failed)))
                               10)))))
               (should settled)
               (should (equal
                        (car (last messages))
                        "SQLite worker disconnected; transaction rolled back.  Reconnect with M-x sql-grid-connect-buffer")))
             (with-current-buffer editor
               (should-not sql-grid--connection)
               (should-not sql-grid--execution-job)
               (should-not sql-grid--execution-timer)
               (should (equal sql-grid--execution-error
                              "connection lost; transaction rolled back; reconnect with M-x sql-grid-connect-buffer")))
             (with-current-buffer result
               (should-not sql-grid--connection)
               (should (sql-grid-test--buffer-contains-p result "kept value"))))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-explicit-disconnect-does-not-run-worker-close ()
  "Explicit disconnect clears, but never invokes, the worker on-close callback."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (let ((calls 0))
       (setf (sql-grid-connection-on-close connection)
             (lambda (_closed) (cl-incf calls)))
       (sql-grid-db-disconnect connection)
       (should (= calls 0))
       (should-not (sql-grid-connection-on-close connection))))))

(ert-deftest sql-grid-test-memory-cancel-decline-keeps-query-running ()
  "Declining the in-memory discard prompt leaves the query and data alive."
  (sql-grid-test--with-memory-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE keep (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO keep VALUES ('kept value')")
     (let ((editor (generate-new-buffer " *sql-grid-test memory decline*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result (prompted 0) messages)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT v FROM keep")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "kept value"))
               (erase-buffer)
               (insert (sql-grid-test--slow-query))
               (sql-grid-execute-statement)
               (should (eq sql-grid--execution-state 'running))
               (should sql-grid--execution-job)
               (should (string-suffix-p
                        " | C-c C-k: cancel (closes connection; discards in-memory database)"
                        (sql-grid--execution-status)))
               (setq messages
                     (sql-grid-test--messages-during
                      (lambda ()
                        (cl-letf (((symbol-function 'yes-or-no-p)
                                   (lambda (_prompt)
                                     (cl-incf prompted)
                                     nil)))
                          (sql-grid-cancel)))))
               (should (= prompted 1))
               (should (equal (car (last messages)) "SQL query left running"))
               (should sql-grid--execution-job)
               (should (eq sql-grid--execution-state 'running))
               (should sql-grid--connection)
               (should (sql-grid-connection-handle connection))
               (should (timerp sql-grid--execution-timer)))
             (should (sql-grid-test--buffer-contains-p result "kept value"))
             (with-current-buffer editor
               (when sql-grid--execution-job
                 (sql-grid-db-cancel sql-grid--execution-job))))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-memory-cancel-accept-discards-database ()
  "Accepting the in-memory prompt closes the connection and discards data."
  (sql-grid-test--with-memory-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE mem_probe (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO mem_probe VALUES ('probe')")
     (sql-grid-db-execute connection "CREATE TABLE keep (v TEXT)")
     (sql-grid-db-execute connection "INSERT INTO keep VALUES ('kept value')")
     (let ((editor (generate-new-buffer " *sql-grid-test memory accept*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result (prompted 0) messages)
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert "SELECT v FROM keep")
               (setq result (sql-grid-execute-statement))
               (sql-grid-test--await-results result)
               (should (sql-grid-test--button-with-help result "kept value"))
               (erase-buffer)
               (insert (sql-grid-test--slow-query))
               (sql-grid-execute-statement)
               (should (eq sql-grid--execution-state 'running))
               (setq messages
                     (sql-grid-test--messages-during
                      (lambda ()
                        (cl-letf (((symbol-function 'yes-or-no-p)
                                   (lambda (_prompt)
                                     (cl-incf prompted)
                                     t)))
                          (sql-grid-cancel)))))
               (should (= prompted 1))
               (should (equal
                        (car (last messages))
                        "SQLite query cancelled; in-memory database discarded.  Reconnect with M-x sql-grid-connect-buffer"))
               (should-not sql-grid--execution-job)
               (should-not sql-grid--execution-timer)
               (should (eq sql-grid--execution-state 'cancelled))
               (should-not sql-grid--connection)
               (should (equal
                        sql-grid--execution-error
                        "connection closed; in-memory database discarded; reconnect with M-x sql-grid-connect-buffer")))
             (should-not (sql-grid-connection-handle connection))
             (with-current-buffer result
               (should-not sql-grid--connection)
               (should (sql-grid-test--button-with-help result "kept value"))
               (should (string-prefix-p
                        "Cancelled: connection closed; in-memory database discarded"
                        (sql-grid--header-line))))
             ;; A fresh in-memory connection cannot see the discarded table.
             (let ((fresh (sql-grid-connection-create
                           :backend 'sqlite :database ":memory:")))
               (unwind-protect
                   (progn
                     (sql-grid-db-connect fresh)
                     (should (equal
                              (sql-grid-result-rows
                               (car (sql-grid-db-execute
                                     fresh
                                     "SELECT count(*) FROM sqlite_master WHERE name = 'mem_probe'")))
                              '((0)))))
                 (sql-grid-db-disconnect fresh))))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (dolist (buffer (list result editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-memory-buffer-kill-cancels-without-prompt ()
  "Killing a source buffer tears down a running in-memory query unprompted."
  (sql-grid-test--with-memory-sqlite
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test memory kill*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil)
           result (prompted 0))
       (unwind-protect
           (progn
             (with-current-buffer editor
               (sql-grid-mode)
               (sql-grid--attach-connection connection)
               (insert (sql-grid-test--slow-query))
               (setq result (sql-grid-execute-statement))
               (should (eq sql-grid--execution-state 'running))
               (should sql-grid--execution-job))
             (cl-letf (((symbol-function 'yes-or-no-p)
                        (lambda (_prompt)
                          (cl-incf prompted)
                          t)))
               (kill-buffer editor))
             (should (= prompted 0))
             (should-not (sql-grid-connection-handle connection))
             (should-not (sql-grid-connection-process connection))
             (should-not (sql-grid-connection-on-close connection)))
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (when sql-grid--connection
               (ignore-errors (sql-grid-disconnect)))))
         (when (buffer-live-p result) (kill-buffer result)))))))

(ert-deftest sql-grid-test-sqlite-schema-browser-shows-partial-index-predicate ()
  "Show real partial-index predicate despite WHERE inside quoted expressions."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection
                           "CREATE TABLE events (id INTEGER, note TEXT)")
     (sql-grid-db-execute
      connection
      "CREATE INDEX events_partial ON events (lower(\"where\"), id) where note <> 'WHERE'")
     (let ((editor (generate-new-buffer " *sql-grid-test partial index*"))
           browser)
       (unwind-protect
           (with-current-buffer editor
             (sql-grid-mode)
             (sql-grid--attach-connection connection)
             (sql-grid-browse-schema)
             (set-buffer editor)
             (should (sql-grid-test--wait-for
                      (lambda ()
                        (with-current-buffer editor
                          (null sql-grid-schema--job)))))
             (setq browser (sql-grid-schema-browser-buffer editor))
             (with-current-buffer browser
               (let ((text (buffer-string)))
                 (should (string-match-p "events_partial" text))
                 (should (string-match-p "where note <> 'WHERE'" text))
                 (should-not (string-match-p
                              "where.*lower" text)))))
         (dolist (buffer (list browser editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-schema-browser-preserves-editor-and-completes-names ()
  "Browse actual constraints without editing SQL, then complete SQL identifiers."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE orders (id INTEGER PRIMARY KEY)")
     (sql-grid-db-execute
      connection "CREATE TABLE \"odd table\" (\"ship date\" TEXT, order_id REFERENCES orders(id))")
     (sql-grid-db-execute connection "CREATE UNIQUE INDEX ship_index ON \"odd table\" (\"ship date\")")
     (let ((editor (generate-new-buffer " *sql-grid-test schema*"))
           browser)
       (unwind-protect
           (with-current-buffer editor
             (sql-grid-mode)
             (sql-grid--attach-connection connection)
             (insert "SELECT ord")
             (sql-grid-browse-schema)
             ;; Browsing selects the browser; return to the editor before
             ;; inspecting or waiting for its buffer-local discovery job.
             (set-buffer editor)
             (should (sql-grid-test--wait-for
                      (lambda ()
                        (with-current-buffer editor
                          (null sql-grid-schema--job)))))
             (should (equal (buffer-string) "SELECT ord"))
             (setq browser (sql-grid-schema-browser-buffer editor))
             (should (buffer-live-p browser))
             (with-current-buffer browser
               (let ((text (buffer-string)))
                 (should (string-match-p "orders" text))
                 (should (string-match-p "id INTEGER" text))
                 (should (string-match-p "PRIMARY KEY" text))
                 (should (string-match-p "ship date" text))
                 (should (string-match-p "order_id" text))
                 (should (string-match-p "ship_index" text))
                 (should (string-match-p "UNIQUE" text))
                 (should (string-match-p
                          "FOREIGN KEY order_id -> orders.id" text))))
             (let ((completion (sql-grid-completion-at-point)))
               (should completion)
               (should (equal (buffer-substring-no-properties
                               (nth 0 completion) (nth 1 completion))
                              "ord"))
               ;; The prefix is ambiguous: both an unqualified table and a
               ;; column must remain available for standard completion.
               (let ((matches (sql-grid-test--completion-matches completion)))
                 (should (member "orders" matches))
                 (should (member "order_id" matches))))
             (should (equal (buffer-string) "SELECT ord"))
             (erase-buffer)
             (insert "SELECT \"odd t")
             (let ((completion (sql-grid-completion-at-point)))
               (should completion)
               (should (equal (buffer-substring-no-properties
                               (nth 0 completion) (nth 1 completion))
                              "\"odd t"))
               (should (member "\"odd table\""
                               (sql-grid-test--completion-matches
                                completion)))))
         (dolist (buffer (list browser editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-schema-completion-lexical-contexts ()
  "Complete outside strings and comments with quoting and qualification."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "CREATE TABLE orders (id INTEGER PRIMARY KEY, order_id INTEGER)")
     (sql-grid-db-execute
      connection "CREATE TABLE \"odd table\" (order_id INTEGER REFERENCES orders(id))")
     (sql-grid-db-execute connection "CREATE TABLE \"say\"\"hi\" (id INTEGER)")
     (sql-grid-db-execute connection "CREATE TABLE other (other_column TEXT)")
     (let ((editor (generate-new-buffer " *sql-grid-test completion*")))
       (unwind-protect
           (with-current-buffer editor
             (sql-grid-mode)
             (sql-grid--attach-connection connection)
             (sql-grid-schema-refresh)
             (should (sql-grid-test--wait-for
                      (lambda ()
                        (with-current-buffer editor
                          (null sql-grid-schema--job)))))
             (cl-labels
                 ((candidates (sql)
                    (erase-buffer)
                    (insert sql)
                    (sql-grid-completion-at-point))
                  (prefix (completion)
                    (and completion
                         (buffer-substring-no-properties
                          (nth 0 completion) (nth 1 completion))))
                  (matches (completion)
                    (sql-grid-test--completion-matches completion)))
               (let ((completion (candidates "SELECT\n  ord")))
                 (should completion)
                 (should (equal (prefix completion) "ord"))
                 (should (member "orders" (matches completion)))
                 (should (member "order_id" (matches completion))))
               ;; Strings and comments, including multiline ones, do not
               ;; become completion contexts.
               (dolist (sql '("SELECT 'ord"
                              "SELECT 'line one\nord"
                              "SELECT 1 -- ord"
                              "SELECT 1\n-- ord"
                              "SELECT /* ord"
                              "SELECT /* multi\nline ord"))
                 (should-not (candidates sql)))
               ;; Completing on a later SQL line still works.
               (let ((completion (candidates "SELECT 1 -- earlier note\n  ord")))
                 (should completion)
                 (should (equal (prefix completion) "ord"))
                 (should (member "orders" (matches completion))))
               (let ((completion (candidates "SELECT \"say\"\"")))
                 (should completion)
                 (should (equal (prefix completion) "\"say\"\""))
                 (should (member "\"say\"\"hi\"" (matches completion))))
               (let ((completion (candidates "SELECT [odd t")))
                 (should completion)
                 (should (equal (prefix completion) "[odd t"))
                 (should (member "[odd table]" (matches completion))))
               (let ((completion (candidates "SELECT \"odd t")))
                 (should completion)
                 (should (equal (prefix completion) "\"odd t"))
                 (should (member "\"odd table\"" (matches completion))))
               ;; A qualifier dot replaces the prefix with bare columns.
               (let ((completion (candidates "SELECT orders.")))
                 (should completion)
                 (should (equal (prefix completion) ""))
                 (should (member "id" (matches completion)))
                 (should (member "order_id" (matches completion)))
                 (should-not (member "other_column" (matches completion))))
               (let ((completion (candidates "SELECT main.orders.")))
                 (should completion)
                 (should (equal (prefix completion) ""))
                 (should (member "id" (matches completion)))
                 (should (member "order_id" (matches completion)))
                 (should-not (member "other_column" (matches completion))))))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-schema-completion-quoting-case-and-discovery ()
  "Complete reserved, escaped, case-distinct, and sqlite-prefixed names."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute
      connection "CREATE TABLE \"Orders\" (payload TEXT)")
     (sql-grid-db-execute
      connection "CREATE TABLE \"Σ\" (sigma_col TEXT)")
     (sql-grid-db-execute
      connection "CREATE TABLE \"σ\" (final_col TEXT)")
     (sql-grid-db-execute
      connection "CREATE TABLE \"a]b\" (col TEXT)")
     (sql-grid-db-execute
      connection "CREATE TABLE \"say\"\"hi\" (id INTEGER)")
     (sql-grid-db-execute
      connection "CREATE TABLE order_lines (id INTEGER, order_id INTEGER)")
     (sql-grid-db-execute connection "CREATE TABLE other (other_column TEXT)")
     (sql-grid-db-execute
      connection "CREATE TABLE reserved (\"on\" TEXT, \"as\" TEXT, \"values\" TEXT)")
     (sql-grid-db-execute connection "CREATE TABLE sqliteXfoo (id INTEGER)")
     (sql-grid-db-execute
      connection "CREATE TABLE auto_t (id INTEGER PRIMARY KEY AUTOINCREMENT)")
     (sql-grid-db-execute connection "INSERT INTO \"Orders\" VALUES ('order-value')")
     (sql-grid-db-execute connection "INSERT INTO \"Σ\" VALUES ('sigma-value')")
     (sql-grid-db-execute connection "INSERT INTO \"σ\" VALUES ('final-value')")
     (sql-grid-db-execute connection "INSERT INTO \"a]b\" VALUES ('bracket-value')")
     (sql-grid-db-execute connection "INSERT INTO \"say\"\"hi\" VALUES (7)")
     (sql-grid-db-execute connection "INSERT INTO order_lines VALUES (5, 6)")
     (sql-grid-db-execute connection "INSERT INTO reserved VALUES ('x', 'y', 'z')")
     (sql-grid-db-execute connection "INSERT INTO sqliteXfoo VALUES (42)")
     (let ((editor (generate-new-buffer " *sql-grid-test schema edge*"))
           (sql-grid-result-format 'text)
           (sql-grid-focus-results nil))
       (unwind-protect
           (with-current-buffer editor
             (sql-grid-mode)
             (sql-grid--attach-connection connection)
             (sql-grid-schema-refresh)
             (should (sql-grid-test--wait-for
                      (lambda ()
                        (with-current-buffer editor
                          (null sql-grid-schema--job)))))
             (cl-labels
                 ((candidates (sql)
                    (erase-buffer)
                    (insert sql)
                    (sql-grid-completion-at-point))
                  (prefix (completion)
                    (and completion
                         (buffer-substring-no-properties
                          (nth 0 completion) (nth 1 completion))))
                  (matches (completion &optional string)
                    (sql-grid-test--completion-matches completion string))
                  (execute (sql)
                    (erase-buffer)
                    (insert sql)
                    (let ((result (sql-grid-execute-statement)))
                      (sql-grid-test--await-results result)
                      result)))
               ;; Reserved words must be quoted by completion.
               (let ((completion (candidates "SELECT ")))
                 (should completion)
                 (let ((names (matches completion)))
                   (dolist (quoted '("\"on\"" "\"as\"" "\"values\""))
                     (should (member quoted names))
                     (should-not (member (substring quoted 1 -1) names)))))
               (let ((result (execute
                              "SELECT \"on\", \"as\", \"values\" FROM reserved")))
                 (should (sql-grid-test--button-with-help result "x")))
               ;; A bracket-quoted identifier containing ] falls back to ".
               (let ((completion (candidates "SELECT * FROM [a")))
                 (should completion)
                 (should (member "\"a]b\"" (matches completion "")))
                 (should-not (member "[a]]b]" (matches completion "")))
                 (delete-region (nth 0 completion) (nth 1 completion))
                 (insert "\"a]b\"")
                 (should (equal (buffer-string) "SELECT * FROM \"a]b\"")))
               (let ((result (execute "SELECT * FROM \"a]b\"")))
                 (should (sql-grid-test--button-with-help
                          result "bracket-value")))
               ;; Midword completion replaces the full token, including an
               ;; ASCII-case-folded table name from a lowercase prefix.
               (erase-buffer)
               (insert "SELECT * FROM ordXYZ")
               (goto-char (+ (point-min) (length "SELECT * FROM ord")))
               (let ((completion (sql-grid-completion-at-point)))
                 (should completion)
                 (should (= (nth 0 completion)
                            (+ (point-min) (length "SELECT * FROM "))))
                 (should (= (nth 1 completion) (point-max)))
                 (should (member "Orders" (matches completion)))
                 (delete-region (nth 0 completion) (nth 1 completion))
                 (insert "Orders")
                 (should (equal (buffer-string) "SELECT * FROM Orders")))
               (let ((result (execute "SELECT * FROM Orders")))
                 (should (sql-grid-test--button-with-help result "order-value")))
               ;; A quoted suffix after point is replaced, not duplicated.
               (erase-buffer)
               (insert "SELECT * FROM \"say\"\"hi\"")
               (goto-char (+ (point-min) (length "SELECT * FROM \"say")))
               (let ((completion (sql-grid-completion-at-point)))
                 (should completion)
                 (should (= (nth 0 completion)
                            (+ (point-min) (length "SELECT * FROM "))))
                 (should (= (nth 1 completion) (point-max)))
                 (should (member "\"say\"\"hi\"" (matches completion)))
                 (delete-region (nth 0 completion) (nth 1 completion))
                 (insert "\"say\"\"hi\"")
                 (should (equal (buffer-string)
                                "SELECT * FROM \"say\"\"hi\"")))
               (let ((result (execute "SELECT * FROM \"say\"\"hi\"")))
                 (should (sql-grid-test--button-with-help result "7")))
               ;; SQLite folds only ASCII identifiers: Σ and σ stay distinct.
               (let ((completion (candidates "SELECT \"Σ\".")))
                 (should completion)
                 (should (equal (prefix completion) ""))
                 (let ((names (matches completion)))
                   (should (member "sigma_col" names))
                   (should-not (member "final_col" names))))
               (let ((result (execute
                              "SELECT \"Σ\".sigma_col FROM \"Σ\"")))
                 (should (sql-grid-test--button-with-help result "sigma-value")))
               ;; Comments between a qualifier and its dot keep the qualifier.
               (let ((completion (candidates "SELECT order_lines /* note */ .")))
                 (should completion)
                 (let ((names (matches completion)))
                   (should (member "id" names))
                   (should (member "order_id" names))
                   (should-not (member "other_column" names))))
               (let ((result (execute
                              "SELECT order_lines /* note */ .id FROM order_lines")))
                 (should (sql-grid-test--button-with-help result "5")))
               ;; LIKE's _ wildcard must not hide sqliteXfoo or show internals.
               (let ((completion (candidates "SELECT * FROM sqlite")))
                 (should completion)
                 (let ((names (matches completion)))
                   (should (member "sqliteXfoo" names))
                   (should-not (member "sqlite_sequence" names)))
                 (delete-region (nth 0 completion) (nth 1 completion))
                 (insert "sqliteXfoo")
                 (should (equal (buffer-string) "SELECT * FROM sqliteXfoo")))
               (let ((result (execute (buffer-string))))
                 (should (sql-grid-test--button-with-help result "42")))))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-sqlite-schema-discovery-includes-attached-metadata ()
  "Discover tables, columns, indexes, and foreign keys in attached schemas."
  (sql-grid-test--with-sqlite
   (lambda (connection)
     (sql-grid-db-execute connection "ATTACH DATABASE ':memory:' AS attached")
     (sql-grid-db-execute connection
                           "CREATE TABLE attached.parent (id INTEGER PRIMARY KEY)")
     (sql-grid-db-execute
      connection
      "CREATE TABLE attached.child (parent_id INTEGER REFERENCES parent(id))")
     (sql-grid-db-execute connection
                           "CREATE INDEX attached.child_parent ON child (parent_id)")
     (let ((editor (generate-new-buffer " *sql-grid-test attached schema*"))
           browser)
       (unwind-protect
           (with-current-buffer editor
             (sql-grid-mode)
             (sql-grid--attach-connection connection)
             (sql-grid-browse-schema)
             (set-buffer editor)
             (should (sql-grid-test--wait-for
                      (lambda ()
                        (with-current-buffer editor
                          (null sql-grid-schema--job)))))
             (setq browser (sql-grid-schema-browser-buffer editor))
             (with-current-buffer browser
               (let ((text (buffer-string)))
                 (should (string-match-p "attached\\.parent (table)" text))
                 (should (string-match-p "attached\\.child (table)" text))
                 (should (string-match-p "parent_id INTEGER" text))
                 (should (string-match-p "child_parent" text))
                 (should (string-match-p
                          "FOREIGN KEY parent_id -> parent.id" text)))))
         (dolist (buffer (list browser editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-schema-browsers-are-editor-owned-and-invalidate ()
  "Give each editor its own browser and drop schema after disconnect."
  (sql-grid-test--with-sqlite
   (lambda (alpha)
     (sql-grid-test--with-sqlite
      (lambda (beta)
        (sql-grid-db-execute alpha "CREATE TABLE alpha_only (id INTEGER PRIMARY KEY)")
        (sql-grid-db-execute beta "CREATE TABLE beta_only (id INTEGER PRIMARY KEY)")
        (let ((first (generate-new-buffer " *sql-grid-test schema first*"))
              (second (generate-new-buffer " *sql-grid-test schema second*"))
              first-browser second-browser)
          (unwind-protect
              (progn
                (with-current-buffer first
                  (sql-grid-mode)
                  (sql-grid--attach-connection alpha))
                (with-current-buffer second
                  (sql-grid-mode)
                  (sql-grid--attach-connection beta))
                (with-current-buffer first
                  (sql-grid-browse-schema)
                  (set-buffer first)
                  (should (sql-grid-test--wait-for
                           (lambda ()
                             (with-current-buffer first
                               (null sql-grid-schema--job))))))
                (with-current-buffer second
                  (sql-grid-browse-schema)
                  (set-buffer second)
                  (should (sql-grid-test--wait-for
                           (lambda ()
                             (with-current-buffer second
                               (null sql-grid-schema--job))))))
                (setq first-browser (sql-grid-schema-browser-buffer first)
                      second-browser (sql-grid-schema-browser-buffer second))
                (should (buffer-live-p first-browser))
                (should (buffer-live-p second-browser))
                (should-not (eq first-browser second-browser))
                (should-not (equal (buffer-name first-browser)
                                   (buffer-name second-browser)))
                (with-current-buffer first-browser
                  (should (string-match-p "alpha_only" (buffer-string)))
                  (should-not (string-match-p "beta_only" (buffer-string))))
                (with-current-buffer second-browser
                  (should (string-match-p "beta_only" (buffer-string)))
                  (should-not (string-match-p "alpha_only" (buffer-string))))
                ;; Disconnecting one editor invalidates only its browser.
                (with-current-buffer first (sql-grid-disconnect))
                (should
                 (sql-grid-test--wait-for
                  (lambda ()
                    (or (not (buffer-live-p first-browser))
                        (with-current-buffer first-browser
                          (and (not (string-match-p "alpha_only"
                                                    (buffer-string)))
                               (string-match-p "disconnected\\|gone"
                                               (buffer-string))))))))
                (with-current-buffer second-browser
                  (should (string-match-p "beta_only" (buffer-string))))
                (with-current-buffer first
                  (should-not (sql-grid-completion-at-point)))
                (with-current-buffer second
                  (erase-buffer)
                  (insert "SELECT bet")
                  (let ((completion (sql-grid-completion-at-point)))
                    (should completion)
                    (should (member "beta_only"
                                    (sql-grid-test--completion-matches
                                     completion))))))
            (dolist (buffer (list first-browser second-browser first second))
              (when (buffer-live-p buffer) (kill-buffer buffer))))))))))


(ert-deftest sql-grid-test-graphical-grid-truncation-retains-original ()
  "Display truncated native text while copy and inspection retain original value."
  (skip-unless (display-graphic-p))
  (let* ((source (generate-new-buffer " *sql-grid-test grid truncation*"))
         (sql-grid-result-format 'grid)
         (sql-grid-max-cell-chars 8)
         (kill-ring nil)
         (interprogram-cut-function nil)
         result-buffer detail-buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("value")
                              :rows '(("prefix-hidden-needle-tail")))))))
          (let* ((position (sql-grid-test--button-with-help
                            result-buffer "prefix-hidden-needle-tail"))
                 (label (sql-grid-test--button-visible-label
                         result-buffer "prefix-hidden-needle-tail")))
            (should position)
            (should (equal label "prefix-…"))
            (sql-grid-test--press-in-buffer result-buffer position "M-w")
            (should (equal (car kill-ring) "prefix-hidden-needle-tail"))
            (setq detail-buffer
                  (sql-grid-test--press-in-buffer result-buffer position "RET"))
            (should (equal (with-current-buffer detail-buffer (buffer-string))
                           "prefix-hidden-needle-tail"))))
      (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))


(ert-deftest sql-grid-test-evil-grid-search-inspects-full-value ()
  "Evil search, copy, and RET use original value in truncated grid cell."
  (skip-unless (and (display-graphic-p) (require 'evil nil t)))
  (let ((source (generate-new-buffer " *sql-grid-test Evil grid search*"))
        (sql-grid-result-format 'grid)
        (sql-grid-max-cell-chars 8)
        (sql-grid-focus-results t)
        (kill-ring nil)
        (interprogram-cut-function nil)
        result-buffer detail-buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("value")
                              :rows '(("prefix-hidden-needle-tail")))))))
          (select-window (get-buffer-window result-buffer))
          (funcall (intern "evil-local-mode") 1)
          (funcall (intern "evil-normal-state"))
          (sql-grid-test--press-in-buffer result-buffer (point-min)
                                           "/ needle RET")
          (sql-grid-test--press-in-buffer result-buffer (point) "M-w")
          (should (equal (car kill-ring) "prefix-hidden-needle-tail"))
          (setq detail-buffer
                (sql-grid-test--press-in-buffer result-buffer (point) "RET"))
          (should (equal (with-current-buffer detail-buffer (buffer-string))
                         "prefix-hidden-needle-tail")))
      (when (buffer-live-p detail-buffer) (kill-buffer detail-buffer))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))
(ert-deftest sql-grid-test-graphical-grid-cell-inspection-display ()
  "Inspect original grid cell value in echo area without leaving result window."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test grid inspection*"))
        (sql-grid-result-format 'grid)
        (sql-grid-cell-display 'minibuffer)
        (sql-grid-max-cell-chars 6)
        result-buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("value")
                              :rows '(("100% complete")))))))
          (let ((position (sql-grid-test--button-with-help result-buffer "100% complete")))
            (should position)
            (select-window (get-buffer-window result-buffer))
            (goto-char position)
            (let ((noninteractive nil))
              (call-interactively (key-binding (kbd "RET"))))
            (should (eq (current-buffer) result-buffer))
            (should (equal (or (current-message) "") "100% complete"))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest sql-grid-test-graphical-grid-column-alignment-after-zoom ()
  "Keep native cell boundaries aligned across mixed-width rows and zoom."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test grid alignment*"))
        (sql-grid-result-format 'grid)
        (kill-ring nil)
        (interprogram-cut-function nil)
        result-buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "value")
                              :rows '((1 "short")
                                      (2 "variable-width Latin phrase")
                                      (3 "ភាសាខ្មែរ")))))))
          (let ((a (sql-grid-test--button-with-help result-buffer "short"))
                (b (sql-grid-test--button-with-help result-buffer "variable-width Latin phrase"))
                (c (sql-grid-test--button-with-help result-buffer "ភាសាខ្មែរ")))
            (redisplay t)
            (should (= (car (posn-x-y (posn-at-point a)))
                       (car (posn-x-y (posn-at-point b)))
                       (car (posn-x-y (posn-at-point c)))))
            (with-current-buffer result-buffer
              (goto-char c)
              (text-scale-set 2)
              (redisplay t)
              (should (= (car (posn-x-y (posn-at-point a)))
                         (car (posn-x-y (posn-at-point b)))
                         (car (posn-x-y (posn-at-point c)))))
              (sql-grid-copy-cell)
              (should (equal (car kill-ring) "ភាសាខ្មែរ"))
              (should (= (point) c))
              (text-scale-set 0))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source)))))
(ert-deftest sql-grid-test-graphical-grid-frame-font-scales-pixels ()
  "Scale native grid pixel geometry with frame font."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid-test grid frame font*"))
        (frame (let ((noninteractive nil))
                 (make-frame `((window-system . ,window-system)
                               (font . "monospace-12")
                               (width . 90) (height . 24)))))
        (sql-grid-result-format 'grid)
        result-buffer)
    (unwind-protect
        (with-selected-frame frame
          (with-current-buffer source
            (setq result-buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("value")
                              :rows '(("Customer 1") ("Customer 2")))))))
          (with-current-buffer result-buffer
            (let* ((position (sql-grid-test--button-with-help result-buffer "Customer 1"))
                   (window (get-buffer-window result-buffer))
                   (normal (window-text-pixel-size window (point-min) (point-max))))
              (goto-char position)
              (set-frame-font "monospace-24" nil (list frame))
              (sql-grid-refresh)
              (redisplay t)
              (let ((enlarged (window-text-pixel-size window (point-min) (point-max))))
                (should (> (cdr enlarged) (cdr normal)))
                (should (= (point) position))
                (should (equal (button-get (button-at (point)) 'sql-grid-value)
                               "Customer 1"))))))
      (when (buffer-live-p result-buffer) (kill-buffer result-buffer))
      (when (buffer-live-p source) (kill-buffer source))
      (when (frame-live-p frame) (delete-frame frame t)))))

(defun sql-grid-test--with-fake-mysql (backend function)
   "Call FUNCTION with BACKEND and a protocol-valid fake client."
   (let* ((directory (make-temp-file "sql-grid-client-test-" t))
          (default-directory directory)
          (temporary-file-directory directory)
          (program (expand-file-name "mysql-fixture" directory))
          (connection (sql-grid-connection-create
                       :backend backend :database "fixture" :host "localhost"
                       :user "fixture" :password "secret")))
     (unwind-protect
         (progn
           (with-temp-file program
             (insert
              "#!/bin/sh\n"
              "defaults=\n"
              "for arg do case \"$arg\" in --defaults-file=*) defaults=${arg#--defaults-file=} ;; esac; done\n"
              "test -n \"$defaults\" && test \"$(stat -c %a \"$defaults\")\" = 600 && grep -Eq '^password=(secret|\"secret\")$' \"$defaults\" || { echo 'credential fixture rejected' >&2; exit 91; }\n"
              "sql=$(cat)\n"
              "case \"$sql\" in\n"
              "  *SHOW*TABLES*) printf '<resultset><row><field name=\"Tables_in_fixture\">OrderLines</field><field name=\"Table_type\">BASE TABLE</field></row><row><field name=\"Tables_in_fixture\">ParentThing</field><field name=\"Table_type\">BASE TABLE</field></row></resultset>\\n' ;;\n"
              "  *SHOW*FULL*COLUMNS*OrderLines*) printf '%s\\n' '<resultset xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><row><field name=\"Field\">left_id</field><field name=\"Type\">int</field><field name=\"Collation\"></field><field name=\"Null\">NO</field><field name=\"Key\">PRI</field><field name=\"Default\" xsi:nil=\"true\"/><field name=\"Extra\">auto_increment</field></row><row><field name=\"Field\">right_id</field><field name=\"Type\">int</field><field name=\"Collation\"></field><field name=\"Null\">YES</field><field name=\"Key\">MUL</field><field name=\"Default\">0</field><field name=\"Extra\"></field></row></resultset>' ;; "
              "  *SHOW*FULL*COLUMNS*ParentThing*) printf '%s\\n' '<resultset xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><row><field name=\"Field\">parent_a</field><field name=\"Type\">int</field><field name=\"Collation\"></field><field name=\"Null\">NO</field><field name=\"Key\">PRI</field><field name=\"Default\" xsi:nil=\"true\"/><field name=\"Extra\"></field></row><row><field name=\"Field\">parent_b</field><field name=\"Type\">int</field><field name=\"Collation\"></field><field name=\"Null\">NO</field><field name=\"Key\">PRI</field><field name=\"Default\" xsi:nil=\"true\"/><field name=\"Extra\"></field></row></resultset>' ;; "
              "  *SHOW*INDEX*OrderLines*) printf '<resultset><row><field name=\"Table\">OrderLines</field><field name=\"Non_unique\">0</field><field name=\"Key_name\">uq_order_pair</field><field name=\"Seq_in_index\">1</field><field name=\"Column_name\">left_id</field></row><row><field name=\"Table\">OrderLines</field><field name=\"Non_unique\">0</field><field name=\"Key_name\">uq_order_pair</field><field name=\"Seq_in_index\">2</field><field name=\"Column_name\">right_id</field></row></resultset>\\n' ;;\n"
              "  *SHOW*INDEX*ParentThing*) printf '<resultset></resultset>\\n' ;;\n"
              "  *KEY_COLUMN_USAGE*) printf '<resultset><row><field name=\"CONSTRAINT_NAME\">fk_order_parent</field><field name=\"TABLE_NAME\">OrderLines</field><field name=\"COLUMN_NAME\">left_id</field><field name=\"REFERENCED_TABLE_SCHEMA\">fixture</field><field name=\"REFERENCED_TABLE_NAME\">ParentThing</field><field name=\"REFERENCED_COLUMN_NAME\">parent_a</field><field name=\"ORDINAL_POSITION\">1</field></row><row><field name=\"CONSTRAINT_NAME\">fk_order_parent</field><field name=\"TABLE_NAME\">OrderLines</field><field name=\"COLUMN_NAME\">right_id</field><field name=\"REFERENCED_TABLE_SCHEMA\">fixture</field><field name=\"REFERENCED_TABLE_NAME\">ParentThing</field><field name=\"REFERENCED_COLUMN_NAME\">parent_b</field><field name=\"ORDINAL_POSITION\">2</field></row></resultset>\\n' ;;\n"
              "  *SQL_GRID_TEST_MYSQL_ERROR*) echo 'ERROR 1064 (42000): fixture error' >&2; exit 1 ;;\n"
              "  *SQL_GRID_TEST_MYSQL_CANCEL*) exec sleep 30 ;;\n"
              "  *SQL_GRID_TEST_MYSQL_WRITE*) printf 'Query OK, 0 rows affected\\n<resultset statement=\"UPDATE\"></resultset>\\n' ;;\n"
              "  *) printf '<?xml version=\"1.0\"?>\\n<resultset statement=\"SELECT\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><row><field name=\"id\">1</field><field name=\"note\" xsi:nil=\"true\"/><field name=\"empty\"></field><field name=\"xml\">a &amp; b &lt; c</field></row><row><field name=\"id\">2</field><field name=\"note\">line one\\nភាសាខ្មែរ</field><field name=\"empty\">NULL</field><field name=\"xml\">quote &quot; value</field></row></resultset>\\n' ;;\n"
              "esac\n"))
           (set-file-modes program #o700)
           (if (eq backend 'mysql)
               (let ((sql-grid-mysql-program program))
                 (sql-grid-db-connect connection)
                 (funcall function connection directory))
             (let ((sql-grid-mariadb-program program))
               (sql-grid-db-connect connection)
               (funcall function connection directory)))
           (should-not (directory-files directory nil
                                        "\\`sql-grid-client-")))
       (ignore-errors (sql-grid-db-disconnect connection))
       (delete-directory directory t))))

(ert-deftest sql-grid-test-mysql-mariadb-public-client-boundary ()
  "Preserve XML results, counts, errors, cancellation, and credential cleanup."
  (dolist (backend '(mysql mariadb))
    (sql-grid-test--with-fake-mysql
     backend
     (lambda (connection directory)
       (dolist (execution '(sync async))
         (let* ((sql "SELECT 'SQL_GRID_TEST_MYSQL_VALUES'")
                (results
                 (if (eq execution 'sync)
                     (sql-grid-db-execute connection sql)
                   (let (done value failure)
                     (sql-grid-db-execute-async
                      connection sql
                      (lambda (result error-data)
                        (setq value result failure error-data done t)))
                     (should (sql-grid-test--wait-for (lambda () done) 10))
                     (should-not failure)
                     value)))
                (result (car results)))
           (should (equal (sql-grid-result-columns result)
                          '("id" "note" "empty" "xml")))
           (should (equal (sql-grid-result-rows result)
                          '(("1" nil "" "a & b < c")
                            ("2" "line one\nភាសាខ្មែរ"
                             "NULL" "quote \" value"))))
           (should-not (sql-grid-result-affected-rows result))
           (should-not (directory-files directory nil
                                        "\\`sql-grid-client-"))))
       (let ((result
              (car (sql-grid-db-execute
                    connection "UPDATE fixture SET value = 'SQL_GRID_TEST_MYSQL_WRITE'"))))
         (should (= (sql-grid-result-affected-rows result) 0)))
       (should-not (directory-files directory nil "\\`sql-grid-client-"))
       (should-error
        (sql-grid-db-execute connection "SELECT 'SQL_GRID_TEST_MYSQL_ERROR'"))
       (should-not (directory-files directory nil "\\`sql-grid-client-"))
       (let (done failure job credentials)
         (setq job
               (sql-grid-db-execute-async
                connection "SELECT 'SQL_GRID_TEST_MYSQL_CANCEL'"
                (lambda (_result error-data)
                  (setq failure error-data done t))))
         (should (sql-grid-test--wait-for
                  (lambda ()
                    (setq credentials
                          (car (directory-files directory t
                                                "\\`sql-grid-client-")))
                    (and credentials (process-live-p
                                      (sql-grid-job-process job))))
                  10))
         (should (= (file-modes credentials) #o600))
         (should-not
          (string-match-p "secret"
                          (mapconcat #'identity
                                     (process-command
                                      (sql-grid-job-process job))
                                     " ")))
         (sql-grid-db-cancel job)
         (should done)
         (should (eq (car failure) 'quit))
         (should-not (file-exists-p credentials))
         (should-not (directory-files directory nil
                                      "\\`sql-grid-client-")))

       (let (done failure job credentials)
         (setq job
               (sql-grid-db-execute-async
                connection "SELECT 'SQL_GRID_TEST_MYSQL_CANCEL'"
                (lambda (_result error-data)
                  (setq failure error-data done t))))
         (should (sql-grid-test--wait-for
                  (lambda ()
                    (setq credentials
                          (car (directory-files directory t
                                                "\\`sql-grid-client-")))
                    (and credentials (process-live-p
                                      (sql-grid-job-process job))))
                  10))
         (sql-grid-db-disconnect connection)
         (should done)
         (should (eq (car failure) 'quit))
         (should-not (file-exists-p credentials))
         (should-not (directory-files directory nil
                                      "\\`sql-grid-client-")))))))
 (ert-deftest sql-grid-test-mysql-mariadb-schema-completion ()
   "Expose ordered indexes, foreign keys, and table completions for both clients."
   (dolist (backend '(mysql mariadb))
     (sql-grid-test--with-fake-mysql
      backend
      (lambda (connection _directory)
        (let ((editor (generate-new-buffer " *sql-grid-test mysql schema*"))
              browser)
          (unwind-protect
              (with-current-buffer editor
                (sql-grid-mode)
                (sql-grid--attach-connection connection)
                (sql-grid-browse-schema)
                (set-buffer editor)
                (should
                 (sql-grid-test--wait-for
                  (lambda () (null sql-grid-schema--job))))
                (setq browser (sql-grid-schema-browser-buffer editor))
                (with-current-buffer browser
                  (let ((text (buffer-string)))
                    (should (string-match-p
                             "left_id int NOT NULL PRIMARY KEY auto_increment" text))
                    (should (string-match-p
                             "right_id int NULL KEY DEFAULT 0" text))
                    (should (string-match-p
                             "uq_order_pair UNIQUE (left_id, right_id)" text))
                    (should (string-match-p
                             "FOREIGN KEY left_id -> fixture.ParentThing.parent_a" text))
                    (should (string-match-p
                             "FOREIGN KEY right_id -> fixture.ParentThing.parent_b" text))))
                (erase-buffer)
                (insert "SELECT left_id FROM Order")
                (let ((completion (sql-grid-completion-at-point)))
                  (should completion)
                  (should (member "OrderLines"
                                  (sql-grid-test--completion-matches completion)))))
            (dolist (buffer (list browser editor))
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))
(defun sql-grid-test--with-fake-postgres (function)
  "Call FUNCTION with a PostgreSQL connection using a deterministic fake psql."
  (let* ((directory (make-temp-file "sql-grid-postgres-" t))
         (default-directory directory)
         (temporary-file-directory directory)
         (program (expand-file-name "psql-fixture" directory))
         (sql-grid-postgres-program program)
         (connection (sql-grid-connection-create
                      :backend 'postgres :database "fixture" :host "localhost"
                      :user "fixture" :password "secret")))
    (unwind-protect
        (progn
          (with-temp-file program
            (insert
             "#!/bin/sh\n"
             "marker=\n"
             "count_marker=\n"
             "for arg do\n"
             "  case \"$arg\" in\n"
             "    --pset=null=*) marker=${arg#--pset=null=} ;;\n"
             "    --command=\\\\echo*) count_marker=${arg#--command=\\\\echo }; count_marker=${count_marker% :ROW_COUNT} ;;\n"
             "  esac\n"
             "done\n"
             "case \"$*\" in\n"
             "  *information_schema.tables*) printf 'table_schema,table_name,kind\\nMixedSchema,OrderLines,table\\nTargetSchema,ParentThing,table\\n%s 2\\n' \"$count_marker\" ;;\n"
             "  *information_schema.columns*OrderLines*) printf 'column_name,data_type,is_nullable,column_default,pk\\nleft_id,integer,NO,,NO\\nright_id,integer,NO,,NO\\n%s 2\\n' \"$count_marker\" ;;\n"
             "  *information_schema.columns*ParentThing*) printf 'column_name,data_type,is_nullable,column_default,pk\\nparent_a,integer,NO,,YES\\nparent_b,integer,NO,,YES\\n%s 2\\n' \"$count_marker\" ;;\n"
             "  *pg_index*OrderLines*) printf 'index_name,unique,column,role,predicate\\npartial_idx,true,left_id,key,active IS TRUE\\npartial_idx,true,note,include,active IS TRUE\\n%s 2\\n' \"$count_marker\" ;;\n"
             "  *pg_index*ParentThing*) printf 'index_name,unique,column,role,predicate\\n%s 0\\n' \"$count_marker\" ;;\n"
             "  *pg_constraint*OrderLines*) printf 'constraint,ordinal,source_column,target_schema,target_table,target_column\\nforeign_pair,1,left_id,TargetSchema,ParentThing,parent_a\\nforeign_pair,2,right_id,TargetSchema,ParentThing,parent_b\\n%s 2\\n' \"$count_marker\" ;;\n"
             "  *SQL_GRID_TEST_EMPTY*) printf 'id,note,kind,tail\\n%s 0\\n' \"$count_marker\" ;;\n"
             "  *SQL_GRID_TEST_RETURNING*) printf 'id,note,kind,tail\\n9,returned,write,kept\\n%s 1\\n' \"$count_marker\" ;;\n"
             "  *SQL_GRID_TEST_WRITE*) printf '%s 3\\n' \"$count_marker\" ;;\n"
             "  *SQL_GRID_TEST_NO_TRAILER*) printf 'id,note,kind,tail\\n1,one,two,three\\n' ;;\n"
             "  *SQL_GRID_TEST_ERROR*) printf 'ERROR: deliberate fixture failure\\n' >&2; exit 1 ;;\n"
             "  *SQL_GRID_TEST_SLOW*) exec sleep 30 ;;\n"
             "  *) printf 'id,note,kind,tail\\n1,\"\",empty,first\\n2,\"comma, quote \"\" and line\\nbreak\",multiline,second\\n3,%s,null,third\\n4,\"ភាសាខ្មែរ\",unicode,fourth\\n5,\"NULL\",literal,fifth\\n%s 0\\n' \"$marker\" \"$count_marker\" ;;\n"
             "esac\n"))
          (set-file-modes program #o700)
          (sql-grid-db-connect connection)
          (funcall function connection)
          (should-not (directory-files directory nil "\\`sql-grid-pgpass-")))
      (ignore-errors (sql-grid-db-disconnect connection))
      (delete-directory directory t))))

(ert-deftest sql-grid-test-postgres-csv-results-preserve-values-and-empty-metadata ()
  "Preserve PostgreSQL CSV NULL, empty, multiline, Unicode, and empty headers."
  (sql-grid-test--with-fake-postgres
   (lambda (connection)
     (let* ((result
             (car (sql-grid-db-execute
                   connection
                  (concat "SELECT 'SQL_GRID_TEST_VALUES' AS id, NULL::text AS note, "
                           "'first' AS kind, 'fourth-column' AS tail"))))
            (rows (sql-grid-result-rows result)))
       (should (equal (sql-grid-result-columns result)
                      '("id" "note" "kind" "tail")))
       (should (equal (nth 0 rows) '("1" "" "empty" "first")))
       (should (equal (nth 1 rows)
                      '("2" "comma, quote \" and line\nbreak"
                        "multiline" "second")))
       (should (equal (nth 2 rows) '("3" nil "null" "third")))
       (should (equal (nth 3 rows)
                      '("4" "ភាសាខ្មែរ" "unicode" "fourth")))
       (should (equal (nth 4 rows)
                      '("5" "NULL" "literal" "fifth")))
       (should (null (sql-grid-result-affected-rows result)))
       (should (>= (sql-grid-result-elapsed result) 0)))
     (let ((empty
            (car (sql-grid-db-execute
                  connection
                  "SELECT 'SQL_GRID_TEST_EMPTY' AS id, NULL::text AS note, 1 AS kind, 2 AS tail WHERE FALSE"))))
       (should (equal (sql-grid-result-columns empty)
                      '("id" "note" "kind" "tail")))
       (should-not (sql-grid-result-rows empty)))
     (let ((returned
            (car (sql-grid-db-execute
                  connection
                  "INSERT INTO fixture VALUES (1) RETURNING 'SQL_GRID_TEST_RETURNING'"))))
       (should (equal (sql-grid-result-rows returned)
                      '(("9" "returned" "write" "kept"))))
       (should (= (sql-grid-result-affected-rows returned) 1)))
     (let ((updated
            (car (sql-grid-db-execute
                  connection
                  (concat "WITH chosen AS (SELECT $$semi; colon$$ AS note "
                          "/* outer /* nested */ still */) "
                          "UPDATE fixture SET value = 'SQL_GRID_TEST_WRITE'")))))
       (should-not (sql-grid-result-columns updated))
       (should-not (sql-grid-result-rows updated))
       (should (= (sql-grid-result-affected-rows updated) 3)))
     (should-error
      (sql-grid-db-execute connection "SELECT 'SQL_GRID_TEST_NO_TRAILER'")))))

(ert-deftest sql-grid-test-postgres-schema-browser-and-mixed-case-completion ()
  "Show schema-qualified PostgreSQL foreign keys and complete exact-case names."
  (sql-grid-test--with-fake-postgres
   (lambda (connection)
     (let ((editor (generate-new-buffer " *sql-grid-test postgres schema*"))
           browser)
       (unwind-protect
           (with-current-buffer editor
             (sql-grid-mode)
             (sql-grid--attach-connection connection)
             (sql-grid-browse-schema)
             (set-buffer editor)
             (should
              (sql-grid-test--wait-for
               (lambda ()
                 (with-current-buffer editor
                   (null sql-grid-schema--job)))))
             (setq browser (sql-grid-schema-browser-buffer editor))
             (with-current-buffer browser
               (let ((text (buffer-string)))
                 (should (string-match-p
                          "\"MixedSchema\"\\.\"OrderLines\"" text))
                 (should (string-match-p
                          "partial_idx.*UNIQUE.*left_id.*INCLUDE.*note.*WHERE active IS TRUE"
                          text))
                 (should (string-match-p
                          "FOREIGN KEY \"left_id\" -> \"TargetSchema\"\\.\"ParentThing\" (\"parent_a\")"
                          text))
                 (should (string-match-p
                          "FOREIGN KEY \"right_id\" -> \"TargetSchema\"\\.\"ParentThing\" (\"parent_b\")"
                          text))))
             (erase-buffer)
             (insert "SELECT \"MixedS")
             (let ((completion (sql-grid-completion-at-point)))
               (should completion)
               (should (member
                        "\"MixedSchema\".\"OrderLines\""
                        (sql-grid-test--completion-matches completion)))))
         (dolist (buffer (list browser editor))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest sql-grid-test-postgres-profile-and-manual-editor-connections ()
  "Connect editor through PostgreSQL profile and manual settings prompts."
  (sql-grid-test--with-fake-postgres
   (lambda (_connection)
     (let ((editor (generate-new-buffer " *sql-grid-test postgres connect*"))
           (sql-grid-connections
            '(("Development" :backend postgres :database "fixture"
               :host "localhost" :port 5432 :user "fixture")))
           (sql-grid-result-format 'text))
       (unwind-protect
           (with-current-buffer editor
             (insert "SELECT 'SQL_GRID_TEST_VALUES'")
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) "Development"))
                       ((symbol-function 'read-passwd)
                        (lambda (&rest _) "secret")))
               (sql-grid-connect-buffer))
             (should (eq (sql-grid-connection-backend sql-grid--connection)
                         'postgres))
             (should (eq sql-product 'postgres))
             (should (equal (buffer-string) "SELECT 'SQL_GRID_TEST_VALUES'"))
             (let ((result
                    (car (sql-grid-db-execute
                          sql-grid--connection
                          "SELECT 'SQL_GRID_TEST_VALUES'"))))
               (should (equal (sql-grid-result-columns result)
                              '("id" "note" "kind" "tail")))
               (should (equal (car (sql-grid-result-rows result))
                              '("1" "" "empty" "first"))))
             (sql-grid-disconnect)
             (setq sql-grid-connections nil)
             (let ((answers '("postgres")))
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) (or (pop answers) "")))
                         ((symbol-function 'read-string)
                          (lambda (prompt &rest _)
                            (pcase prompt
                              ("Database: " "fixture")
                              ("Host [localhost]: " "localhost")
                              ("Port [default]: " "5432")
                              ("User: " "fixture"))))
                         ((symbol-function 'read-passwd)
                          (lambda (&rest _) "secret")))
                 (sql-grid-connect-buffer)))
             (should (eq (sql-grid-connection-backend sql-grid--connection)
                         'postgres))
             (let ((result
                    (car (sql-grid-db-execute
                          sql-grid--connection
                          "SELECT 'SQL_GRID_TEST_VALUES'"))))
               (should (equal (sql-grid-result-columns result)
                              '("id" "note" "kind" "tail")))
               (should (equal (car (sql-grid-result-rows result))
                              '("1" "" "empty" "first")))))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-postgres-profile-passwords-skip-prompt ()
  "Use configured profile passwords, including the empty string, without prompting."
  (sql-grid-test--with-fake-postgres
   (lambda (_connection)
     (let ((editor (generate-new-buffer " *sql-grid-test profile password*"))
           (sql-grid-result-format 'text)
           (sql-grid-connections nil)
           (prompt-count 0))
       (unwind-protect
           (dolist (password '("configured secret" ""))
             (setq sql-grid-connections
                   `(("Configured" :backend postgres :database "fixture"
                      :password ,password)))
             (with-current-buffer editor
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "Configured"))
                         ((symbol-function 'read-passwd)
                          (lambda (&rest _)
                            (setq prompt-count (1+ prompt-count))
                            "prompted secret")))
                 (sql-grid-connect-buffer))
               (should (equal (sql-grid-connection-password sql-grid--connection)
                              password))
               (should (sql-grid-db-execute
                        sql-grid--connection
                        "SELECT 'SQL_GRID_TEST_VALUES'"))
               (sql-grid-disconnect)
               (should-not sql-grid--connection))
             (should (= prompt-count 0)))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-postgres-profile-password-omission-prompts ()
  "Prompt when server profiles omit :password or set it to nil."
  (sql-grid-test--with-fake-postgres
   (lambda (_connection)
     (let ((editor (generate-new-buffer " *sql-grid-test prompted profile*"))
           (sql-grid-result-format 'text)
           (sql-grid-connections nil)
           (prompt-count 0))
       (unwind-protect
           (dolist (password '(:missing nil))
             (setq sql-grid-connections
                   `(("Prompted" :backend postgres :database "fixture"
                      ,@(unless (eq password :missing)
                          (list :password password)))))
             (with-current-buffer editor
               (cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) "Prompted"))
                         ((symbol-function 'read-passwd)
                          (lambda (&rest _)
                            (setq prompt-count (1+ prompt-count))
                            "prompted secret")))
                 (sql-grid-connect-buffer))
               (should (equal (sql-grid-connection-password sql-grid--connection)
                              "prompted secret"))
               (should (sql-grid-db-execute
                        sql-grid--connection
                        "SELECT 'SQL_GRID_TEST_VALUES'"))
               (sql-grid-disconnect))
             (should (= prompt-count (if (eq password :missing) 1 2))))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-postgres-profile-invalid-password-rejected ()
  "Reject non-string profile passwords before prompting or connecting."
  (sql-grid-test--with-fake-postgres
   (lambda (_connection)
     (let ((editor (generate-new-buffer " *sql-grid-test invalid profile password*"))
           (sql-grid-result-format 'text)
           (sql-grid-connections
            '(("Invalid" :backend postgres :database "fixture" :password 42)))
           (prompted nil))
       (unwind-protect
           (with-current-buffer editor
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) "Invalid"))
                       ((symbol-function 'read-passwd)
                        (lambda (&rest _)
                          (setq prompted t)
                          "secret")))
               (should-error (sql-grid-connect-buffer) :type 'user-error))
             (should-not prompted)
             (should-not sql-grid--connection))
         (when (buffer-live-p editor) (kill-buffer editor)))))))

(ert-deftest sql-grid-test-postgres-profile-passwords-are-buffer-owned ()
  "Disconnect and reconnect independent profile connections without changing configuration."
  (sql-grid-test--with-fake-postgres
   (lambda (_connection)
     (let ((first (generate-new-buffer " *sql-grid-test password owner one*"))
           (second (generate-new-buffer " *sql-grid-test password owner two*"))
           (sql-grid-result-format 'text)
           (sql-grid-connections
            '(("Saved" :backend postgres :database "fixture"
               :password "saved secret"))))
       (unwind-protect
           (progn
             (dolist (editor (list first second))
               (with-current-buffer editor
                 (cl-letf (((symbol-function 'completing-read)
                            (lambda (&rest _) "Saved"))
                           ((symbol-function 'read-passwd)
                            (lambda (&rest _) (ert-fail "Unexpected password prompt"))))
                   (sql-grid-connect-buffer))
                 (should (sql-grid-db-execute
                          sql-grid--connection
                          "SELECT 'SQL_GRID_TEST_VALUES'"))))
             (let ((first-connection
                    (buffer-local-value 'sql-grid--connection first))
                   (second-connection
                    (buffer-local-value 'sql-grid--connection second)))
               (should-not (eq first-connection second-connection))
               (with-current-buffer first (sql-grid-disconnect))
               (should-not (sql-grid-connection-password first-connection))
               (should (equal (plist-get (cdar sql-grid-connections) :password)
                              "saved secret"))
               (should (equal (sql-grid-connection-password second-connection)
                              "saved secret"))
               (with-current-buffer first
                 (cl-letf (((symbol-function 'completing-read)
                            (lambda (&rest _) "Saved"))
                           ((symbol-function 'read-passwd)
                            (lambda (&rest _) (ert-fail "Unexpected password prompt"))))
                   (sql-grid-connect-buffer))
                 (should (equal (sql-grid-connection-password sql-grid--connection)
                                "saved secret"))
                 (sql-grid-disconnect))
               (should (equal (plist-get (cdar sql-grid-connections) :password)
                              "saved secret"))
               (with-current-buffer second (sql-grid-disconnect))
               (should-not (sql-grid-connection-password second-connection))))
         (dolist (editor (list first second))
           (when (buffer-live-p editor) (kill-buffer editor))))))))

(ert-deftest sql-grid-test-postgres-single-statement-lexing-and-errors ()
  "Accept PostgreSQL quoting/comment constructs and reject meta/multiple statements."
  (sql-grid-test--with-fake-postgres
   (lambda (connection)
     (dolist (sql '("SELECT $$semi; colon$$"
                    "SELECT E'escaped\\'; semicolon; still string'"
                    "SELECT 1 /* outer /* inner; */ still comment */"))
       (let ((result (car (sql-grid-db-execute connection sql))))
         (should (equal (sql-grid-result-columns result)
                        '("id" "note" "kind" "tail")))
         (should (equal (car (sql-grid-result-rows result))
                        '("1" "" "empty" "first")))))
     (should-error (sql-grid-db-execute connection "\\dt")
                   :type 'user-error)
     (should-error (sql-grid-db-execute connection "SELECT 1; SELECT 2")
                   :type 'user-error)
     (should-error
      (sql-grid-db-execute
       connection "SELECT 1 AS foo$tag$; SELECT 2 AS bar$tag$;")
      :type 'user-error)
     (should-error
      (sql-grid-db-execute connection "SELECT 'SQL_GRID_TEST_ERROR'")))))

(ert-deftest sql-grid-test-postgres-async-error-and-cancel-cleanup ()
  "Report asynchronous errors once and cancel a running psql process cleanly."
  (sql-grid-test--with-fake-postgres
   (lambda (connection)
     (let ((calls 0) done results failure)
       (sql-grid-db-execute-async
        connection "SELECT 'SQL_GRID_TEST_ERROR'"
        (lambda (value error-data)
          (cl-incf calls)
          (setq results value failure error-data done t)))
       (should (sql-grid-test--wait-for (lambda () done) 10))
       (should (= calls 1))
       (should-not results)
       (should failure))
     (let ((calls 0) done results failure)
       (let* ((job
               (sql-grid-db-execute-async
                connection "SELECT 'SQL_GRID_TEST_SLOW'"
                (lambda (value error-data)
                  (cl-incf calls)
                  (setq results value failure error-data done t))))
              (password-files
               (directory-files temporary-file-directory t
                                "\\`[^/]*sql-grid-pgpass-[^/]+\\'"))
              (password-file (car password-files)))
         (should (= (length password-files) 1))
         (should (= (file-modes password-file) #o600))
         (should-not (string-match-p
                      "secret" (mapconcat #'identity
                                           (process-command
                                            (sql-grid-job-process job))
                                           " ")))
         (should-not (sql-grid-test--wait-for (lambda () done) 0.1))
         (sql-grid-db-cancel job)
         (should done)
         (should (= calls 1))
         (should-not results)
         (should (eq (car failure) 'quit))
         (should-not (file-exists-p password-file))
         (sql-grid-db-cancel job)
         (should (= calls 1)))))))

(ert-deftest sql-grid-test-grid-stripes-follow-visible-sort-order ()
  "Alternate row backgrounds after sorting, without changing raw row identity."
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *sql-grid stripes*"))
        (sql-grid-result-format 'grid)
        (sql-grid-focus-results t)
        buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id")
                              :rows '((4) (2) (3) (1)))))))
          (with-current-buffer buffer
            (sql-grid-sort-column 0)
            (let ((first (sql-grid-test--button-with-help buffer "1"))
                  (second (sql-grid-test--button-with-help buffer "2"))
                  (third (sql-grid-test--button-with-help buffer "3")))
              (should-not
               (equal (plist-get (get-text-property first 'face) :background)
                      (plist-get (get-text-property second 'face) :background)))
              (should
               (equal (plist-get (get-text-property first 'face) :background)
                      (plist-get (get-text-property third 'face) :background))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (kill-buffer source))))

(ert-deftest sql-grid-test-copy-cell-rectangle-uses-visible-order-and-full-values ()
  "Copy reverse selections after sort and column moves without grid decoration."
  (let ((source (generate-new-buffer " *sql-grid selection*"))
        (sql-grid-result-format 'text)
        (sql-grid-max-rows 2)
        (sql-grid-max-cell-chars 4)
        (transient-mark-mode t)
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("rank" "value" "empty")
                              :rows '((3 "excluded" "x")
                                      (1 "first\nfull" nil)
                                      (2 "second full" "")))))))
          (with-current-buffer buffer
            (sql-grid-sort-column 0)
            (goto-char (sql-grid-test--button-with-help buffer "value"))
            (sql-grid-move-column-left)
            (goto-char (sql-grid-test--button-with-help buffer ""))
            (push-mark (point) t t)
            (goto-char (sql-grid-test--button-with-help buffer "first\nfull"))
            (call-interactively (key-binding (kbd "M-w")))
            (should (equal (current-kill 0)
                           "first\nfull\t1\tNULL\nsecond full\t2\t"))
            (should-not mark-active)
            (should-not (text-properties-at 0 (current-kill 0)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (kill-buffer source))))

(ert-deftest sql-grid-test-selection-rejects-cross-result-set-endpoints ()
  "Do not silently copy a partial rectangle across result sets."
  (let ((source (generate-new-buffer " *sql-grid selection sets*"))
        (sql-grid-result-format 'text)
        (transient-mark-mode t)
        buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create :columns '("a") :rows '(("left")))
                             (sql-grid-result-create :columns '("b") :rows '(("right")))))))
          (with-current-buffer buffer
            (goto-char (sql-grid-test--button-with-help buffer "left"))
            (push-mark (point) t t)
            (goto-char (sql-grid-test--button-with-help buffer "right"))
            (should-error (sql-grid-copy-selection) :type 'user-error)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (kill-buffer source))))

(ert-deftest sql-grid-test-buffer-row-limit-survives-new-results ()
  "Keep a local page-size override without changing other buffers or defaults."
  (let ((source (generate-new-buffer " *sql-grid local limit*"))
        (other-source (generate-new-buffer " *sql-grid other limit*"))
        (sql-grid-result-format 'text)
        (sql-grid-max-rows 1)
        (results (list (sql-grid-result-create
                        :columns '("value") :rows '(("alpha") ("beta") ("gamma")))))
        buffer other)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer (sql-grid-display-results nil results)))
          (with-current-buffer buffer
            (sql-grid-next-page)
            (sql-grid-set-max-rows 2)
            (should (sql-grid-test--button-with-help buffer "alpha"))
            (should (sql-grid-test--button-with-help buffer "beta"))
            (should-not (sql-grid-test--button-with-help buffer "gamma"))
            (sql-grid-refresh)
            (should (sql-grid-test--button-with-help buffer "beta"))
            (sql-grid-set-max-rows nil))
          (with-current-buffer source (sql-grid-display-results nil results))
          (with-current-buffer buffer
            (should (sql-grid-test--button-with-help buffer "gamma"))
            (should-error (sql-grid-next-page) :type 'user-error)
            (dolist (invalid '(0 -1 "2" 2.5))
              (should-error (sql-grid-set-max-rows invalid) :type 'user-error))
            (should (sql-grid-test--button-with-help buffer "gamma")))
          (with-current-buffer other-source
            (setq other (sql-grid-display-results nil results)))
          (should-not (sql-grid-test--button-with-help other "beta"))
          (should (= sql-grid-max-rows 1)))
      (dolist (item (list buffer other source other-source))
        (when (buffer-live-p item) (kill-buffer item))))))

(ert-deftest sql-grid-test-native-and-evil-visual-cell-selection ()
  "Use real native mark and Evil Visual keys to copy complete cell rectangles."
  (skip-unless (and (display-graphic-p) (require 'evil nil t)))
  (let ((source (generate-new-buffer " *sql-grid visual keys*"))
        (sql-grid-result-format 'grid)
        (sql-grid-max-cell-chars 4)
        (sql-grid-focus-results t)
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        buffer)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer
                  (sql-grid-display-results
                   nil (list (sql-grid-result-create
                              :columns '("id" "value" "ignored")
                              :rows '((1 "first full" "unused")
                                      (2 "second full" "unused")))))))
          (select-window (get-buffer-window buffer))
          (dolist (keys '("C-SPC <right> <down> M-w"
                          "S-<right> S-<down> M-w"))
            (sql-grid-test--press-in-buffer
             buffer (sql-grid-test--button-with-help buffer "1") keys)
            (should (equal (current-kill 0) "1\tfirst full\n2\tsecond full")))
          (funcall (intern "evil-local-mode") 1)
          (funcall (intern "evil-normal-state"))
          (dolist (keys '("v l j y" "C-v l j y"))
            (sql-grid-test--press-in-buffer
             buffer (sql-grid-test--button-with-help buffer "1") keys)
            (should (equal (current-kill 0) "1\tfirst full\n2\tsecond full"))
            (should (eq (symbol-value 'evil-state) 'normal)))
          (sql-grid-test--press-in-buffer
           buffer (sql-grid-test--button-with-help buffer "1") "V j y")
          (should (equal (current-kill 0)
                         "1\tfirst full\tunused\n2\tsecond full\tunused")))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (kill-buffer source))))



(provide 'sql-grid-test)
;;; sql-grid-test.el ends here
