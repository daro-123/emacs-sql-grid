;;; sql-grid-selection.el --- Rectangular result-cell selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Select complete result cells through the native mark or Evil Visual state.
;; Copy original values as tab-separated plain text, without table decoration.

;;; Code:

(require 'cl-lib)
(require 'button)

(defvar evil-state)
(defvar evil-visual-selection)
(defvar evil-visual-mark)
(defvar evil-visual-point)
(defvar evil-visual-region-expanded)

(defvar-local sql-grid--selection-overlays nil)

(defun sql-grid--selection-active-p ()
  "Return non-nil when a native or Evil cell selection is active."
  (or (region-active-p)
      (and (boundp 'evil-state) (eq evil-state 'visual))))

(defun sql-grid--selection-cell (position)
  "Return the cell at POSITION, including its trailing boundary."
  (when position
    (or (button-at position)
        (and (> position (point-min)) (button-at (1- position))))))

(defun sql-grid--selected-cell-rows ()
  "Return selected button rows in visible order, or nil for invalid endpoints.
Native and character/block Visual selections use an inclusive cell rectangle.
Evil line selections include every column of the selected rows.  Endpoints
must belong to the same displayed result set."
  (when (sql-grid--selection-active-p)
    (let* ((expanded (and (boundp 'evil-state) (eq evil-state 'visual)
                          (boundp 'evil-visual-region-expanded)
                          evil-visual-region-expanded))
           (anchor (sql-grid--selection-cell
                    (if expanded evil-visual-mark (mark t))))
           (target (sql-grid--selection-cell
                    (if expanded evil-visual-point (point))))
           (from (and anchor (button-get anchor 'sql-grid-cell)))
           (to (and target (button-get target 'sql-grid-cell))))
      (when (and from to (= (car from) (car to)))
        (let* ((set-index (car from))
               (earlier (if (< (button-start anchor) (button-start target))
                            anchor target))
               (later (if (> (button-start anchor) (button-start target))
                          anchor target))
               (earlier-coordinate (button-get earlier 'sql-grid-cell))
               (later-coordinate (button-get later 'sql-grid-cell))
               (earlier-row (cadr earlier-coordinate))
               (later-row (cadr later-coordinate))
               (linewise (and (boundp 'evil-state) (eq evil-state 'visual)
                              (boundp 'evil-visual-selection)
                              (eq evil-visual-selection 'line)))
               rows row row-coordinate columns left right button start finish)
          (setq start earlier
                finish later)
          (while (let* ((previous (previous-button (button-start start)))
                        (coordinate (and previous
                                         (button-get previous 'sql-grid-cell))))
                   (when (and coordinate (= (car coordinate) set-index)
                              (equal (cadr coordinate) earlier-row))
                     (setq start previous))))
          (while (let* ((next (next-button (button-end finish) t))
                        (coordinate (and next
                                         (button-get next 'sql-grid-cell))))
                   (when (and coordinate (= (car coordinate) set-index)
                              (equal (cadr coordinate) later-row))
                     (setq finish next))))
          (setq button (next-button (button-start start) t))
          (while (and button (<= (button-start button) (button-start finish)))
            (let ((coordinate (button-get button 'sql-grid-cell)))
              (when (and coordinate (= (car coordinate) set-index))
                (unless (equal (cadr coordinate) row-coordinate)
                  (when row (push (nreverse row) rows))
                  (setq row nil row-coordinate (cadr coordinate)))
                (push button row)))
            (setq button (next-button (button-end button) t)))
          (when row (push (nreverse row) rows))
          (setq rows (nreverse rows)
                columns (mapcar (lambda (cell)
                                  (nth 2 (button-get cell 'sql-grid-cell)))
                                (car rows))
                left (cl-position (nth 2 from) columns)
                right (cl-position (nth 2 to) columns))
          (when (and rows (or linewise (and left right)))
            (mapcar (lambda (cells)
                      (if linewise cells
                        (cl-subseq cells (min left right)
                                   (1+ (max left right)))))
                    rows)))))))

(defun sql-grid--clear-selection ()
  "Remove cell-selection highlights."
  (mapc #'delete-overlay sql-grid--selection-overlays)
  (setq sql-grid--selection-overlays nil))

(defun sql-grid--highlight-selection (&optional _window)
  "Highlight complete selected cells rather than buffer separators."
  (let* ((rows (sql-grid--selected-cell-rows))
         (cells (apply #'append rows))
         (old sql-grid--selection-overlays)
         overlays)
    (when (and rows (boundp 'evil-state) (eq evil-state 'visual)
               (fboundp 'evil-visual-highlight))
      (funcall #'evil-visual-highlight -1))
    (dolist (cell cells)
      (let ((overlay (or (pop old) (make-overlay 1 1))))
        (move-overlay overlay (button-start cell) (button-end cell))
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'priority 100)
        (push overlay overlays)))
    (mapc #'delete-overlay old)
    (setq sql-grid--selection-overlays (nreverse overlays))))

(defun sql-grid--highlight-region (start end window overlay)
  "Highlight selected cells or ordinary region START to END in WINDOW.
Reuse OVERLAY while suppressing the ordinary region over table decoration."
  (let ((overlay (or overlay (make-overlay start end))))
    (move-overlay overlay start (if (sql-grid--selected-cell-rows) start end))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'face 'region)
    overlay))

(defun sql-grid--setup-selection ()
  "Enable native and Evil cell-selection highlighting in this buffer."
  (setq-local redisplay-highlight-region-function #'sql-grid--highlight-region)
  (add-hook 'post-command-hook #'sql-grid--highlight-selection t t)
  (add-hook 'pre-redisplay-functions #'sql-grid--highlight-selection t t)
  (add-hook 'deactivate-mark-hook #'sql-grid--clear-selection nil t)
  (add-hook 'change-major-mode-hook #'sql-grid--clear-selection nil t))

(defun sql-grid-copy-selection ()
  "Copy selected cells as plain text using complete original scalar values.
Separate columns with tabs and rows with newlines, without grid decoration.
Preserve embedded whitespace; SQL NULL copies as NULL, empty strings as empty."
  (interactive)
  (let ((rows (sql-grid--selected-cell-rows)))
    (unless rows
      (user-error "Select cells within one displayed result set"))
    (kill-new
     (mapconcat
      (lambda (cells)
        (mapconcat (lambda (cell)
                     (let ((value (button-get cell 'sql-grid-value)))
                       (if (null value) "NULL" (format "%s" value))))
                   cells "\t"))
      rows "\n"))
    (when (and (boundp 'evil-state) (eq evil-state 'visual)
               (fboundp 'evil-exit-visual-state))
      (funcall #'evil-exit-visual-state))
    (deactivate-mark)
    (sql-grid--clear-selection)
    (message "Copied selected cells (untruncated plain text)")))

(provide 'sql-grid-selection)
;;; sql-grid-selection.el ends here
