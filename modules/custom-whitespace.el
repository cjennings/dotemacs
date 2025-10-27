;;; custom-whitespace.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; This module provides whitespace manipulation operations for cleaning and transforming whitespace in text.

;; Functions include:

;; - removing leading and trailing whitespace
;; - collapsing multiple spaces to single spaces
;; - deleting blank lines
;; - converting whitespace to hyphens.

;; All operations work on the current line, active region, or entire buffer depending on context.

;; Bound to keymap prefix C-; w

;;; Code:

(eval-when-compile (defvar cj/custom-keymap)) ;; cj/custom-keymap defined in keybindings.el

;;; ---------------------- Whitespace Operations And Keymap ---------------------

;; ------------------- Remove Leading/Trailing Whitespace ---------------------

(defun cj/--remove-leading-trailing-whitespace (start end)
  "Internal implementation: Remove leading and trailing whitespace.
START and END define the region to operate on.
Removes leading whitespace (^[ \\t]+) and trailing whitespace ([ \\t]+$).
Preserves interior whitespace."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t) (replace-match "")))))

(defun cj/remove-leading-trailing-whitespace ()
  "Remove leading and trailing whitespace in a region, line, or buffer.
When called interactively:
- If a region is active, operate on the region.
- If called with a \\[universal-argument] prefix, operate on the entire buffer.
- Otherwise, operate on the current line."
  (interactive)
  (let ((start (cond (current-prefix-arg (point-min))
					 ((use-region-p) (region-beginning))
					 (t (line-beginning-position))))
		(end (cond (current-prefix-arg (point-max))
				   ((use-region-p) (region-end))
				   (t (line-end-position)))))
	(cj/--remove-leading-trailing-whitespace start end)))

;; ----------------------- Collapse Whitespace ---------------------------------

(defun cj/--collapse-whitespace (start end)
  "Internal implementation: Collapse whitespace to single spaces.
START and END define the region to operate on.
Converts tabs to spaces, removes leading/trailing whitespace,
and collapses multiple spaces to single space.
Preserves newlines and line structure."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      ;; Replace all tabs with space
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
        (replace-match " " nil t))
      ;; Remove leading and trailing spaces (but not newlines)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+\\|[ \t]+$" nil t)
        (replace-match "" nil nil))
      ;; Ensure only one space between words (but preserve newlines)
      (goto-char (point-min))
      (while (re-search-forward "[ \t]\\{2,\\}" nil t)
        (replace-match " " nil nil)))))

(defun cj/collapse-whitespace-line-or-region ()
  "Collapse whitespace to one space in the current line or active region.
Ensure there is exactly one space between words and remove leading and
trailing whitespace."
  (interactive)
  (let* ((region-active (use-region-p))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active (region-end) (line-end-position))))
    (cj/--collapse-whitespace beg end)))

;; ------------------------ Delete Blank Lines ---------------------------------

(defun cj/--delete-blank-lines (start end)
  "Internal implementation: Delete blank lines between START and END.
Blank lines are lines containing only whitespace or nothing.
Uses the regexp ^[[:space:]]*$ to match blank lines."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (save-excursion
    (save-restriction
      (widen)
      ;; Regexp "^[[:space:]]*$" matches lines of zero or more spaces/tabs/newlines.
      (flush-lines "^[[:space:]]*$" start end))))

(defun cj/delete-blank-lines-region-or-buffer (start end)
  "Delete blank lines between START and END.
Treat blank lines as lines that contain nothing or only whitespace.
Operate on the active region when one exists.
Prompt before operating on the whole buffer when no region is selected.
Signal a user error and do nothing when the user declines.
Restore point to its original position after deletion."
  (interactive
   (if (use-region-p)
       ;; grab its boundaries if there's a region
       (list (region-beginning) (region-end))
     ;; or ask if user intended operating on whole buffer
     (if (yes-or-no-p "Delete blank lines in entire buffer? ")
         (list (point-min) (point-max))
       (user-error "Aborted"))))
  (cj/--delete-blank-lines start end)
  ;; Return nil (Emacs conventions). Point is already restored.
  nil)

;; ------------------------- Hyphenate Whitespace ------------------------------

(defun cj/--hyphenate-whitespace (start end)
  "Internal implementation: Replace whitespace runs with hyphens.
START and END define the region to operate on.
Replaces all runs of spaces, tabs, newlines, and carriage returns with hyphens."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n\r]+" nil t)
        (replace-match "-")))))

(defun cj/hyphenate-whitespace-in-region (start end)
  "Replace runs of whitespace between START and END with hyphens.
Operate on the active region designated by START and END."
  (interactive "*r")
  (if (use-region-p)
      (cj/--hyphenate-whitespace start end)
    (message "No region; nothing to hyphenate.")))


;; Whitespace operations prefix and keymap
(defvar-keymap cj/whitespace-map
  :doc "Keymap for whitespace operations"
  "r" #'cj/remove-leading-trailing-whitespace
  "c" #'cj/collapse-whitespace-line-or-region
  "l" #'cj/delete-blank-lines-region-or-buffer
  "-" #'cj/hyphenate-whitespace-in-region)

(keymap-set cj/custom-keymap "w" cj/whitespace-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; w" "whitespace menu"))

(provide 'custom-whitespace)
;;; custom-whitespace.el ends here.
