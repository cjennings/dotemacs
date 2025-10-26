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


;;; ---------------------- Whitespace Operations And Keymap ---------------------

(defun cj/remove-leading-trailing-whitespace ()
  "Remove leading and trailing whitespace in a region, line, or buffer.
When called interactively:
- If a region is active, operate on the region.
- If called with a \[universal-argument] prefix, operate on the entire buffer.
- Otherwise, operate on the current line."
  (interactive)
  (let ((start (cond (current-prefix-arg (point-min))
					 ((use-region-p) (region-beginning))
					 (t (line-beginning-position))))
		(end (cond (current-prefix-arg (point-max))
				   ((use-region-p) (region-end))
				   (t (line-end-position)))))
	(save-excursion
	  (save-restriction
		(narrow-to-region start end)
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
		(goto-char (point-min))
		(while (re-search-forward "[ \t]+$" nil t) (replace-match ""))))))

(defun cj/collapse-whitespace-line-or-region ()
  "Collapse whitespace to one space in the current line or active region.
Ensure there is exactly one space between words and remove leading and trailing whitespace."
  (interactive)
  (save-excursion
	(let* ((region-active (use-region-p))
		   (beg (if region-active (region-beginning) (line-beginning-position)))
		   (end (if region-active (region-end) (line-end-position))))
	  (save-restriction
		(narrow-to-region beg end)
		;; Replace all tabs with space
		(goto-char (point-min))
		(while (search-forward "\t" nil t)
		  (replace-match " " nil t))
		;; Remove leading and trailing spaces
		(goto-char (point-min))
		(while (re-search-forward "^\\s-+\\|\\s-+$" nil t)
		  (replace-match "" nil nil))
		;; Ensure only one space between words/symbols
		(goto-char (point-min))
		(while (re-search-forward "\\s-\\{2,\\}" nil t)
		  (replace-match " " nil nil))))))

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
  (save-excursion
	(save-restriction
	  (widen)
	  ;; Regexp "^[[:space:]]*$" matches lines of zero or more spaces/tabs.
	  (flush-lines "^[[:space:]]*$" start end)))
  ;; Return nil (Emacs conventions). Point is already restored.
  nil)

(defun cj/hyphenate-whitespace-in-region (start end)
  "Replace runs of whitespace between START and END with hyphens.
Operate on the active region designated by START and END."
  (interactive "*r")
  (if (use-region-p)
	  (save-excursion
		(save-restriction
		  (narrow-to-region start end)
		  (goto-char (point-min))
		  (while (re-search-forward "[ \t\n\r]+" nil t)
			(replace-match "-"))))
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
