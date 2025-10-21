;;; custom-misc.el --- Miscellaneous utility functions  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides various utility functions for text manipulation,
;; formatting, and navigation.  Features include:
;; - Jump between matching delimiters
;; - Format regions/buffers (untabify, reindent, remove trailing whitespace)
;; - Word counting with region awareness
;; - Fraction glyph conversion (¼ ↔ 1/4)
;; - Force align-regexp to use spaces instead of tabs
;;
;; All functions are bound to the cj/custom-keymap for easy access.
;;
;;; Code:

;; cj/custom-keymap defined in keybindings.el
(eval-when-compile (defvar cj/custom-keymap))

(defun cj/jump-to-matching-paren ()
  "Jump to the matching delimiter if point is on (or just after) one.
If not on a delimiter, show a message. Respects the current syntax table."
  (interactive)
  (let* ((ca (char-after))
		 (cb (char-before))
		 ;; Check if on opening paren
		 (open-p (and ca (eq (char-syntax ca) ?\()))
		 ;; Check if on or just after closing paren
		 (close-p (or (and ca (eq (char-syntax ca) ?\)))
					  (and cb (eq (char-syntax cb) ?\))))))
	(cond
	 ;; Jump forward from opening
	 (open-p
	  (condition-case err
		  (forward-sexp)
		(scan-error
		 (message "No matching delimiter: %s" (error-message-string err)))))
	 ;; Jump backward from closing
	 (close-p
	  (condition-case err
		  (backward-sexp)
		(scan-error
		 (message "No matching delimiter: %s" (error-message-string err)))))
	 ;; Not on delimiter
	 (t
	  (message "Point is not on a delimiter.")))))


(defun cj/format-region-or-buffer ()
  "Reformat the region or the entire buffer.
Replaces tabs with spaces, deletes trailing whitespace, and reindents."
  (interactive)
  (let ((start-pos (if (use-region-p) (region-beginning) (point-min)))
		(end-pos (if (use-region-p) (region-end) (point-max))))
	(save-excursion
	  (save-restriction
		(narrow-to-region start-pos end-pos)
		(untabify (point-min) (point-max))
		(indent-region (point-min) (point-max))
		(delete-trailing-whitespace (point-min) (point-max))))
	(message "Formatted %s" (if (use-region-p) "region" "buffer"))))


(defun cj/count-words-buffer-or-region ()
  "Count the number of words in the buffer or region.
Display the result in the minibuffer."
  (interactive)
  (let* ((use-region (use-region-p))
		 (begin (if use-region (region-beginning) (point-min)))
		 (end (if use-region (region-end) (point-max)))
		 (area-type (if use-region "the region" "the buffer")))
	(message "There are %d words in %s." (count-words begin end) area-type)))


(defun cj/replace-fraction-glyphs (start end)
  "Replace common fraction glyphs between START and END.
Operate on the buffer or region designated by START and END.
Replace the text representations with glyphs when called with a
\\[universal-argument] prefix."
  (interactive (if (use-region-p)
				   (list (region-beginning) (region-end))
				 (list (point-min) (point-max))))
  (let ((replacements (if current-prefix-arg
						  '(("1/4" . "¼")
							("1/2" . "½")
							("3/4" . "¾")
							("1/3" . "⅓")
							("2/3" . "⅔"))
						'(("¼" . "1/4")
						  ("½" . "1/2")
						  ("¾" . "3/4")
						  ("⅓" . "1/3")
						  ("⅔" . "2/3"))))
		(count 0))
	(save-excursion
	  (dolist (r replacements)
		(goto-char start)
		(while (search-forward (car r) end t)
		  (replace-match (cdr r))
		  (setq count (1+ count)))))
	(message "Replaced %d fraction%s" count (if (= count 1) "" "s"))))

(defun cj/align-regexp-with-spaces (orig-fun &rest args)
  "Call ORIG-FUN with ARGS while temporarily disabling tabs for alignment.
This advice ensures =align-regexp' uses spaces by binding =indent-tabs-mode'
to nil."
  (let ((indent-tabs-mode nil))
	(apply orig-fun args)))

;; avoid double advice stacking in case the file is reloaded
(advice-remove 'align-regexp #'cj/align-regexp-with-spaces)
(advice-add    'align-regexp :around #'cj/align-regexp-with-spaces)

(keymap-set cj/custom-keymap ")" #'cj/jump-to-matching-paren)
(keymap-set cj/custom-keymap "f" #'cj/format-region-or-buffer)
(keymap-set cj/custom-keymap "W" #'cj/count-words-buffer-or-region)
(keymap-set cj/custom-keymap "/" #'cj/replace-fraction-glyphs)
(keymap-set cj/custom-keymap "A" #'align-regexp)
(keymap-set cj/custom-keymap "|" #'display-fill-column-indicator-mode)

(provide 'custom-misc)
;;; custom-misc.el ends here
