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


(defun cj/--format-region (start end)
  "Internal implementation: Reformat text between START and END.
START and END define the region to operate on.
Replaces tabs with spaces, reindents, and deletes trailing whitespace."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max)))))

(defun cj/format-region-or-buffer ()
  "Reformat the region or the entire buffer.
Replaces tabs with spaces, deletes trailing whitespace, and reindents."
  (interactive)
  (let ((start-pos (if (use-region-p) (region-beginning) (point-min)))
        (end-pos (if (use-region-p) (region-end) (point-max))))
    (cj/--format-region start-pos end-pos)
    (message "Formatted %s" (if (use-region-p) "region" "buffer"))))

(defun cj/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun cj/--count-words (start end)
  "Internal implementation: Count words between START and END.
START and END define the region to count.
Returns the word count as an integer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (count-words start end))

(defun cj/count-words-buffer-or-region ()
  "Count the number of words in the buffer or region.
Display the result in the minibuffer."
  (interactive)
  (let* ((use-region (use-region-p))
		 (begin (if use-region (region-beginning) (point-min)))
		 (end (if use-region (region-end) (point-max)))
		 (area-type (if use-region "the region" "the buffer"))
		 (word-count (cj/--count-words begin end)))
	(message "There are %d words in %s." word-count area-type)))

(defun cj/--count-characters (start end)
  "Internal implementation: Count characters between START and END.
START and END define the region to count.
Returns the character count as an integer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (- end start))

(defun cj/count-characters-buffer-or-region ()
  "Count the number of characters in the buffer or region.
Display the result in the minibuffer."
  (interactive)
  (let* ((use-region (use-region-p))
		 (begin (if use-region (region-beginning) (point-min)))
		 (end (if use-region (region-end) (point-max)))
		 (area-type (if use-region "the region" "the buffer"))
		 (char-count (cj/--count-characters begin end)))
	(message "There are %d characters in %s." char-count area-type)))


(defun cj/--replace-fraction-glyphs (start end to-glyphs)
  "Internal implementation: Replace fraction glyphs or text between START and END.
START and END define the region to operate on.
TO-GLYPHS when non-nil converts text (1/4) to glyphs (¼),
otherwise converts glyphs to text."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((replacements (if to-glyphs
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
        (count 0)
        (end-marker (copy-marker end)))
    (save-excursion
      (dolist (r replacements)
        (goto-char start)
        (while (search-forward (car r) end-marker t)
          (replace-match (cdr r))
          (setq count (1+ count)))))
    count))

(defun cj/replace-fraction-glyphs (start end)
  "Replace common fraction glyphs between START and END.
Operate on the buffer or region designated by START and END.
Replace the text representations with glyphs when called with a
\\[universal-argument] prefix."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((count (cj/--replace-fraction-glyphs start end current-prefix-arg)))
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
(keymap-set cj/custom-keymap "C" #'cj/count-characters-buffer-or-region)
(keymap-set cj/custom-keymap "/" #'cj/replace-fraction-glyphs)
(keymap-set cj/custom-keymap "A" #'align-regexp)
(keymap-set cj/custom-keymap "SPC" #'cj/switch-to-previous-buffer)
(keymap-set cj/custom-keymap "|" #'display-fill-column-indicator-mode)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; )" "jump to paren"
    "C-; f" "format buffer"
    "C-; W" "count words"
    "C-; C" "count characters"
    "C-; /" "fraction glyphs"
    "C-; A" "align regexp"
    "C-; SPC" "prev buffer"
    "C-; |" "fill column"))

(provide 'custom-misc)
;;; custom-misc.el ends here
