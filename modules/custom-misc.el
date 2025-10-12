;;; custom-misc.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:


(defun cj/jump-to-matching-paren ()
  "Jump to the matching parenthesis when point is on one.

Signal a message when point is not on a parenthesis."
  (interactive)
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
		 (forward-list))
		((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
		 (backward-list))
		(t (message "Cursor doesn't follow parenthesis, so there's no match."))))

(defun cj/format-region-or-buffer ()
  "Reformat the region or the entire buffer.
Replaces tabs with spaces, deletes trailing whitespace, and reindents the region."
  (interactive)
  (let ((start-pos (if (use-region-p) (region-beginning) (point-min)))
		(end-pos (if (use-region-p) (region-end) (point-max))))
	(save-excursion
	  (save-restriction
		(narrow-to-region start-pos end-pos)
		(untabify (point-min) (point-max)))
	  (indent-region (point-min) (point-max))
	  (delete-trailing-whitespace))))

(defun cj/count-words-buffer-or-region ()
  "Count the number of words in the buffer or region.
Display the result in the minibuffer and *Messages* buffer."
  (interactive)
  (let ((begin (point-min))
		(end (point-max))
		(area_type "the buffer"))
	(when mark-active
	  (setq begin (region-beginning)
			end (region-end)
			area_type "the region"))
	(message (format "There are %d words in %s." (count-words begin end) area_type))))

(defun cj/replace-fraction-glyphs (start end)
  "Replace common fraction glyphs between START and END.
Operate on the buffer or region designated by START and END.
Replace the text representations with glyphs when called with a \[universal-argument] prefix."
  (interactive (if (use-region-p)
				   (list (region-beginning) (region-end))
				 (list (point-min) (point-max))))
  (let ((replacements (if current-prefix-arg
						  '(("1/4" . "¼")
							("1/2" . "½")
							("3/4" . "¾")
							("1/3" .  "⅓")
							("2/3" . "⅔"))
						'(("¼" . "1/4")
						  ("½" . "1/2")
						  ("¾" . "3/4")
						  ("⅓" . "1/3")
						  ("⅔" . "2/3")))))
	(save-excursion
	  (dolist (r replacements)
		(goto-char start)
		(while (search-forward (car r) end t)
		  (replace-match (cdr r)))))))

(defun cj/align-regexp-with-spaces (orig-fun &rest args)
  "Call ORIG-FUN with ARGS while temporarily disabling tabs for alignment.

This advice ensures `align-regexp' uses spaces by binding `indent-tabs-mode' to nil."
  (let ((indent-tabs-mode nil))
	(apply orig-fun args)))

(advice-remove 'align-regexp #'align-regexp-with-spaces) ; in case this is reloaded
(advice-add    'align-regexp :around #'cj/align-regexp-with-spaces)


(define-key cj/custom-keymap ")" 'cj/jump-to-matching-paren)
(define-key cj/custom-keymap "f" 'cj/format-region-or-buffer)
(define-key cj/custom-keymap "W" 'cj/count-words-buffer-or-region)
(define-key cj/custom-keymap "/" 'cj/replace-fraction-glyphs)
(define-key cj/custom-keymap "A" 'align-regexp)
(define-key cj/custom-keymap "B" 'toggle-debug-on-error)
(define-key cj/custom-keymap "|" 'display-fill-column-indicator-mode)

(provide 'custom-misc)
;;; custom-misc.el ends here.
