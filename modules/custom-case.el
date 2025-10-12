;;; custom-case.el --- Custom Functions Handling Text Case -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun cj/upcase-dwim ()
  "Upcase the active region, or upcase the symbol at point if no region."
  (interactive)
  (if (use-region-p)
	  (upcase-region (region-beginning) (region-end))
	(let ((bounds (bounds-of-thing-at-point 'symbol)))
	  (if bounds
		  (upcase-region (car bounds) (cdr bounds))
		(user-error "No symbol at point")))))

(defun cj/downcase-dwim ()
  "Downcase the active region, or downcase the symbol at point if no region."
  (interactive)
  (if (use-region-p)
	  (downcase-region (region-beginning) (region-end))
	(let ((bounds (bounds-of-thing-at-point 'symbol)))
	  (if bounds
		  (downcase-region (car bounds) (cdr bounds))
		(user-error "No symbol at point")))))

(defun cj/title-case-region ()
  "Capitalize the region in title case format.
Title case is a capitalization convention where major words
are capitalized,and most minor words are lowercase.  Nouns,
verbs (including linking verbs), adjectives, adverbs,pronouns,
and all words of four letters or more are considered major words.
Short (i.e., three letters or fewer) conjunctions, short prepositions,
and all articles are considered minor words."
  (interactive)
  (let ((beg nil)
		(end nil)
		(prev-word-end nil)
		;; Allow capitals for skip characters after this, so:
		;;   Warning: An Example
		;; Capitalizes the `An'.
		(chars-skip-reset '(?: ?! ??))
		;; Don't capitalize characters directly after these. e.g.
		;; "Foo-bar" or "Foo\bar" or "Foo's".

		(chars-separator '(?\\ ?- ?' ?.))

		(word-chars "[:alnum:]")
		(word-skip
		 (list "a" "an" "and" "as" "at" "but" "by"
			   "for" "if" "in" "is" "nor" "of"
			   "on" "or" "so" "the" "to" "yet"))
		(is-first t))
	(cond
	 ((region-active-p)
	  (setq beg (region-beginning))
	  (setq end (region-end)))
	 (t
	  (setq beg (line-beginning-position))
	  (setq end (line-end-position))))
	(save-excursion
	  ;; work on uppercased text (e.g., headlines) by downcasing first
	  (downcase-region beg end)
	  (goto-char beg)

	  (while (< (point) end)
		(setq prev-word-end (point))
		(skip-chars-forward (concat "^" word-chars) end)
		(let ((word-end
			   (save-excursion
				 (skip-chars-forward word-chars end)
				 (point))))

		  (unless (memq (char-before (point)) chars-separator)
			(let* ((c-orig (char-to-string (char-after (point))))
				   (c-up (capitalize c-orig)))
			  (unless (string-equal c-orig c-up)
				(let ((word (buffer-substring-no-properties (point) word-end)))
				  (when
					  (or
					   ;; Always allow capitalization.
					   is-first
					   ;; If it's not a skip word, allow.
					   (not (member word word-skip))
					   ;; Check the beginning of the previous word doesn't reset first.
					   (save-excursion
						 (and
						  (not (zerop
								(skip-chars-backward "[:blank:]" prev-word-end)))
						  (memq (char-before (point)) chars-skip-reset))))
					(delete-region (point) (1+ (point)))
					(insert c-up))))))
		  (goto-char word-end)
		  (setq is-first nil))))))
;; replace the capitalize-region keybinding to call title-case
(global-set-key [remap capitalize-region] 'cj/title-case-region)

;; Case-change operations prefix and keymap
(define-prefix-command 'cj/case-map nil
					   "Keymap for case-change operations.")
(define-key cj/custom-keymap "c" 'cj/case-map)
(define-key cj/case-map "t" 'cj/title-case-region)
(define-key cj/case-map "u" 'cj/upcase-dwim)
(define-key cj/case-map "l" 'cj/downcase-dwim) ;; for "lower" case

(provide 'custom-case)
;;; custom-case.el ends here.
