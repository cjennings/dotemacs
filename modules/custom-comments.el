;;; custom-comments.el --- Custom Comment Operations -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;; This module provides custom comment formatting and manipulation utilities for code editing.
;;
;; Functions include:
;; - deleting all comments in a buffer,
;; - reformatting commented text into single-line paragraphs,
;; - creating centered comment headers with customizable separator characters,
;; - creating comment boxes around text
;; - inserting hyphen-style centered comments.
;;
;; These utilities help create consistent, well-formatted code comments and section headers.
;; Bound to keymap prefix: C-; C
;;
;;
;;; Code:

(eval-when-compile (require 'keybindings)) ;; for keymapping below
(autoload 'cj/join-line-or-region "custom-line-paragraph" nil t)

;; --------------------------- Delete Buffer Comments --------------------------

(defun cj/delete-buffer-comments ()
  "Delete all comments within the current buffer."
  (interactive)
  (goto-char (point-min))
  (let (kill-ring)
	(comment-kill (count-lines (point-min) (point-max)))))

;; ------------------------------ Comment Reformat -----------------------------

(defun cj/comment-reformat ()
  "Reformat commented text into a single paragraph."
  (interactive)
  (if mark-active
	  (let ((beg (region-beginning))
			(end (copy-marker (region-end)))
			(orig-fill-column fill-column))
		(uncomment-region beg end)
		(setq fill-column (- fill-column 3))
		(cj/join-line-or-region beg end)
		(comment-region beg end)
		(setq fill-column orig-fill-column )))
  ;; if no region
  (message "No region was selected. Select the comment lines to reformat."))

;; ------------------------------ Comment Centered -----------------------------

(defun cj/comment-centered (&optional comment-char)
  "Insert comment text centered around the COMMENT-CHAR character.
Default to the hash character when COMMENT-CHAR is nil.
Use the lesser of `fill-column' or 80 to calculate the comment length.
Begin and end line with the appropriate comment symbols for the current mode."
  (interactive)
  (if (not (char-or-string-p comment-char))
	  (setq comment-char "#"))
  (let* ((comment (capitalize (string-trim (read-from-minibuffer "Comment: "))))
		 (fill-column (min fill-column 80))
		 (comment-length (length comment))
		 ;; (comment-start-length (length comment-start))
		 ;; (comment-end-length (length comment-end))
		 (current-column-pos (current-column))
		 (space-on-each-side (/ (- fill-column
								   current-column-pos
								   comment-length
								   (length comment-start)
								   (length comment-end)
								   ;; Single space on each side of comment
								   (if (> comment-length 0) 2 0)
								   ;; Single space after comment syntax sting
								   1)
								2)))
	(if (< space-on-each-side 2)
		(message "Comment string is too big to fit in one line")
	  (progn
		(insert comment-start)
		(when (equal comment-start ";") ;; emacs-lisp line comments are ';;'
		  (insert comment-start))       ;; so insert comment-char again
		(insert " ")
		(dotimes (_ space-on-each-side) (insert comment-char))
		(when (> comment-length 0) (insert " "))
		(insert comment)
		(when (> comment-length 0) (insert " "))
		(dotimes (_ (if (= (% comment-length 2) 0)
						(- space-on-each-side 1)
					  space-on-each-side))
		  (insert comment-char))
		;; Only insert trailing space and comment-end if comment-end is not empty
		(when (not (string-empty-p comment-end))
		  (insert " ")
		  (insert comment-end))))))

;; -------------------------------- Comment Box --------------------------------

(defun cj/comment-box ()
  "Insert a comment box around text that the user inputs.
The box extends to the fill column, centers the text, and uses the current
mode's comment syntax at both the beginning and end of each line. The box
respects the current indentation level and avoids trailing whitespace."
  (interactive)
  (let* ((comment-char (if (equal comment-start ";") ";;"
						 (string-trim comment-start)))
		 (comment-end-char (if (string-empty-p comment-end)
							   comment-char
							 (string-trim comment-end)))
		 (line-char (if (equal comment-char ";;") "-" "#"))
		 (comment (capitalize (string-trim (read-from-minibuffer "Comment: "))))
		 (comment-length (length comment))
		 (current-column-pos (current-column))
		 (max-width (min fill-column 80))
		 ;; Calculate available width between comment markers
		 (available-width (- max-width
							 current-column-pos
							 (length comment-char)
							 (length comment-end-char)))
		 ;; Inner width is the width without the spaces after comment start and before comment end
		 (inner-width (- available-width 2))
		 ;; Calculate padding for each side of the centered text
		 (padding-each-side (max 1 (/ (- inner-width comment-length) 2)))
		 ;; Adjust for odd-length comments
		 (right-padding (if (= (% (- inner-width comment-length) 2) 0)
							padding-each-side
						  (1+ padding-each-side))))

	;; Check if we have enough space
	(if (< inner-width (+ comment-length 4)) ; minimum sensible width
		(message "Comment string is too big to fit in one line")
	  (progn
		;; Top line - fill entirely with line characters except for space after comment start
		(insert comment-char)
		(insert " ")
		(insert (make-string inner-width (string-to-char line-char)))
		(insert " ")
		(insert comment-end-char)
		(newline)

		;; Add indentation on the new line to match current column
		(dotimes (_ current-column-pos) (insert " "))

		;; Middle line with centered text
		(insert comment-char)
		(insert " ")
		;; Left padding
		(dotimes (_ padding-each-side) (insert " "))
		;; The comment text
		(insert comment)
		;; Right padding
		(dotimes (_ right-padding) (insert " "))
		(insert " ")
		(insert comment-end-char)
		(newline)

		;; Add indentation on the new line to match current column
		(dotimes (_ current-column-pos) (insert " "))

		;; Bottom line - same as top line
		(insert comment-char)
		(insert " ")
		(dotimes (_ inner-width) (insert line-char))
		(insert " ")
		(insert comment-end-char)
		(newline)))))

;; ------------------------------- Comment Hyphen ------------------------------

(defun cj/comment-hyphen()
  "Insert a centered comment with '-' (hyphens) on each side.
Leverages cj/comment-centered."
  (interactive)
  (cj/comment-centered "-"))

;; ------------------------------- Comment Keymap ------------------------------

;; Comment styles & comment removal keymap.
(defvar-keymap cj/comment-map
  :doc "Keymap for code comment operations."
  "r" #'cj/comment-reformat
  "c" #'cj/comment-centered
  "-" #'cj/comment-hyphen
  "b" #'cj/comment-box
  "D" #'cj/delete-buffer-comments)
(keymap-set cj/custom-keymap "C" cj/comment-map)

(provide 'custom-comments)
;;; custom-comments.el ends here.
