;;; custom-ordering.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; This module provides functions for converting text between different formats and sorting operations.
;; These utilities are useful for reformatting data structures and organizing text.

;; Functions include:

;; - converting lines to quoted comma-separated arrays (arrayify)
;; - converting arrays back to separate lines (unarrayify)
;; - alphabetically sorting words in a region
;; - splitting comma-separated text into individual lines

;; Bound to keymap prefix C-; o

;;; Code:

;; cj/custom-keymap defined in keybindings.el
(eval-when-compile (defvar cj/custom-keymap))
(defvar cj/ordering-map)

(defun cj/arrayify (start end quote)
  "Convert lines between START and END into quoted, comma-separated strings.
START and END identify the active region.
QUOTE specifies the quotation characters to surround each element."
  (interactive "r\nMQuotation character to use for array element: ")
  (let ((insertion
		 (mapconcat
		  (lambda (x) (format "%s%s%s" quote x quote))
		  (split-string (buffer-substring start end)) ", ")))
	(delete-region start end)
	(insert insertion)))

(defun cj/unarrayify (start end)
  "Convert quoted comma-separated strings between START and END to separate lines.
START and END identify the active region."
  (interactive "r")
  (let ((insertion
		 (mapconcat
		  (lambda (x) (replace-regexp-in-string "[\"']" "" x))
		  (split-string (buffer-substring start end) ", ") "\n")))
	(delete-region start end)
	(insert insertion)))

(defun cj/alphabetize-region ()
  "Alphabetize words in the active region and replace the original text.
Produce a comma-separated list as the result."
  (interactive)
  (unless (use-region-p)
	(user-error "No region selected"))
  (let ((start (region-beginning))
		(end (region-end))
		(string (buffer-substring-no-properties (region-beginning) (region-end))))
	(delete-region start end)
	(goto-char start)
	(insert
	 (mapconcat #'identity
				(sort (split-string string "[[:space:],]+" t)
					  #'string-lessp)
				", "))))

(defun cj/comma-separated-text-to-lines ()
  "Break up comma-separated text in active region so each item is on own line."
  (interactive)
  (if (not (region-active-p))
	  (error "No region selected"))

  (let ((beg (region-beginning))
		(end (region-end))
		(text (buffer-substring-no-properties (region-beginning) (region-end))))
	(with-temp-buffer
	  (insert text)
	  (goto-char (point-min))
	  (while (search-forward "," nil t)
		(replace-match "\n" nil t))
	  (delete-trailing-whitespace)
	  (setq text (buffer-string)))

	(delete-region beg end)
	(goto-char beg)
	(insert text)))



;; Ordering & sorting prefix and keymap
(defvar-keymap cj/ordering-map
  :doc "Keymap for text ordering and sorting operations"
  "a" #'cj/arrayify
  "u" #'cj/unarrayify
  "A" #'cj/alphabetize-region
  "l" #'cj/comma-separated-text-to-lines)

(keymap-set cj/custom-keymap "o" cj/ordering-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; o" "ordering/sorting menu"))

(provide 'custom-ordering)
;;; custom-ordering.el ends here.
