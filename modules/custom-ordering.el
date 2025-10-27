;;; custom-ordering.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Text transformation and sorting utilities for reformatting data structures.
;;
;; Array/list formatting:
;; - arrayify/listify - convert lines to comma-separated format (with/without quotes, brackets)
;; - unarrayify - convert arrays back to separate lines
;;
;; Line manipulation:
;; - toggle-quotes - swap double ↔ single quotes
;; - reverse-lines - reverse line order
;; - number-lines - add line numbers with custom format (supports zero-padding)
;; - alphabetize-region - sort words alphabetically
;; - comma-separated-text-to-lines - split CSV text into lines
;;
;; Convenience functions: listify, arrayify-json, arrayify-python
;; Bound to keymap prefix C-; o

;;; Code:

(require 'cl-lib)

;; cj/custom-keymap defined in keybindings.el
(eval-when-compile (defvar cj/custom-keymap))
(defvar cj/ordering-map)

(defun cj/--arrayify (start end quote &optional prefix suffix)
  "Internal implementation: Convert lines to quoted, comma-separated format.
START and END define the region to operate on.
QUOTE specifies the quotation characters to surround each element.
  Use \"\" for no quotes, \"\\\"\" for double quotes, \"'\" for single quotes.
PREFIX is an optional string to prepend to the result (e.g., \"[\" or \"(\").
SUFFIX is an optional string to append to the result (e.g., \"]\" or \")\").
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((result (mapconcat
                 (lambda (x) (format "%s%s%s" quote x quote))
                 (split-string (buffer-substring start end)) ", ")))
    (concat (or prefix "") result (or suffix ""))))

(defun cj/arrayify (start end quote)
  "Convert lines between START and END into quoted, comma-separated strings.
START and END identify the active region.
QUOTE specifies the quotation characters to surround each element."
  (interactive "r\nMQuotation character to use for array element: ")
  (let ((insertion (cj/--arrayify start end quote)))
	(delete-region start end)
	(insert insertion)))

(defun cj/listify (start end)
  "Convert lines between START and END into an unquoted, comma-separated list.
START and END identify the active region.
Example: `apple banana cherry' becomes `apple, banana, cherry'."
  (interactive "r")
  (let ((insertion (cj/--arrayify start end "")))
    (delete-region start end)
    (insert insertion)))

(defun cj/arrayify-json (start end)
  "Convert lines between START and END into a JSON-style array.
START and END identify the active region.
Example: `apple banana cherry' becomes `[\"apple\", \"banana\", \"cherry\"]'."
  (interactive "r")
  (let ((insertion (cj/--arrayify start end "\"" "[" "]")))
    (delete-region start end)
    (insert insertion)))

(defun cj/arrayify-python (start end)
  "Convert lines between START and END into a Python-style list.
START and END identify the active region.
Example: `apple banana cherry' becomes `[\"apple\", \"banana\", \"cherry\"]'."
  (interactive "r")
  (let ((insertion (cj/--arrayify start end "\"" "[" "]")))
    (delete-region start end)
    (insert insertion)))

(defun cj/--unarrayify (start end)
  "Internal implementation: Convert comma-separated array to lines.
START and END define the region to operate on.
Removes quotes (both single and double) and splits by ', '.
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (mapconcat
   (lambda (x) (replace-regexp-in-string "[\"']" "" x))
   (split-string (buffer-substring start end) ", ") "\n"))

(defun cj/unarrayify (start end)
  "Convert quoted comma-separated strings between START and END to separate lines.
START and END identify the active region."
  (interactive "r")
  (let ((insertion (cj/--unarrayify start end)))
	(delete-region start end)
	(insert insertion)))

(defun cj/--toggle-quotes (start end)
  "Internal implementation: Toggle between double and single quotes.
START and END define the region to operate on.
Swaps all double quotes with single quotes and vice versa.
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((text (buffer-substring start end)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Use a placeholder to avoid double-swapping
      (while (search-forward "\"" nil t)
        (replace-match "\001" nil t))
      (goto-char (point-min))
      (while (search-forward "'" nil t)
        (replace-match "\"" nil t))
      (goto-char (point-min))
      (while (search-forward "\001" nil t)
        (replace-match "'" nil t))
      (buffer-string))))

(defun cj/toggle-quotes (start end)
  "Toggle between double and single quotes in region between START and END.
START and END identify the active region."
  (interactive "r")
  (let ((insertion (cj/--toggle-quotes start end)))
    (delete-region start end)
    (insert insertion)))

(defun cj/--reverse-lines (start end)
  "Internal implementation: Reverse the order of lines in region.
START and END define the region to operate on.
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((lines (split-string (buffer-substring start end) "\n")))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun cj/reverse-lines (start end)
  "Reverse the order of lines in region between START and END.
START and END identify the active region."
  (interactive "r")
  (let ((insertion (cj/--reverse-lines start end)))
    (delete-region start end)
    (insert insertion)))

(defun cj/--number-lines (start end format-string zero-pad)
  "Internal implementation: Number lines in region with custom format.
START and END define the region to operate on.
FORMAT-STRING is the format for each line, with N as placeholder for number.
  Example: \"N. \" produces \"1. \", \"2. \", etc.
  Example: \"[N] \" produces \"[1] \", \"[2] \", etc.
ZERO-PAD when non-nil pads numbers with zeros for alignment.
  Example with 100 lines: \"001\", \"002\", ..., \"100\".
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let* ((lines (split-string (buffer-substring start end) "\n"))
         (line-count (length lines))
         (width (if zero-pad (length (number-to-string line-count)) 1))
         (format-spec (if zero-pad (format "%%0%dd" width) "%d")))
    (mapconcat
     (lambda (pair)
       (let* ((num (car pair))
              (line (cdr pair))
              (num-str (format format-spec num)))
         (concat (replace-regexp-in-string "N" num-str format-string) line)))
     (cl-loop for line in lines
              for i from 1
              collect (cons i line))
     "\n")))

(defun cj/number-lines (start end format-string zero-pad)
  "Number lines in region between START and END with custom format.
START and END identify the active region.
FORMAT-STRING is the format for each line, with N as placeholder for number.
  Example: \"N. \" produces \"1. \", \"2. \", etc.
ZERO-PAD when non-nil (prefix argument) pads numbers with zeros."
  (interactive "r\nMFormat string (use N for number): \nP")
  (let ((insertion (cj/--number-lines start end format-string zero-pad)))
    (delete-region start end)
    (insert insertion)))

(defun cj/--alphabetize-region (start end)
  "Internal implementation: Alphabetize words in region.
START and END define the region to operate on.
Splits by whitespace and commas, sorts alphabetically, joins with ', '.
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((string (buffer-substring-no-properties start end)))
    (mapconcat #'identity
               (sort (split-string string "[[:space:],]+" t)
                     #'string-lessp)
               ", ")))

(defun cj/alphabetize-region ()
  "Alphabetize words in the active region and replace the original text.
Produce a comma-separated list as the result."
  (interactive)
  (unless (use-region-p)
	(user-error "No region selected"))
  (let ((start (region-beginning))
		(end (region-end))
		(insertion (cj/--alphabetize-region (region-beginning) (region-end))))
	(delete-region start end)
	(goto-char start)
	(insert insertion)))

(defun cj/--comma-separated-text-to-lines (start end)
  "Internal implementation: Convert comma-separated text to lines.
START and END define the region to operate on.
Replaces commas with newlines and removes trailing whitespace from each line.
Returns the transformed string without modifying the buffer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((text (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (search-forward "," nil t)
        (replace-match "\n" nil t))
      (delete-trailing-whitespace)
      (buffer-string))))

(defun cj/comma-separated-text-to-lines ()
  "Break up comma-separated text in active region so each item is on own line."
  (interactive)
  (if (not (region-active-p))
	  (error "No region selected"))

  (let ((beg (region-beginning))
		(end (region-end))
		(text (cj/--comma-separated-text-to-lines (region-beginning) (region-end))))
	(delete-region beg end)
	(goto-char beg)
	(insert text)))



;; Ordering & sorting prefix and keymap
(defvar-keymap cj/ordering-map
  :doc "Keymap for text ordering and sorting operations"
  "a" #'cj/arrayify
  "u" #'cj/unarrayify
  "l" #'cj/listify
  "j" #'cj/arrayify-json
  "p" #'cj/arrayify-python
  "q" #'cj/toggle-quotes
  "r" #'cj/reverse-lines
  "n" #'cj/number-lines
  "A" #'cj/alphabetize-region
  "L" #'cj/comma-separated-text-to-lines)

(keymap-set cj/custom-keymap "o" cj/ordering-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; o" "ordering/sorting menu"))

(provide 'custom-ordering)
;;; custom-ordering.el ends here.
