;;; custom-ordering.el --- Region sorting and list-format transforms -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/C.
;; Load shape: eager.
;; Eager reason: registers its C-; o ordering submap at load. Currently eager by
;;   init order; a deferral candidate for Phase 3/4 (command/autoload +
;;   registration API).
;; Top-level side effects: defines cj/ordering-map, registers it under C-; o.
;; Runtime requires: cl-lib, keybindings (org-config on demand via
;;   declare-function).
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Region transforms under C-; o for sorting, reversing, numbering, quote
;; toggling, and converting between line lists and comma-separated arrays.
;; Helpers preserve trailing newlines where line-oriented callers expect them.
;;
;;; Code:

(require 'cl-lib)
(require 'keybindings)  ;; provides cj/custom-keymap

;; Bound in the ordering keymap below but owned by org-config (loaded eagerly
;; in init); declare it so byte-compiling this module standalone is clean.
(declare-function cj/org-sort-by-todo-and-priority "org-config")

(defvar cj/ordering-map)

(defun cj/--ordering-validate-region (start end)
  "Signal an error when START is greater than END.
Shared guard for the pure ordering helpers below, which all operate on a
buffer region and must reject an inverted one before reading it."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end)))

(defun cj/--ordering-replace-region (start end insertion)
  "Replace the buffer text between START and END with INSERTION.
Point is left after the inserted text.  Shared tail for the interactive
ordering commands, which all compute a transformed string from the
original region then swap it in.  INSERTION is evaluated by the caller
before this runs, so the transform reads the pre-deletion text."
  (delete-region start end)
  (goto-char start)
  (insert insertion))

(defun cj/--arrayify (start end quote &optional prefix suffix)
  "Internal implementation: Convert lines to quoted, comma-separated format.
START and END define the region to operate on.
QUOTE specifies the quotation characters to surround each element.
  Use \"\" for no quotes, \"\\\"\" for double quotes, \"'\" for single quotes.
PREFIX is an optional string to prepend to the result (e.g., \"[\" or \"(\").
SUFFIX is an optional string to append to the result (e.g., \"]\" or \")\").
Preserves a trailing newline if the input region ends with one, so
line-oriented operations on the result behave the same as before.
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
  (let* ((raw (buffer-substring start end))
         (trailing-newline (string-suffix-p "\n" raw))
         (result (mapconcat
                  (lambda (x) (format "%s%s%s" quote x quote))
                  (split-string raw) ", ")))
    (concat (or prefix "") result (or suffix "")
            (if trailing-newline "\n" ""))))

(defun cj/arrayify (start end quote)
  "Convert lines between START and END into quoted, comma-separated strings.
START and END identify the active region.
QUOTE specifies the quotation characters to surround each element."
  (interactive "r\nMQuotation character to use for array element: ")
  (cj/--ordering-replace-region start end (cj/--arrayify start end quote)))

(defun cj/listify (start end)
  "Convert lines between START and END into an unquoted, comma-separated list.
START and END identify the active region.
Example: `apple banana cherry' becomes `apple, banana, cherry'."
  (interactive "r")
  (cj/--ordering-replace-region start end (cj/--arrayify start end "")))

(defun cj/arrayify-json (start end)
  "Convert lines between START and END into a JSON-style array.
START and END identify the active region.
Example: `apple banana cherry' becomes `[\"apple\", \"banana\", \"cherry\"]'."
  (interactive "r")
  (cj/--ordering-replace-region start end (cj/--arrayify start end "\"" "[" "]")))

;; JSON arrays and Python lists coincide here (double-quoted, square-bracketed),
;; so the Python command is an alias.  Split it back into its own defun if the
;; two formats ever need to differ (e.g. Python single quotes).
(defalias 'cj/arrayify-python 'cj/arrayify-json
  "Convert lines in the active region into a Python-style list.
Example: `apple banana cherry' becomes `[\"apple\", \"banana\", \"cherry\"]'.
Currently identical to `cj/arrayify-json'.")

(defun cj/--unarrayify (start end)
  "Internal implementation: Convert comma-separated array to lines.
START and END define the region to operate on.
Removes quotes (both single and double) and splits by ', '.
Preserves a trailing newline if the input region ends with one.
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
  (let* ((raw (buffer-substring start end))
         (trailing-newline (string-suffix-p "\n" raw))
         (result (mapconcat
                  (lambda (x) (replace-regexp-in-string "[\"']" "" x))
                  (split-string raw ", ") "\n")))
    (concat result (if trailing-newline "\n" ""))))

(defun cj/unarrayify (start end)
  "Convert quoted comma-separated strings between START and END to separate lines.
START and END identify the active region."
  (interactive "r")
  (cj/--ordering-replace-region start end (cj/--unarrayify start end)))

(defun cj/--toggle-quotes (start end)
  "Internal implementation: Toggle between double and single quotes.
START and END define the region to operate on.
Swaps all double quotes with single quotes and vice versa.
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
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
  (cj/--ordering-replace-region start end (cj/--toggle-quotes start end)))

(defun cj/--reverse-lines (start end)
  "Internal implementation: Reverse the order of lines in region.
START and END define the region to operate on.
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
  (let ((lines (split-string (buffer-substring start end) "\n")))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun cj/reverse-lines (start end)
  "Reverse the order of lines in region between START and END.
START and END identify the active region."
  (interactive "r")
  (cj/--ordering-replace-region start end (cj/--reverse-lines start end)))

(defun cj/--number-lines (start end format-string zero-pad)
  "Internal implementation: Number lines in region with custom format.
START and END define the region to operate on.
FORMAT-STRING is the format for each line, with N as placeholder for number.
  Example: \"N. \" produces \"1. \", \"2. \", etc.
  Example: \"[N] \" produces \"[1] \", \"[2] \", etc.
ZERO-PAD when non-nil pads numbers with zeros for alignment.
  Example with 100 lines: \"001\", \"002\", ..., \"100\".
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
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
  (cj/--ordering-replace-region
   start end (cj/--number-lines start end format-string zero-pad)))

(defun cj/--alphabetize-region (start end)
  "Internal implementation: Alphabetize words in region.
START and END define the region to operate on.
Splits by whitespace and commas, sorts alphabetically, joins with ', '.
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
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
        (end (region-end)))
    (cj/--ordering-replace-region start end (cj/--alphabetize-region start end))))

(defun cj/--comma-separated-text-to-lines (start end)
  "Internal implementation: Convert comma-separated text to lines.
START and END define the region to operate on.
Replaces commas with newlines and removes trailing whitespace from each line.
Returns the transformed string without modifying the buffer."
  (cj/--ordering-validate-region start end)
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
        (end (region-end)))
    (cj/--ordering-replace-region
     beg end (cj/--comma-separated-text-to-lines beg end))))



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
  "L" #'cj/comma-separated-text-to-lines
  "o" #'cj/org-sort-by-todo-and-priority)

(cj/register-prefix-map "o" cj/ordering-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; o" "ordering/sorting menu"
    "C-; o l" "listify"
    "C-; o j" "JSON array"
    "C-; o p" "Python list"
    "C-; o q" "toggle quotes"
    "C-; o r" "reverse lines"
    "C-; o n" "number lines"
    "C-; o A" "alphabetize"
    "C-; o L" "comma to lines"
    "C-; o o" "org: sort by TODO+priority"))

(provide 'custom-ordering)
;;; custom-ordering.el ends here.
