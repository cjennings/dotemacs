;;; cj-org-text-lib.el --- Pure helpers for sanitizing external text into Org -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Pure string helpers for safely composing Org-mode content from
;; external text (calendar event bodies, web-clipped HTML, mail
;; subject lines, AI conversation transcripts, etc.).
;;
;; The shared concern is that text from outside sources can contain
;; characters that disturb Org structure if pasted verbatim:
;;
;; - leading `*' creates an unintended heading,
;; - newlines inside a property value spawn extra drawer lines,
;; - newlines inside a heading split it into two outline entries.
;;
;; These helpers neutralize each pattern with predictable, testable
;; replacements.  They are pure (string in, string out, nil-safe) and
;; have no Org-mode dependency, so they remain useful in batch and in
;; tests without loading Org.

;;; Code:

(defun cj/org-sanitize-body-text (text)
  "Sanitize TEXT for safe inclusion as Org body content.
Replaces leading asterisks with dashes so external lines aren't
parsed as Org headings.  Handles multiple levels (`**' becomes `--').
Returns nil for nil input."
  (when text
    (replace-regexp-in-string
     "^\\(\\*+\\) "
     (lambda (match)
       (concat (make-string (length (match-string 1 match)) ?-) " "))
     text)))

(defun cj/org-sanitize-property-value (text)
  "Sanitize TEXT for safe inclusion as a single Org property value.
Collapses whitespace and newlines into single spaces and trims, so the
result fits on one line of an Org property drawer.  Returns nil for
nil input."
  (when text
    (string-trim
     (replace-regexp-in-string
      "[[:space:]\n\r]+"
      " "
      text))))

(defun cj/org-sanitize-heading (text)
  "Sanitize TEXT for safe inclusion as a single Org heading title.
Composes `cj/org-sanitize-body-text' (neutralizes leading stars) and
`cj/org-sanitize-property-value' (flattens to a single line).  Returns
nil for nil input."
  (cj/org-sanitize-property-value
   (cj/org-sanitize-body-text text)))

(provide 'cj-org-text-lib)
;;; cj-org-text-lib.el ends here
