;;; ai-rewrite.el --- Directive-picker wrappers for gptel-rewrite -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Adds two ergonomic wrappers around `gptel-rewrite':
;;
;;   cj/gptel-rewrite-with-directive       Pick a named directive,
;;                                         then rewrite the region.
;;   cj/gptel-rewrite-redo-with-different-directive
;;                                         Re-run the previous region
;;                                         with a different directive.
;;
;; A directive is a short system-message snippet attached to a name
;; (e.g. "terse", "fix-grammar").  The directive body is injected
;; into the rewrite via `gptel-rewrite-directives-hook' just for that
;; call -- no global state changes.

;;; Code:

;; Declare the hook variable special so our `let'-binding below is
;; dynamic (visible across the `call-interactively' that follows)
;; rather than lexical when this file is byte-compiled.
(defvar gptel-rewrite-directives-hook)

(defcustom cj/gptel-rewrite-directives
  '(("terse"
     . "Rewrite the text to be as terse as possible without losing meaning.\nDo not add commentary.  Return only the rewritten text.")
    ("fix-grammar"
     . "Fix grammar and spelling errors only.  Do not rephrase, restructure,\nor change tone.  Return only the corrected text.")
    ("refactor-readability"
     . "Refactor the code for readability.  Improve naming, split long\nfunctions when appropriate, remove unnecessary complexity, and preserve\nbehavior exactly.  Return only the refactored code.")
    ("add-docstring"
     . "Add or improve docstrings for every function in the region.  Use the\nidiomatic docstring style for the language.  Do not change executable\ncode.  Return the whole region with the updated docstrings.")
    ("explain-as-comment"
     . "Replace the region with the original code, preceded by a concise\nblock comment explaining what the code does.  Use the language's\nidiomatic comment syntax.  Return code + comment, nothing else.")
    ("shorten"
     . "Shorten the text while preserving meaning, technical accuracy, and\nthe author's voice.  Remove rhetorical padding.  Return only the\nshortened text."))
  "Named system-message directives for `cj/gptel-rewrite-with-directive'.
Each entry is a (NAME . BODY) pair where NAME is the directive label
presented in the completing-read prompt and BODY is the system
message injected into the next `gptel-rewrite' call."
  :type '(alist :key-type string :value-type string)
  :group 'cj)

(defvar-local cj/gptel-rewrite--last-region nil
  "Cons (BEG-MARKER . END-MARKER) of the last directive-driven rewrite.")

(defvar-local cj/gptel-rewrite--last-directive nil
  "Name of the directive used in the last directive-driven rewrite.")

(defun cj/gptel-rewrite--call-with-directive (directive-name beg end)
  "Run `gptel-rewrite' over BEG..END with DIRECTIVE-NAME's system message.
Stores the region (as markers) and directive name on buffer-local
variables so `cj/gptel-rewrite-redo-with-different-directive' can
revisit them."
  (let ((body (alist-get directive-name cj/gptel-rewrite-directives
                         nil nil #'equal)))
    (unless body
      (user-error "Unknown rewrite directive: %s" directive-name))
    (setq-local cj/gptel-rewrite--last-region
                (cons (copy-marker beg) (copy-marker end)))
    (setq-local cj/gptel-rewrite--last-directive directive-name)
    (let ((gptel-rewrite-directives-hook
           (cons (lambda () body) gptel-rewrite-directives-hook)))
      (save-excursion
        (goto-char beg)
        (push-mark end t t)
        (call-interactively #'gptel-rewrite)))))

;;;###autoload
(defun cj/gptel-rewrite-with-directive (directive-name)
  "Pick DIRECTIVE-NAME from `cj/gptel-rewrite-directives' and rewrite the region.
Requires an active region.  The directive is applied only to this
call -- it does not modify global `gptel-directives'."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "No region selected"))
     (list (completing-read
            "Rewrite directive: "
            (mapcar #'car cj/gptel-rewrite-directives) nil t))))
  (cj/gptel-rewrite--call-with-directive
   directive-name (region-beginning) (region-end)))

;;;###autoload
(defun cj/gptel-rewrite-redo-with-different-directive ()
  "Re-run the previous directive-driven rewrite with a different directive.
The region is restored from the markers captured at the last call;
the user picks a new directive from the remaining choices."
  (interactive)
  (unless cj/gptel-rewrite--last-region
    (user-error "No previous rewrite to redo in this buffer"))
  (let* ((beg-mk (car cj/gptel-rewrite--last-region))
         (end-mk (cdr cj/gptel-rewrite--last-region))
         (current cj/gptel-rewrite--last-directive)
         (others (cl-remove
                  current
                  (mapcar #'car cj/gptel-rewrite-directives)
                  :test #'equal))
         (chosen (completing-read
                  (format "Re-rewrite with (was %s): " current)
                  others nil t)))
    (cj/gptel-rewrite--call-with-directive
     chosen (marker-position beg-mk) (marker-position end-mk))))

(provide 'ai-rewrite)
;;; ai-rewrite.el ends here
