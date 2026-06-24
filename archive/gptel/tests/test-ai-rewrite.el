;;; test-ai-rewrite.el --- Tests for ai-rewrite.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the directive-picker wrappers around `gptel-rewrite'.
;; `gptel-rewrite' itself is stubbed so the tests verify what the
;; wrappers do (which directive body lands in the hook, which region
;; was captured) without touching the real rewrite UI.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'testutil-ai-config)

;; Stub the gptel-rewrite surface so the wrapper can dispatch to it
;; without loading the real package.  testutil-ai-config provides a
;; non-interactive stub of `gptel-rewrite'; we override it with an
;; interactive recorder that captures the hook-derived directive body
;; and the active region.
(defvar gptel-rewrite-directives-hook nil)
(defvar test-ai-rewrite--captured-directive nil
  "Last system-message body produced by the hook during a stub rewrite.")
(defvar test-ai-rewrite--captured-region nil
  "Cons (BEG . END) captured from `mark' and `point' at stub-rewrite time.")
(defun gptel-rewrite ()
  "Stub: capture the directive body and the active region."
  (interactive)
  (setq test-ai-rewrite--captured-directive
        (run-hook-with-args-until-success 'gptel-rewrite-directives-hook))
  (setq test-ai-rewrite--captured-region
        (cons (region-beginning) (region-end))))

(require 'ai-rewrite)

;; ---------------------------- defcustom shape

(ert-deftest test-ai-rewrite-directives-defcustom-has-named-entries ()
  "Default directives include the names called out in the spec."
  (let ((names (mapcar #'car cj/gptel-rewrite-directives)))
    (dolist (expected '("terse" "fix-grammar" "refactor-readability"
                        "add-docstring" "explain-as-comment" "shorten"))
      (should (member expected names)))))

(ert-deftest test-ai-rewrite-directives-bodies-are-strings ()
  "Every directive body is a non-empty string."
  (dolist (entry cj/gptel-rewrite-directives)
    (should (stringp (cdr entry)))
    (should (> (length (cdr entry)) 0))))

;; ---------------------------- with-directive

(ert-deftest test-ai-rewrite-with-directive-normal ()
  "Wrapper injects the directive body and runs gptel-rewrite on the region."
  (with-temp-buffer
    (insert "first body line\nsecond body line\n")
    (let ((test-ai-rewrite--captured-directive nil)
          (test-ai-rewrite--captured-region nil)
          (cj/gptel-rewrite-directives
           '(("test" . "BODY FOR TEST DIRECTIVE"))))
      ;; Activate the region across both lines
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (cj/gptel-rewrite-with-directive "test")
      (should (equal test-ai-rewrite--captured-directive
                     "BODY FOR TEST DIRECTIVE"))
      (should test-ai-rewrite--captured-region))))

(ert-deftest test-ai-rewrite-with-directive-error-no-region ()
  "No active region signals."
  (with-temp-buffer
    (insert "no region")
    (deactivate-mark)
    (should-error (call-interactively #'cj/gptel-rewrite-with-directive))))

(ert-deftest test-ai-rewrite-with-directive-error-unknown-directive ()
  "Unknown directive name signals."
  (with-temp-buffer
    (insert "body")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (let ((cj/gptel-rewrite-directives '(("known" . "x"))))
      (should-error
       (cj/gptel-rewrite--call-with-directive
        "unknown" (point-min) (point-max))))))

(ert-deftest test-ai-rewrite-with-directive-records-last-state ()
  "Wrapper records the region and directive name for later redo."
  (with-temp-buffer
    (insert "abc\ndef\n")
    (let ((cj/gptel-rewrite-directives
           '(("first" . "FIRST BODY")))
          (test-ai-rewrite--captured-directive nil))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (cj/gptel-rewrite-with-directive "first")
      (should (equal cj/gptel-rewrite--last-directive "first"))
      (should (consp cj/gptel-rewrite--last-region))
      (should (markerp (car cj/gptel-rewrite--last-region)))
      (should (markerp (cdr cj/gptel-rewrite--last-region))))))

;; ---------------------------- redo

(ert-deftest test-ai-rewrite-redo-normal ()
  "Redo replays the last region with a new directive."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let* ((cj/gptel-rewrite-directives
            '(("first" . "FIRST BODY")
              ("second" . "SECOND BODY")))
           (test-ai-rewrite--captured-directive nil)
           (test-ai-rewrite--captured-region nil))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (cj/gptel-rewrite-with-directive "first")
      (should (equal test-ai-rewrite--captured-directive "FIRST BODY"))
      (let ((first-region test-ai-rewrite--captured-region))
        (setq test-ai-rewrite--captured-directive nil)
        (setq test-ai-rewrite--captured-region nil)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p choices &rest _) (car choices))))
          (cj/gptel-rewrite-redo-with-different-directive))
        (should (equal test-ai-rewrite--captured-directive "SECOND BODY"))
        (should (equal test-ai-rewrite--captured-region first-region))))))

(ert-deftest test-ai-rewrite-redo-error-no-previous ()
  "Redo without prior rewrite signals."
  (with-temp-buffer
    (setq-local cj/gptel-rewrite--last-region nil)
    (should-error (cj/gptel-rewrite-redo-with-different-directive))))

(ert-deftest test-ai-rewrite-redo-excludes-current-directive ()
  "Redo's completing-read prompt offers every directive except the last."
  (with-temp-buffer
    (insert "body")
    (let ((cj/gptel-rewrite-directives
           '(("a" . "A") ("b" . "B") ("c" . "C")))
          (offered nil))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (cj/gptel-rewrite-with-directive "b")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p choices &rest _)
                   (setq offered choices)
                   (car choices))))
        (cj/gptel-rewrite-redo-with-different-directive))
      (should (equal (sort (copy-sequence offered) #'string<)
                     '("a" "c"))))))

(provide 'test-ai-rewrite)
;;; test-ai-rewrite.el ends here
