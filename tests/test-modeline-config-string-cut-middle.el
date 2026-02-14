;;; test-modeline-config-string-cut-middle.el --- Tests for cj/modeline-string-cut-middle -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/modeline-string-cut-middle function from modeline-config.el
;;
;; This function truncates a string in the middle with "..." when conditions
;; are met (narrow window, multi-window, string exceeds truncation length).
;; Example: "my-very-long-name.el" → "my-ver...me.el"
;;
;; We mock cj/modeline-string-truncate-p to isolate the string manipulation
;; logic from window state checks.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(unless (boundp 'cj/buffer-status-colors)
  (defvar cj/buffer-status-colors
    '((unmodified . "#FFFFFF")
      (modified   . "#00FF00")
      (read-only  . "#FF0000")
      (overwrite  . "#FFD700"))))

(require 'modeline-config)

;;; Test Helpers

(defmacro with-truncation-enabled (&rest body)
  "Execute BODY with truncation forced on.
Mocks `cj/modeline-string-truncate-p' to always return t."
  `(cl-letf (((symbol-function 'cj/modeline-string-truncate-p)
              (lambda (_str) t)))
     ,@body))

(defmacro with-truncation-disabled (&rest body)
  "Execute BODY with truncation forced off.
Mocks `cj/modeline-string-truncate-p' to always return nil."
  `(cl-letf (((symbol-function 'cj/modeline-string-truncate-p)
              (lambda (_str) nil)))
     ,@body))

;;; Normal Cases

(ert-deftest test-modeline-config-string-cut-middle-normal-truncates-long-string ()
  "Should truncate a long string in the middle with '...'."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 12)
           (result (cj/modeline-string-cut-middle "my-very-long-name.el")))
      (should (string-match-p "\\.\\.\\." result))
      (should (< (length result) (length "my-very-long-name.el"))))))

(ert-deftest test-modeline-config-string-cut-middle-normal-preserves-start-and-end ()
  "Should keep the beginning and end of the string."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 12)
           (result (cj/modeline-string-cut-middle "my-very-long-name.el")))
      (should (string-prefix-p "my-ver" result))
      (should (string-suffix-p "me.el" result)))))

(ert-deftest test-modeline-config-string-cut-middle-normal-result-length ()
  "Truncated result should be truncate-length + 3 (for '...')."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 12)
           (result (cj/modeline-string-cut-middle "abcdefghijklmnopqrstuvwxyz")))
      ;; half=6, so 6 chars + "..." + 6 chars = 15
      (should (= (length result) 15)))))

(ert-deftest test-modeline-config-string-cut-middle-normal-no-truncation-when-disabled ()
  "Should return string unchanged when truncation is disabled."
  (with-truncation-disabled
    (let ((result (cj/modeline-string-cut-middle "my-very-long-name.el")))
      (should (string= result "my-very-long-name.el")))))

(ert-deftest test-modeline-config-string-cut-middle-normal-short-string-unchanged ()
  "Short string should pass through unchanged when truncation is disabled."
  (with-truncation-disabled
    (let ((result (cj/modeline-string-cut-middle "init.el")))
      (should (string= result "init.el")))))

;;; Boundary Cases

(ert-deftest test-modeline-config-string-cut-middle-boundary-empty-string ()
  "Empty string should pass through unchanged."
  (with-truncation-disabled
    (let ((result (cj/modeline-string-cut-middle "")))
      (should (string= result "")))))

(ert-deftest test-modeline-config-string-cut-middle-boundary-single-char ()
  "Single character string should pass through unchanged."
  (with-truncation-disabled
    (let ((result (cj/modeline-string-cut-middle "x")))
      (should (string= result "x")))))

(ert-deftest test-modeline-config-string-cut-middle-boundary-odd-truncate-length ()
  "Odd truncation length should floor the half correctly."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 11)
           (result (cj/modeline-string-cut-middle "abcdefghijklmnopqrstuvwxyz")))
      ;; half = (floor 11 2) = 5, so 5 + "..." + 5 = 13
      (should (= (length result) 13))
      (should (string= (substring result 0 5) "abcde"))
      (should (string= (substring result -5) "vwxyz")))))

(ert-deftest test-modeline-config-string-cut-middle-boundary-truncate-length-2 ()
  "Minimum practical truncation length of 2."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 2)
           (result (cj/modeline-string-cut-middle "abcdefghij")))
      ;; half = (floor 2 2) = 1, so 1 + "..." + 1 = 5
      (should (= (length result) 5))
      (should (string= result "a...j")))))

(ert-deftest test-modeline-config-string-cut-middle-boundary-unicode-filename ()
  "Should handle unicode characters in filename."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 10)
           (result (cj/modeline-string-cut-middle "überlangerDateiname.el")))
      (should (string-match-p "\\.\\.\\." result))
      (should (< (length result) (length "überlangerDateiname.el"))))))

(ert-deftest test-modeline-config-string-cut-middle-boundary-dots-in-filename ()
  "Should handle filenames with dots (common in elisp)."
  (with-truncation-enabled
    (let* ((cj/modeline-string-truncate-length 12)
           (result (cj/modeline-string-cut-middle "test-custom-whitespace-collapse.el")))
      (should (string-match-p "\\.\\.\\." result)))))

;;; Error Cases

(ert-deftest test-modeline-config-string-cut-middle-error-nil-input ()
  "Nil input should pass through (truncate-p returns nil for non-strings)."
  (with-truncation-disabled
    (should (null (cj/modeline-string-cut-middle nil)))))

(ert-deftest test-modeline-config-string-cut-middle-error-number-input ()
  "Number input should pass through (truncate-p returns nil for non-strings)."
  (with-truncation-disabled
    (should (= (cj/modeline-string-cut-middle 42) 42))))

(provide 'test-modeline-config-string-cut-middle)
;;; test-modeline-config-string-cut-middle.el ends here
