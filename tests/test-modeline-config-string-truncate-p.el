;;; test-modeline-config-string-truncate-p.el --- Tests for cj/modeline-string-truncate-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/modeline-string-truncate-p function from modeline-config.el
;;
;; This function returns non-nil when ALL conditions are met:
;; 1. STR is a string
;; 2. STR is not empty
;; 3. Window is narrow (< 100 chars via cj/modeline-window-narrow-p)
;; 4. String length exceeds cj/modeline-string-truncate-length
;; 5. Not in a single-window frame (one-window-p returns nil)
;;
;; We mock window functions to isolate the logic.

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

(defmacro with-window-state (narrow-p one-window-p &rest body)
  "Execute BODY with mocked window state.
NARROW-P controls `cj/modeline-window-narrow-p' return value.
ONE-WINDOW-P controls `one-window-p' return value."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'cj/modeline-window-narrow-p)
              (lambda () ,narrow-p))
             ((symbol-function 'one-window-p)
              (lambda (&rest _args) ,one-window-p)))
     ,@body))

;;; Normal Cases - All conditions met (should return non-nil)

(ert-deftest test-modeline-config-string-truncate-p-normal-long-string-narrow-multi-window ()
  "Should return non-nil when string is long, window is narrow, and multiple windows."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should (cj/modeline-string-truncate-p "a-very-long-filename.el")))))

(ert-deftest test-modeline-config-string-truncate-p-normal-exactly-over-length ()
  "Should return non-nil when string is one char over truncation length."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should (cj/modeline-string-truncate-p "1234567890123")))))  ; 13 chars

;;; Normal Cases - Conditions not met (should return nil)

(ert-deftest test-modeline-config-string-truncate-p-normal-short-string ()
  "Should return nil when string is shorter than truncation length."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p "init.el")))))

(ert-deftest test-modeline-config-string-truncate-p-normal-wide-window ()
  "Should return nil when window is wide (not narrow)."
  (with-window-state nil nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p "a-very-long-filename.el")))))

(ert-deftest test-modeline-config-string-truncate-p-normal-single-window ()
  "Should return nil when only one window is open."
  (with-window-state t t
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p "a-very-long-filename.el")))))

;;; Boundary Cases

(ert-deftest test-modeline-config-string-truncate-p-boundary-exact-length ()
  "Should return nil when string is exactly at truncation length."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p "123456789012")))))  ; exactly 12

(ert-deftest test-modeline-config-string-truncate-p-boundary-empty-string ()
  "Should return nil for empty string."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p "")))))

(ert-deftest test-modeline-config-string-truncate-p-boundary-single-char ()
  "Should return nil for single character string."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p "x")))))

(ert-deftest test-modeline-config-string-truncate-p-boundary-truncate-length-1 ()
  "Should return non-nil for any string > 1 when truncation length is 1."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 1))
      (should (cj/modeline-string-truncate-p "ab")))))

(ert-deftest test-modeline-config-string-truncate-p-boundary-truncate-length-0 ()
  "Should return non-nil for any non-empty string when truncation length is 0."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 0))
      (should (cj/modeline-string-truncate-p "a")))))

;;; Error Cases

(ert-deftest test-modeline-config-string-truncate-p-error-nil-input ()
  "Should return nil for nil input."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p nil)))))

(ert-deftest test-modeline-config-string-truncate-p-error-number-input ()
  "Should return nil for number input."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p 42)))))

(ert-deftest test-modeline-config-string-truncate-p-error-symbol-input ()
  "Should return nil for symbol input."
  (with-window-state t nil
    (let ((cj/modeline-string-truncate-length 12))
      (should-not (cj/modeline-string-truncate-p 'some-symbol)))))

(provide 'test-modeline-config-string-truncate-p)
;;; test-modeline-config-string-truncate-p.el ends here
