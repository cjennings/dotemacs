;;; test-custom-misc-jump-to-matching-paren.el --- Tests for cj/jump-to-matching-paren -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/jump-to-matching-paren function from custom-misc.el
;;
;; This function jumps to matching delimiters using Emacs's sexp navigation.
;; It works with any delimiter that has matching syntax according to the
;; current syntax table (parentheses, brackets, braces, etc.).
;;
;; Unlike other functions in this test suite, this is an INTERACTIVE function
;; that moves point and displays messages. We test it as an integration test
;; by setting up buffers, positioning point, calling the function, and
;; verifying where point ends up.
;;
;; Key behaviors:
;; - When on opening delimiter: jump forward to matching closing delimiter
;; - When on closing delimiter: jump backward to matching opening delimiter
;; - When just after closing delimiter: jump backward to matching opening
;; - When not on delimiter: display message, don't move
;; - When no matching delimiter: display error message, don't move

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-misc)

;;; Test Helpers

(defun test-jump-to-matching-paren (text point-position)
  "Test cj/jump-to-matching-paren with TEXT and point at POINT-POSITION.
Returns the new point position after calling the function.
POINT-POSITION is 1-indexed (1 = first character)."
  (with-temp-buffer
    (emacs-lisp-mode)  ; Use elisp mode for proper syntax table
    (insert text)
    (goto-char point-position)
    (cj/jump-to-matching-paren)
    (point)))

;;; Normal Cases - Forward Jump (Opening to Closing)

(ert-deftest test-jump-paren-forward-simple ()
  "Should jump forward from opening paren to closing paren."
  ;; Text: "(hello)"
  ;; Start at position 1 (on opening paren)
  ;; Should end at position 8 (after closing paren)
  (should (= 8 (test-jump-to-matching-paren "(hello)" 1))))

(ert-deftest test-jump-paren-forward-nested ()
  "Should jump forward over nested parens."
  ;; Text: "(foo (bar))"
  ;; Start at position 1 (on outer opening paren)
  ;; Should end at position 12 (after outer closing paren)
  (should (= 12 (test-jump-to-matching-paren "(foo (bar))" 1))))

(ert-deftest test-jump-paren-forward-inner-nested ()
  "Should jump forward from inner opening paren."
  ;; Text: "(foo (bar))"
  ;; Start at position 6 (on inner opening paren)
  ;; Should end at position 11 (after inner closing paren)
  (should (= 11 (test-jump-to-matching-paren "(foo (bar))" 6))))

(ert-deftest test-jump-bracket-forward ()
  "Should jump forward from opening bracket."
  ;; Text: "[1 2 3]"
  ;; Start at position 1
  ;; Should end at position 8
  (should (= 8 (test-jump-to-matching-paren "[1 2 3]" 1))))

;; Note: Braces are not treated as matching delimiters in emacs-lisp-mode
;; so we don't test them here

;;; Normal Cases - Backward Jump (Closing to Opening)

(ert-deftest test-jump-paren-backward-simple ()
  "Should jump backward from closing paren to opening paren."
  ;; Text: "(hello)"
  ;; Start at position 7 (on closing paren)
  ;; Should end at position 2 (after opening paren)
  (should (= 2 (test-jump-to-matching-paren "(hello)" 7))))

(ert-deftest test-jump-paren-backward-nested ()
  "Should jump backward over nested parens from after outer closing."
  ;; Text: "(foo (bar))"
  ;; Start at position 12 (after outer closing paren)
  ;; backward-sexp goes back to before opening paren
  (should (= 1 (test-jump-to-matching-paren "(foo (bar))" 12))))

(ert-deftest test-jump-paren-backward-inner-nested ()
  "Should jump backward from inner closing paren."
  ;; Text: "(foo (bar))"
  ;; Start at position 10 (on inner closing paren)
  ;; Should end at position 7 (after inner opening paren)
  (should (= 7 (test-jump-to-matching-paren "(foo (bar))" 10))))

(ert-deftest test-jump-bracket-backward ()
  "Should jump backward from after closing bracket."
  ;; Text: "[1 2 3]"
  ;; Start at position 8 (after ])
  ;; backward-sexp goes back one sexp
  (should (= 1 (test-jump-to-matching-paren "[1 2 3]" 8))))

;;; Normal Cases - Jump from After Closing Delimiter

(ert-deftest test-jump-paren-after-closing ()
  "Should jump backward when just after closing paren."
  ;; Text: "(hello)"
  ;; Start at position 8 (after closing paren)
  ;; backward-sexp goes back one sexp, ending before the opening paren
  (should (= 1 (test-jump-to-matching-paren "(hello)" 8))))

;;; Boundary Cases - No Movement

(ert-deftest test-jump-paren-not-on-delimiter ()
  "Should not move when not on delimiter."
  ;; Text: "(hello world)"
  ;; Start at position 3 (on 'e' in hello)
  ;; Should stay at position 3
  (should (= 3 (test-jump-to-matching-paren "(hello world)" 3))))

(ert-deftest test-jump-paren-on-whitespace ()
  "Should not move when on whitespace."
  ;; Text: "(hello world)"
  ;; Start at position 7 (on space)
  ;; Should stay at position 7
  (should (= 7 (test-jump-to-matching-paren "(hello world)" 7))))

;;; Boundary Cases - Unmatched Delimiters

(ert-deftest test-jump-paren-unmatched-opening ()
  "Should not move from unmatched opening paren."
  ;; Text: "(hello"
  ;; Start at position 1 (on opening paren with no closing)
  ;; Should stay at position 1
  (should (= 1 (test-jump-to-matching-paren "(hello" 1))))

(ert-deftest test-jump-paren-unmatched-closing ()
  "Should move to beginning from unmatched closing paren."
  ;; Text: "hello)"
  ;; Start at position 6 (on closing paren with no opening)
  ;; backward-sexp with unmatched closing paren goes to beginning
  (should (= 1 (test-jump-to-matching-paren "hello)" 6))))

;;; Boundary Cases - Empty Delimiters

(ert-deftest test-jump-paren-empty ()
  "Should jump over empty parens."
  ;; Text: "()"
  ;; Start at position 1
  ;; Should end at position 3
  (should (= 3 (test-jump-to-matching-paren "()" 1))))

(ert-deftest test-jump-paren-empty-backward ()
  "Should stay put when on closing paren of empty parens."
  ;; Text: "()"
  ;; Start at position 2 (on closing paren)
  ;; backward-sexp from closing of empty parens gives an error, so stays at 2
  (should (= 2 (test-jump-to-matching-paren "()" 2))))

;;; Boundary Cases - Multiple Delimiter Types

(ert-deftest test-jump-paren-mixed-delimiters ()
  "Should jump over mixed delimiter types."
  ;; Text: "(foo [bar {baz}])"
  ;; Start at position 1 (on opening paren)
  ;; Should end at position 18 (after closing paren)
  (should (= 18 (test-jump-to-matching-paren "(foo [bar {baz}])" 1))))

(ert-deftest test-jump-bracket-in-parens ()
  "Should jump from bracket inside parens."
  ;; Text: "(foo [bar])"
  ;; Start at position 6 (on opening bracket)
  ;; Should end at position 11 (after closing bracket)
  (should (= 11 (test-jump-to-matching-paren "(foo [bar])" 6))))

;;; Complex Cases - Strings and Comments

(ert-deftest test-jump-paren-over-string ()
  "Should jump over parens containing strings."
  ;; Text: "(\"hello (world)\")"
  ;; Start at position 1 (on opening paren)
  ;; Should end at position 18 (after closing paren)
  ;; The parens in the string should be ignored
  (should (= 18 (test-jump-to-matching-paren "(\"hello (world)\")" 1))))

(provide 'test-custom-misc-jump-to-matching-paren)
;;; test-custom-misc-jump-to-matching-paren.el ends here
