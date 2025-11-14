;;; test-custom-comments-delete-buffer-comments.el --- Tests for cj/delete-buffer-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/delete-buffer-comments function from custom-comments.el
;;
;; This function deletes all comments in the current buffer by delegating to
;; Emacs' built-in `comment-kill` function.
;;
;; Cross-Language Testing Strategy:
;; --------------------------------
;; This function works across multiple programming languages/major modes because
;; it delegates to `comment-kill`, which respects each mode's comment syntax
;; (comment-start, comment-end).
;;
;; Rather than testing exhaustively in every language (8+ languages = 100+ tests),
;; we test strategically:
;;
;; 1. EXTENSIVE testing in Emacs Lisp (our primary language):
;;    - ~15 tests covering all normal/boundary/error cases
;;    - Tests edge cases: empty buffers, inline comments, unicode, etc.
;;
;; 2. REPRESENTATIVE testing in Python and C:
;;    - ~3 tests each proving different comment syntaxes work
;;    - Python: hash-based comments (#)
;;    - C: C-style line (//) and block (/* */) comments
;;
;; Why this approach?
;; - OUR code is simple: (goto-char (point-min)) + (comment-kill ...)
;; - We're testing OUR integration logic, not Emacs' comment-kill implementation
;; - After proving 3 different syntaxes work, additional languages have
;;   diminishing returns (testing Emacs internals, not our code)
;; - Avoids test suite bloat (21 tests vs 100+) while maintaining confidence
;; - Groups languages by similarity: C-style covers C/Java/Go/JavaScript/Rust
;;
;; See ai-prompts/quality-engineer.org: "Testing Framework/Library Integration"

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-comments)

;;; Test Helper

(defun test-delete-comments-in-mode (mode content-before expected-after)
  "Test comment deletion in MODE.
Insert CONTENT-BEFORE, run cj/delete-buffer-comments, verify EXPECTED-AFTER."
  (with-temp-buffer
    (funcall mode)
    (insert content-before)
    (cj/delete-buffer-comments)
    (should (equal (string-trim (buffer-string)) (string-trim expected-after)))))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

(ert-deftest test-delete-comments-elisp-simple-line-comments ()
  "Should delete simple line comments in emacs-lisp-mode."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ";; This is a comment\n(defun foo () nil)"
   "(defun foo () nil)"))

(ert-deftest test-delete-comments-elisp-inline-comments ()
  "Should delete inline/end-of-line comments."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   "(setq x 10) ;; set x to 10"
   "(setq x 10)"))

(ert-deftest test-delete-comments-elisp-only-comments ()
  "Buffer with only comments should become empty."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ";; Comment 1\n;; Comment 2\n;; Comment 3"
   ""))

(ert-deftest test-delete-comments-elisp-mixed-code-and-comments ()
  "Should preserve code and delete all comments."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ";; Header comment\n(defun foo ()\n  ;; body comment\n  (+ 1 2)) ;; inline"
   "(defun foo ()\n\n  (+ 1 2))"))

(ert-deftest test-delete-comments-elisp-empty-buffer ()
  "Should do nothing in empty buffer."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ""
   ""))

(ert-deftest test-delete-comments-elisp-no-comments ()
  "Should preserve all content when no comments exist."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   "(defun foo ()\n  (+ 1 2))"
   "(defun foo ()\n  (+ 1 2))"))

(ert-deftest test-delete-comments-elisp-whitespace-only-comments ()
  "Should delete comments containing only whitespace."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ";;    \n;; \t\n(setq x 1)"
   "(setq x 1)"))

(ert-deftest test-delete-comments-elisp-unicode-in-comments ()
  "Should handle unicode characters in comments."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ";; Hello 👋 مرحبا café\n(setq x 1)"
   "(setq x 1)"))

(ert-deftest test-delete-comments-elisp-indented-comments ()
  "Should delete comments at various indentation levels."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   "(defun foo ()\n  ;; indented comment\n    ;; more indented\n  (+ 1 2))"
   "(defun foo ()\n\n\n  (+ 1 2))"))

(ert-deftest test-delete-comments-elisp-special-chars-in-comments ()
  "Should handle special characters in comments."
  (test-delete-comments-in-mode
   'emacs-lisp-mode
   ";; Special: !@#$%^&*()[]{}|\\/<>?\n(setq x 1)"
   "(setq x 1)"))

(ert-deftest test-delete-comments-elisp-point-not-at-beginning ()
  "Should work regardless of initial point position."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Comment 1\n(setq x 1)\n;; Comment 2")
    (goto-char (point-max))  ; Point at end
    (cj/delete-buffer-comments)
    (should (equal (string-trim (buffer-string)) "(setq x 1)"))))

(ert-deftest test-delete-comments-elisp-does-not-affect-kill-ring ()
  "Should not add deleted comments to kill-ring."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Comment\n(setq x 1)")
    (setq kill-ring nil)
    (cj/delete-buffer-comments)
    (should (null kill-ring))))

(ert-deftest test-delete-comments-elisp-read-only-buffer ()
  "Should signal error in read-only buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Comment\n(setq x 1)")
    (read-only-mode 1)
    (should-error (cj/delete-buffer-comments))))

(ert-deftest test-delete-comments-elisp-narrowed-buffer ()
  "Should only affect visible region when narrowed."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Comment 1\n(setq x 1)\n;; Comment 2\n(setq y 2)")
    (goto-char (point-min))
    (forward-line 2)
    (narrow-to-region (point) (point-max))
    (cj/delete-buffer-comments)
    (widen)
    ;; First comment should remain (was outside narrowed region)
    ;; Second comment should be deleted
    (should (string-match-p "Comment 1" (buffer-string)))
    (should-not (string-match-p "Comment 2" (buffer-string)))))


;;; Python Tests (Hash-based comments)

(ert-deftest test-delete-comments-python-simple ()
  "Should delete Python hash comments."
  (test-delete-comments-in-mode
   'python-mode
   "# This is a comment\ndef foo():\n    return 42"
   "def foo():\n    return 42"))

(ert-deftest test-delete-comments-python-inline ()
  "Should delete inline Python comments."
  (test-delete-comments-in-mode
   'python-mode
   "x = 10  # set x to 10\ny = 20"
   "x = 10\ny = 20"))

(ert-deftest test-delete-comments-python-mixed ()
  "Should preserve code and delete Python comments."
  (test-delete-comments-in-mode
   'python-mode
   "# Header\ndef foo():\n    # body\n    return 42  # inline"
   "def foo():\n\n    return 42"))

;;; C Tests (C-style line and block comments)

(ert-deftest test-delete-comments-c-line-comments ()
  "Should delete C line comments (//)."
  (test-delete-comments-in-mode
   'c-mode
   "// This is a comment\nint main() {\n    return 0;\n}"
   "int main() {\n    return 0;\n}"))

(ert-deftest test-delete-comments-c-block-comments ()
  "Should delete C block comments (/* */)."
  (test-delete-comments-in-mode
   'c-mode
   "/* Block comment */\nint x = 10;"
   "int x = 10;"))

(ert-deftest test-delete-comments-c-mixed ()
  "Should delete both line and block comments in C."
  (test-delete-comments-in-mode
   'c-mode
   "// Line comment\n/* Block comment */\nint x = 10; // inline"
   "int x = 10;"))

(provide 'test-custom-comments-delete-buffer-comments)
;;; test-custom-comments-delete-buffer-comments.el ends here
