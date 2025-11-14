;;; test-custom-comments-comment-reformat.el --- Tests for cj/comment-reformat -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-reformat function from custom-comments.el
;;
;; This function reformats multi-line comments into a single paragraph by:
;; 1. Uncommenting the selected region
;; 2. Joining lines together (via cj/join-line-or-region)
;; 3. Re-commenting the result
;; 4. Temporarily reducing fill-column by 3 during the join operation
;;
;; Dependencies:
;; - Requires cj/join-line-or-region from custom-line-paragraph.el
;; - We load the REAL module to test actual integration behavior
;; - This follows our "test production code" guideline
;; - If join-line-or-region has bugs, our tests will catch integration issues
;;
;; Cross-Language Testing Strategy:
;; - Comprehensive testing in Emacs Lisp (12 tests)
;; - Representative testing in Python and C (1 test each)
;; - Function delegates to uncomment-region/comment-region, so we test OUR logic
;; - See test-custom-comments-delete-buffer-comments.el for detailed rationale

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Load the real custom-line-paragraph module (for cj/join-line-or-region)
(require 'custom-line-paragraph)

;; Now load the actual production module
(require 'custom-comments)

;;; Test Helpers

(defun test-comment-reformat-in-mode (mode content-before expected-after)
  "Test comment reformatting in MODE.
Insert CONTENT-BEFORE, select all, run cj/comment-reformat, verify EXPECTED-AFTER."
  (with-temp-buffer
    (transient-mark-mode 1)  ; Enable transient-mark-mode for batch testing
    (funcall mode)
    (insert content-before)
    (mark-whole-buffer)
    (activate-mark)  ; Explicitly activate the mark
    (cj/comment-reformat)
    (should (equal (string-trim (buffer-string)) (string-trim expected-after)))))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

(ert-deftest test-comment-reformat-elisp-simple-multiline ()
  "Should join multiple commented lines into one."
  (test-comment-reformat-in-mode
   'emacs-lisp-mode
   ";; Line one\n;; Line two\n;; Line three"
   ";; Line one Line two Line three"))

(ert-deftest test-comment-reformat-elisp-preserves-content ()
  "Should preserve text content after reformat."
  (test-comment-reformat-in-mode
   'emacs-lisp-mode
   ";; Hello world\n;; from Emacs"
   ";; Hello world from Emacs"))

(ert-deftest test-comment-reformat-elisp-restores-fill-column ()
  "Should restore fill-column after operation."
  (with-temp-buffer
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (let ((original-fill-column fill-column))
      (insert ";; Line one\n;; Line two")
      (mark-whole-buffer)
      (activate-mark)
      (cj/comment-reformat)
      (should (= fill-column original-fill-column)))))

(ert-deftest test-comment-reformat-elisp-single-line ()
  "Should handle single commented line."
  (test-comment-reformat-in-mode
   'emacs-lisp-mode
   ";; Single line comment"
   ";; Single line comment"))

(ert-deftest test-comment-reformat-elisp-empty-region ()
  "Should error when trying to comment empty buffer."
  (with-temp-buffer
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (mark-whole-buffer)
    (activate-mark)
    (should-error (cj/comment-reformat))))

(ert-deftest test-comment-reformat-elisp-whitespace-in-comments ()
  "Should handle comments with only whitespace."
  (test-comment-reformat-in-mode
   'emacs-lisp-mode
   ";;   \n;;  \n;; text"
   ";; text"))

(ert-deftest test-comment-reformat-elisp-unicode ()
  "Should handle unicode in comments."
  (test-comment-reformat-in-mode
   'emacs-lisp-mode
   ";; Hello 👋\n;; مرحبا café"
   ";; Hello 👋 مرحبا café"))

(ert-deftest test-comment-reformat-elisp-long-text ()
  "Should handle many lines of comments."
  (test-comment-reformat-in-mode
   'emacs-lisp-mode
   ";; Line 1\n;; Line 2\n;; Line 3\n;; Line 4\n;; Line 5"
   ";; Line 1 Line 2 Line 3 Line 4 Line 5"))

(ert-deftest test-comment-reformat-elisp-indented-comments ()
  "Should handle indented comments."
  (with-temp-buffer
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (insert "  ;; Indented line 1\n  ;; Indented line 2")
    (mark-whole-buffer)
    (activate-mark)
    (cj/comment-reformat)
    ;; After reformatting, should still be commented
    (should (string-match-p ";;" (buffer-string)))
    ;; Content should be joined
    (should (string-match-p "line 1.*line 2" (buffer-string)))))

(ert-deftest test-comment-reformat-elisp-region-at-buffer-start ()
  "Should handle region at buffer start."
  (with-temp-buffer
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (insert ";; Start line 1\n;; Start line 2\n(setq x 1)")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (activate-mark)
    (cj/comment-reformat)
    (should (string-match-p ";; Start line 1.*Start line 2" (buffer-string)))))

(ert-deftest test-comment-reformat-elisp-no-region-active ()
  "Should show message when no region selected."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Comment line")
    (deactivate-mark)
    (let ((message-log-max nil)
          (messages '()))
      ;; Capture messages
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (cj/comment-reformat)
        (should (string-match-p "No region was selected" (car messages)))))))

(ert-deftest test-comment-reformat-elisp-read-only-buffer ()
  "Should signal error in read-only buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Line 1\n;; Line 2")
    (mark-whole-buffer)
    (read-only-mode 1)
    (should-error (cj/comment-reformat))))

;;; Python Tests (Hash-based comments)

(ert-deftest test-comment-reformat-python-simple ()
  "Should join Python hash comments."
  (test-comment-reformat-in-mode
   'python-mode
   "# Line one\n# Line two"
   "# Line one Line two"))

;;; C Tests (C-style comments)

(ert-deftest test-comment-reformat-c-line-comments ()
  "Should join C line comments (C-mode converts to block comments)."
  (test-comment-reformat-in-mode
   'c-mode
   "// Line one\n// Line two"
   "/* Line one Line two */"))

(provide 'test-custom-comments-comment-reformat)
;;; test-custom-comments-comment-reformat.el ends here
