;;; test-custom-comments-comment-unicode-box.el --- Tests for cj/comment-unicode-box -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-unicode-box function from custom-comments.el
;;
;; This function generates a 3-line unicode box comment:
;; - Top line: comment-start + top-left corner + horizontal lines + top-right corner
;; - Text line: comment-start + vertical bar + text + vertical bar
;; - Bottom line: comment-start + bottom-left corner + horizontal lines + bottom-right corner
;;
;; Supports both 'single and 'double box styles with different unicode characters.
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-unicode-box)
;; to avoid mocking user prompts. This follows our testing best practice
;; of separating business logic from UI interaction.
;;
;; Cross-Language Testing Strategy:
;; - Comprehensive testing in Emacs Lisp (our primary language)
;; - Representative testing in Python and C (hash-based and C-style comments)
;; - Function handles comment syntax generically, so testing 3 syntaxes
;;   proves cross-language compatibility
;; - See test-custom-comments-delete-buffer-comments.el for detailed rationale

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

;;; Test Helpers

(defun test-unicode-box-at-column (column-pos comment-start comment-end text length box-style)
  "Test cj/--comment-unicode-box at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-unicode-box with
COMMENT-START, COMMENT-END, TEXT, LENGTH, and BOX-STYLE.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-unicode-box comment-start comment-end text length box-style)
    (buffer-string)))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

;;; Normal Cases - Single Box Style

(ert-deftest test-unicode-box-elisp-single-basic ()
  "Should generate 3-line single-line unicode box in emacs-lisp style."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Section Header" 70 'single)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Should have single-line box characters
    (should (string-match-p "┌" result))
    (should (string-match-p "┐" result))
    (should (string-match-p "└" result))
    (should (string-match-p "┘" result))
    (should (string-match-p "─" result))
    (should (string-match-p "│" result))
    ;; Should contain text
    (should (string-match-p "Section Header" result))))

(ert-deftest test-unicode-box-elisp-double-basic ()
  "Should generate 3-line double-line unicode box in emacs-lisp style."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Section Header" 70 'double)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Should have double-line box characters
    (should (string-match-p "╔" result))
    (should (string-match-p "╗" result))
    (should (string-match-p "╚" result))
    (should (string-match-p "╝" result))
    (should (string-match-p "═" result))
    (should (string-match-p "║" result))
    ;; Should contain text
    (should (string-match-p "Section Header" result))))

(ert-deftest test-unicode-box-elisp-single-vs-double ()
  "Should use different characters for single vs double."
  (let ((single-result (test-unicode-box-at-column 0 ";;" "" "Header" 70 'single))
        (double-result (test-unicode-box-at-column 0 ";;" "" "Header" 70 'double)))
    ;; Single should have single-line chars but not double
    (should (string-match-p "─" single-result))
    (should-not (string-match-p "═" single-result))
    ;; Double should have double-line chars but not single
    (should (string-match-p "═" double-result))
    (should-not (string-match-p "─" double-result))))

(ert-deftest test-unicode-box-elisp-custom-text ()
  "Should include custom text in box."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Custom Text Here" 70 'single)))
    (should (string-match-p "Custom Text Here" result))))

(ert-deftest test-unicode-box-elisp-empty-text ()
  "Should handle empty text string."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "" 70 'single)))
    ;; Should still generate 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Should have box characters
    (should (string-match-p "┌" result))))

(ert-deftest test-unicode-box-elisp-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Header" 70 'single)))
    ;; First character should be semicolon
    (should (string-prefix-p ";;" result))))

(ert-deftest test-unicode-box-elisp-indented ()
  "Should work when indented."
  (let ((result (test-unicode-box-at-column 4 ";;" "" "Header" 70 'single)))
    ;; Result should start with spaces
    (should (string-prefix-p "    ;;" result))
    ;; All lines should be indented
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p "    ;;" line)))))

(ert-deftest test-unicode-box-elisp-short-text ()
  "Should handle short text properly."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "X" 70 'single)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Text should be present
    (should (string-match-p "X" result))))

(ert-deftest test-unicode-box-elisp-long-text ()
  "Should handle longer text."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "This is a longer header text" 70 'single)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Text should be present
    (should (string-match-p "This is a longer header text" result))))

;;; Boundary Cases

(ert-deftest test-unicode-box-elisp-minimum-length ()
  "Should work with minimum viable length."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "X" 15 'single)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "X" result))))

(ert-deftest test-unicode-box-elisp-very-long-length ()
  "Should handle very long length."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Header" 200 'single)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Border lines should be very long
    (let ((first-line (car (split-string result "\n" t))))
      (should (> (length first-line) 100)))))

(ert-deftest test-unicode-box-elisp-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Hello 👋 مرحبا café" 70 'single)))
    (should (string-match-p "👋" result))
    (should (string-match-p "مرحبا" result))
    (should (string-match-p "café" result))))

(ert-deftest test-unicode-box-elisp-very-long-text ()
  "Should handle very long text."
  (let* ((long-text (make-string 100 ?x))
         (result (test-unicode-box-at-column 0 ";;" "" long-text 70 'single)))
    ;; Should still generate output
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should contain some of the text
    (should (string-match-p "xxx" result))))

(ert-deftest test-unicode-box-elisp-comment-end-empty ()
  "Should handle empty comment-end correctly."
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Header" 70 'single)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Lines should not have trailing comment-end
    (should-not (string-match-p ";;.*;;$" result))))

(ert-deftest test-unicode-box-elisp-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-unicode-box-at-column 60 ";;" "" "Header" 100 'single)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; All lines should start with 60 spaces
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p (make-string 60 ?\s) line)))))

;;; Error Cases

(ert-deftest test-unicode-box-elisp-length-too-small ()
  "Should error when length is too small."
  (should-error
   (test-unicode-box-at-column 0 ";;" "" "Header" 5 'single)
   :type 'error))

(ert-deftest test-unicode-box-elisp-negative-length ()
  "Should error with negative length."
  (should-error
   (test-unicode-box-at-column 0 ";;" "" "Header" -10 'single)
   :type 'error))

(ert-deftest test-unicode-box-elisp-zero-length ()
  "Should error with zero length."
  (should-error
   (test-unicode-box-at-column 0 ";;" "" "Header" 0 'single)
   :type 'error))

(ert-deftest test-unicode-box-elisp-nil-text ()
  "Should error when text is nil."
  (should-error
   (test-unicode-box-at-column 0 ";;" "" nil 70 'single)
   :type 'wrong-type-argument))

(ert-deftest test-unicode-box-elisp-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-unicode-box-at-column 0 ";;" "" "Header" "not-a-number" 'single)
   :type 'wrong-type-argument))

(ert-deftest test-unicode-box-elisp-invalid-box-style ()
  "Should handle invalid box-style gracefully."
  ;; Function may use a default or error - either is acceptable
  (let ((result (test-unicode-box-at-column 0 ";;" "" "Header" 70 'invalid)))
    ;; Should still generate some output
    (should (stringp result))))

;;; Python Tests (Hash-based comments)

(ert-deftest test-unicode-box-python-single ()
  "Should generate unicode box with Python comment syntax."
  (let ((result (test-unicode-box-at-column 0 "#" "" "Section" 70 'single)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^# ┌" result))
    (should (string-match-p "Section" result))))

(ert-deftest test-unicode-box-python-double ()
  "Should generate double-line unicode box with Python comment syntax."
  (let ((result (test-unicode-box-at-column 0 "#" "" "Section" 70 'double)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^# ╔" result))
    (should (string-match-p "Section" result))))

;;; C Tests (C-style comments)

(ert-deftest test-unicode-box-c-block-comments-single ()
  "Should generate unicode box with C block comment syntax."
  (let ((result (test-unicode-box-at-column 0 "/*" "*/" "Section" 70 'single)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\* ┌" result))
    (should (string-match-p "Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/" result))))

(ert-deftest test-unicode-box-c-block-comments-double ()
  "Should generate double-line unicode box with C block comment syntax."
  (let ((result (test-unicode-box-at-column 0 "/*" "*/" "Section" 70 'double)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\* ╔" result))
    (should (string-match-p "Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/" result))))

(provide 'test-custom-comments-comment-unicode-box)
;;; test-custom-comments-comment-unicode-box.el ends here
