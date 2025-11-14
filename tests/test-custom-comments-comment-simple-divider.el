;;; test-custom-comments-comment-simple-divider.el --- Tests for cj/comment-simple-divider -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-simple-divider function from custom-comments.el
;;
;; This function generates a simple 3-line comment divider banner:
;; - Top line: comment-start + decoration chars
;; - Middle line: comment-start + text
;; - Bottom line: comment-start + decoration chars
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-simple-divider)
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

(defun test-simple-divider-at-column (column-pos comment-start comment-end decoration-char text length)
  "Test cj/--comment-simple-divider at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-simple-divider with
COMMENT-START, COMMENT-END, DECORATION-CHAR, TEXT, and LENGTH.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-simple-divider comment-start comment-end decoration-char text length)
    (buffer-string)))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

;;; Normal Cases

(ert-deftest test-simple-divider-elisp-basic ()
  "Should generate simple 3-line divider in emacs-lisp style."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-" "Section Header" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Each line should start with ;;
    (should (string-match-p "^;; -" result))
    ;; Middle line should contain text
    (should (string-match-p ";; Section Header" result))))

(ert-deftest test-simple-divider-elisp-custom-decoration ()
  "Should use custom decoration character."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "=" "Header" 70)))
    (should (string-match-p ";; =" result))
    (should-not (string-match-p ";; -" result))))

(ert-deftest test-simple-divider-elisp-custom-text ()
  "Should include custom text in middle line."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-" "Custom Text Here" 70)))
    (should (string-match-p ";; Custom Text Here" result))))

(ert-deftest test-simple-divider-elisp-custom-length ()
  "Should respect custom length."
  (let* ((result (test-simple-divider-at-column 0 ";;" "" "-" "Header" 50))
         (lines (split-string result "\n" t)))
    ;; Should have 3 lines
    (should (= 3 (length lines)))
    ;; First and last lines (decoration) should be approximately 50 chars
    (should (<= (length (car lines)) 51))
    (should (>= (length (car lines)) 48))
    (should (<= (length (car (last lines))) 51))
    (should (>= (length (car (last lines))) 48))))

(ert-deftest test-simple-divider-elisp-empty-text ()
  "Should handle empty text string."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-" "" 70)))
    ;; Should still generate 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should just be comment-start
    (should (string-match-p "^;; *\n" result))))

(ert-deftest test-simple-divider-elisp-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-""Header" 70)))
    ;; First character should be semicolon
    (should (string-prefix-p ";;" result))))

(ert-deftest test-simple-divider-elisp-indented ()
  "Should work when indented."
  (let ((result (test-simple-divider-at-column 4 ";;" "" "-""Header" 70)))
    ;; Result should start with spaces
    (should (string-prefix-p "    ;;" result))
    ;; All lines should be indented
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p "    ;;" line)))))

;;; Boundary Cases

(ert-deftest test-simple-divider-elisp-minimum-length ()
  "Should work with minimum viable length at column 0."
  ;; Minimum length at column 0: 2 (;;) + 1 (space) + 1 (space) + 3 (dashes) = 7
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-""" 7)))
    (should (= 3 (length (split-string result "\n" t))))))

(ert-deftest test-simple-divider-elisp-minimum-length-indented ()
  "Should work with minimum viable length when indented."
  ;; At column 4, minimum is 4 + 2 + 1 + 1 + 3 = 11
  (let ((result (test-simple-divider-at-column 4 ";;" "" "-""" 11)))
    (should (= 3 (length (split-string result "\n" t))))))

(ert-deftest test-simple-divider-elisp-very-long-length ()
  "Should handle very long length."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-""Header" 200)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Decoration lines should be very long
    (let ((first-line (car (split-string result "\n" t))))
      (should (> (length first-line) 100)))))

(ert-deftest test-simple-divider-elisp-unicode-decoration ()
  "Should handle unicode decoration character."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "─""Header" 70)))
    (should (string-match-p "─" result))))

(ert-deftest test-simple-divider-elisp-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-""Hello 👋 مرحبا café" 70)))
    (should (string-match-p "👋" result))
    (should (string-match-p "مرحبا" result))
    (should (string-match-p "café" result))))

(ert-deftest test-simple-divider-elisp-very-long-text ()
  "Should handle very long text (may wrap or truncate)."
  (let* ((long-text (make-string 100 ?x))
         (result (test-simple-divider-at-column 0 ";;" "" "-"long-text 70)))
    ;; Should still generate output (behavior may vary)
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should contain some of the text
    (should (string-match-p "xxx" result))))

(ert-deftest test-simple-divider-elisp-comment-end-empty ()
  "Should handle empty comment-end correctly."
  (let ((result (test-simple-divider-at-column 0 ";;" "" "-""Header" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Lines should not have trailing comment-end
    (should-not (string-match-p ";;.*;;$" result))))

(ert-deftest test-simple-divider-elisp-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-simple-divider-at-column 60 ";;" "" "-""Header" 100)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; All lines should start with 60 spaces
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p (make-string 60 ?\s) line)))))

;;; Error Cases

(ert-deftest test-simple-divider-elisp-length-too-small-column-0 ()
  "Should error when length is too small at column 0."
  (should-error
   (test-simple-divider-at-column 0 ";;" "" "-" "Header" 5)
   :type 'error))

(ert-deftest test-simple-divider-elisp-length-too-small-indented ()
  "Should error when length is too small for indentation level."
  (should-error
   (test-simple-divider-at-column 10 ";;" "" "-" "Header" 15)
   :type 'error))

(ert-deftest test-simple-divider-elisp-negative-length ()
  "Should error with negative length."
  (should-error
   (test-simple-divider-at-column 0 ";;" "" "-" "Header" -10)
   :type 'error))

(ert-deftest test-simple-divider-elisp-zero-length ()
  "Should error with zero length."
  (should-error
   (test-simple-divider-at-column 0 ";;" "" "-" "Header" 0)
   :type 'error))

(ert-deftest test-simple-divider-elisp-nil-decoration ()
  "Should error when decoration-char is nil."
  (should-error
   (test-simple-divider-at-column 0 ";;" "" nil "Header" 70)
   :type 'wrong-type-argument))

(ert-deftest test-simple-divider-elisp-nil-text ()
  "Should error when text is nil."
  (should-error
   (test-simple-divider-at-column 0 ";;" "" "-" nil 70)
   :type 'wrong-type-argument))

(ert-deftest test-simple-divider-elisp-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-simple-divider-at-column 0 ";;" "" "-""Header" "not-a-number")
   :type 'wrong-type-argument))

;;; Python Tests (Hash-based comments)

(ert-deftest test-simple-divider-python-basic ()
  "Should generate simple divider with Python comment syntax."
  (let ((result (test-simple-divider-at-column 0 "#" "" "-""Section" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^# -" result))
    (should (string-match-p "# Section" result))))

(ert-deftest test-simple-divider-python-indented ()
  "Should handle indented Python comments."
  (let ((result (test-simple-divider-at-column 4 "#" "" "=""Function Section" 70)))
    (should (string-prefix-p "    #" result))
    (should (string-match-p "Function Section" result))))

;;; C Tests (C-style comments)

(ert-deftest test-simple-divider-c-block-comments ()
  "Should generate simple divider with C block comment syntax."
  (let ((result (test-simple-divider-at-column 0 "/*" "*/" "-""Section" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\* -" result))
    (should (string-match-p "/\\* Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/" result))))

(ert-deftest test-simple-divider-c-line-comments ()
  "Should generate simple divider with C line comment syntax."
  (let ((result (test-simple-divider-at-column 0 "//" "" "=""Header" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^// =" result))
    (should (string-match-p "// Header" result))))

(provide 'test-custom-comments-comment-simple-divider)
;;; test-custom-comments-comment-simple-divider.el ends here
