;;; test-custom-comments-comment-inline-border.el --- Tests for cj/comment-inline-border -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-inline-border function from custom-comments.el
;;
;; This function generates a single-line centered comment with decoration borders:
;; Format: comment-start + decoration + space + text + space + decoration + comment-end
;; Example: ";; ======= Section Header ======="
;;
;; The text is centered with decoration characters on both sides. When text has
;; odd length, the right side gets one less decoration character.
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-inline-border)
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

(defun test-inline-border-at-column (column-pos comment-start comment-end decoration-char text length)
  "Test cj/--comment-inline-border at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-inline-border with
COMMENT-START, COMMENT-END, DECORATION-CHAR, TEXT, and LENGTH.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-inline-border comment-start comment-end decoration-char text length)
    (buffer-string)))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

;;; Normal Cases

(ert-deftest test-inline-border-elisp-basic ()
  "Should generate single-line centered comment in emacs-lisp style."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Section Header" 70)))
    ;; Should be single line
    (should (= 1 (length (split-string result "\n" t))))
    ;; Should start with ;;
    (should (string-match-p "^;; =" result))
    ;; Should contain text
    (should (string-match-p "Section Header" result))
    ;; Should have decoration on both sides
    (should (string-match-p "= Section Header =" result))))

(ert-deftest test-inline-border-elisp-custom-decoration ()
  "Should use custom decoration character."
  (let ((result (test-inline-border-at-column 0 ";;" "" "#" "Header" 70)))
    (should (string-match-p ";; #" result))
    (should (string-match-p "# Header #" result))
    (should-not (string-match-p "=" result))))

(ert-deftest test-inline-border-elisp-custom-text ()
  "Should include custom text centered."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Custom Text Here" 70)))
    (should (string-match-p "Custom Text Here" result))))

(ert-deftest test-inline-border-elisp-empty-text ()
  "Should handle empty text string."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "" 70)))
    ;; Should still generate output with decoration
    (should (string-match-p ";; =" result))
    ;; Should not have extra spaces where text would be
    (should-not (string-match-p "  " result))))

(ert-deftest test-inline-border-elisp-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Header" 70)))
    ;; First character should be semicolon
    (should (string-prefix-p ";;" result))))

(ert-deftest test-inline-border-elisp-indented ()
  "Should work when indented."
  (let ((result (test-inline-border-at-column 4 ";;" "" "=" "Header" 70)))
    ;; Result should start with spaces
    (should (string-prefix-p "    ;;" result))))

(ert-deftest test-inline-border-elisp-short-text ()
  "Should center short text properly."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "X" 70)))
    (should (string-match-p "X" result))
    ;; Should have decoration on both sides
    (should (string-match-p "= X =" result))))

(ert-deftest test-inline-border-elisp-custom-length ()
  "Should respect custom length."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Header" 50)))
    ;; Line should be approximately 50 chars
    (let ((line (car (split-string result "\n" t))))
      (should (<= (length line) 51))
      (should (>= (length line) 48)))))

;;; Boundary Cases

(ert-deftest test-inline-border-elisp-minimum-length ()
  "Should work with minimum viable length."
  ;; Minimum: 2 (;;) + 1 (space) + 1 (space) + 2 (min decoration each side) = 6
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "" 10)))
    (should (string-match-p ";" result))))

(ert-deftest test-inline-border-elisp-text-centering-even ()
  "Should center text properly with even length."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "EVEN" 70)))
    ;; Text should be centered with roughly equal decoration
    (should (string-match-p "= EVEN =" result))))

(ert-deftest test-inline-border-elisp-text-centering-odd ()
  "Should center text properly with odd length."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "ODD" 70)))
    ;; Text should be centered (right side has one less due to odd length)
    (should (string-match-p "= ODD =" result))))

(ert-deftest test-inline-border-elisp-very-long-text ()
  "Should handle text that fills most of the line."
  (let* ((long-text (make-string 50 ?x))
         (result (test-inline-border-at-column 0 ";;" "" "=" long-text 70)))
    ;; Should still have decoration
    (should (string-match-p "=" result))
    ;; Text should be present
    (should (string-match-p "xxx" result))))

(ert-deftest test-inline-border-elisp-unicode-decoration ()
  "Should handle unicode decoration character."
  (let ((result (test-inline-border-at-column 0 ";;" "" "─" "Header" 70)))
    (should (string-match-p "─" result))))

(ert-deftest test-inline-border-elisp-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Hello 👋 café" 70)))
    (should (string-match-p "👋" result))
    (should (string-match-p "café" result))))

(ert-deftest test-inline-border-elisp-comment-end-empty ()
  "Should handle empty comment-end correctly."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Header" 70)))
    ;; Line should not have trailing comment-end
    (should-not (string-match-p ";;$" result))))

(ert-deftest test-inline-border-elisp-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-inline-border-at-column 60 ";;" "" "=" "H" 100)))
    (should (string-prefix-p (make-string 60 ?\s) result))))

(ert-deftest test-inline-border-elisp-minimum-decoration-each-side ()
  "Should have at least 2 decoration chars on each side."
  (let ((result (test-inline-border-at-column 0 ";;" "" "=" "Test" 20)))
    ;; Should have at least == on each side
    (should (string-match-p "== Test ==" result))))

;;; Error Cases

(ert-deftest test-inline-border-elisp-length-too-small ()
  "Should error when length is too small for text."
  (should-error
   (test-inline-border-at-column 0 ";;" "" "=" "Very Long Header Text" 20)
   :type 'error))

(ert-deftest test-inline-border-elisp-negative-length ()
  "Should error with negative length."
  (should-error
   (test-inline-border-at-column 0 ";;" "" "=" "Header" -10)
   :type 'error))

(ert-deftest test-inline-border-elisp-zero-length ()
  "Should error with zero length."
  (should-error
   (test-inline-border-at-column 0 ";;" "" "=" "Header" 0)
   :type 'error))

(ert-deftest test-inline-border-elisp-nil-decoration ()
  "Should error when decoration-char is nil."
  (should-error
   (test-inline-border-at-column 0 ";;" "" nil "Header" 70)
   :type 'wrong-type-argument))

(ert-deftest test-inline-border-elisp-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-inline-border-at-column 0 ";;" "" "=" "Header" "not-a-number")
   :type 'wrong-type-argument))

;;; Python Tests (Hash-based comments)

(ert-deftest test-inline-border-python-basic ()
  "Should generate inline border with Python comment syntax."
  (let ((result (test-inline-border-at-column 0 "#" "" "=" "Section" 70)))
    (should (string-match-p "^# =" result))
    (should (string-match-p "Section" result))))

(ert-deftest test-inline-border-python-indented ()
  "Should handle indented Python comments."
  (let ((result (test-inline-border-at-column 4 "#" "" "-" "Function Section" 70)))
    (should (string-prefix-p "    #" result))
    (should (string-match-p "Function Section" result))))

;;; C Tests (C-style comments)

(ert-deftest test-inline-border-c-block-comments ()
  "Should generate inline border with C block comment syntax."
  (let ((result (test-inline-border-at-column 0 "/*" "*/" "=" "Section" 70)))
    (should (string-match-p "^/\\* =" result))
    (should (string-match-p "Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/$" result))))

(ert-deftest test-inline-border-c-line-comments ()
  "Should generate inline border with C line comment syntax."
  (let ((result (test-inline-border-at-column 0 "//" "" "-" "Header" 70)))
    (should (string-match-p "^// -" result))
    (should (string-match-p "Header" result))))

(provide 'test-custom-comments-comment-inline-border)
;;; test-custom-comments-comment-inline-border.el ends here
