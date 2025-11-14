;;; test-custom-comments-comment-padded-divider.el --- Tests for cj/comment-padded-divider -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-padded-divider function from custom-comments.el
;;
;; This function generates a padded 3-line comment divider banner:
;; - Top line: comment-start + decoration chars
;; - Middle line: comment-start + padding spaces + text
;; - Bottom line: comment-start + decoration chars
;;
;; The key difference from simple-divider is the PADDING parameter which
;; adds spaces before the text to create visual indentation.
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-padded-divider)
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

(defun test-padded-divider-at-column (column-pos comment-start comment-end decoration-char text length padding)
  "Test cj/--comment-padded-divider at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-padded-divider with
COMMENT-START, COMMENT-END, DECORATION-CHAR, TEXT, LENGTH, and PADDING.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-padded-divider comment-start comment-end decoration-char text length padding)
    (buffer-string)))

;;; Emacs Lisp Tests (Primary Language - Comprehensive Coverage)

;;; Normal Cases

(ert-deftest test-padded-divider-elisp-basic ()
  "Should generate padded 3-line divider in emacs-lisp style."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Section Header" 70 2)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; First line should start with ;; and have decoration
    (should (string-match-p "^;; =" result))
    ;; Middle line should contain text with padding
    (should (string-match-p ";;   Section Header" result))))

(ert-deftest test-padded-divider-elisp-custom-padding ()
  "Should respect custom padding value."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Header" 70 4)))
    ;; Middle line should have 4 spaces before text
    (should (string-match-p ";;     Header" result))))

(ert-deftest test-padded-divider-elisp-zero-padding ()
  "Should work with zero padding."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "-" "Header" 70 0)))
    ;; Middle line should have text immediately after comment-start + space
    (should (string-match-p "^;; Header$" result))))

(ert-deftest test-padded-divider-elisp-large-padding ()
  "Should work with large padding value."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Text" 70 10)))
    ;; Middle line should have 10 spaces before text
    (should (string-match-p ";;           Text" result))))

(ert-deftest test-padded-divider-elisp-custom-decoration ()
  "Should use custom decoration character."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "*" "Header" 70 2)))
    (should (string-match-p ";; \\*" result))
    (should-not (string-match-p ";; =" result))))

(ert-deftest test-padded-divider-elisp-custom-text ()
  "Should include custom text in middle line."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Custom Text Here" 70 2)))
    (should (string-match-p "Custom Text Here" result))))

(ert-deftest test-padded-divider-elisp-empty-text ()
  "Should handle empty text string."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "-" "" 70 2)))
    ;; Should still generate 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should just be comment-start + padding
    (should (string-match-p "^;;   *\n" result))))

(ert-deftest test-padded-divider-elisp-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Header" 70 2)))
    ;; First character should be semicolon
    (should (string-prefix-p ";;" result))))

(ert-deftest test-padded-divider-elisp-indented ()
  "Should work when indented."
  (let ((result (test-padded-divider-at-column 4 ";;" "" "=" "Header" 70 2)))
    ;; Result should start with spaces
    (should (string-prefix-p "    ;;" result))
    ;; All lines should be indented
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p "    ;;" line)))))

;;; Boundary Cases

(ert-deftest test-padded-divider-elisp-minimum-length ()
  "Should work with minimum viable length at column 0."
  ;; Minimum: 2 (;;) + 1 (space) + 1 (space) + 3 (dashes) = 7
  (let ((result (test-padded-divider-at-column 0 ";;" "" "-" "" 7 0)))
    (should (= 3 (length (split-string result "\n" t))))))

(ert-deftest test-padded-divider-elisp-very-long-length ()
  "Should handle very long length."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Header" 200 2)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Decoration lines should be very long
    (let ((first-line (car (split-string result "\n" t))))
      (should (> (length first-line) 100)))))

(ert-deftest test-padded-divider-elisp-padding-larger-than-length ()
  "Should handle padding that exceeds reasonable bounds."
  ;; This tests behavior when padding is very large relative to length
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "X" 70 50)))
    ;; Should still generate output (text may extend beyond decoration)
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "X" result))))

(ert-deftest test-padded-divider-elisp-unicode-decoration ()
  "Should handle unicode decoration character."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "─" "Header" 70 2)))
    (should (string-match-p "─" result))))

(ert-deftest test-padded-divider-elisp-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Hello 👋 مرحبا café" 70 2)))
    (should (string-match-p "👋" result))
    (should (string-match-p "مرحبا" result))
    (should (string-match-p "café" result))))

(ert-deftest test-padded-divider-elisp-very-long-text ()
  "Should handle very long text."
  (let* ((long-text (make-string 100 ?x))
         (result (test-padded-divider-at-column 0 ";;" "" "=" long-text 70 2)))
    ;; Should still generate output
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should contain some of the text
    (should (string-match-p "xxx" result))))

(ert-deftest test-padded-divider-elisp-comment-end-empty ()
  "Should handle empty comment-end correctly."
  (let ((result (test-padded-divider-at-column 0 ";;" "" "=" "Header" 70 2)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Lines should not have trailing comment-end
    (should-not (string-match-p ";;.*;;$" result))))

(ert-deftest test-padded-divider-elisp-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-padded-divider-at-column 60 ";;" "" "=" "Header" 100 2)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; All lines should start with 60 spaces
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p (make-string 60 ?\s) line)))))

;;; Error Cases

(ert-deftest test-padded-divider-elisp-negative-padding ()
  "Should error with negative padding."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" "=" "Header" 70 -5)
   :type 'error))

(ert-deftest test-padded-divider-elisp-negative-length ()
  "Should error with negative length."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" "=" "Header" -10 2)
   :type 'error))

(ert-deftest test-padded-divider-elisp-zero-length ()
  "Should error with zero length."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" "=" "Header" 0 2)
   :type 'error))

(ert-deftest test-padded-divider-elisp-nil-decoration ()
  "Should error when decoration-char is nil."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" nil "Header" 70 2)
   :type 'wrong-type-argument))

(ert-deftest test-padded-divider-elisp-nil-text ()
  "Should error when text is nil."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" "=" nil 70 2)
   :type 'wrong-type-argument))

(ert-deftest test-padded-divider-elisp-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" "=" "Header" "not-a-number" 2)
   :type 'wrong-type-argument))

(ert-deftest test-padded-divider-elisp-non-integer-padding ()
  "Should error when padding is not an integer."
  (should-error
   (test-padded-divider-at-column 0 ";;" "" "=" "Header" 70 "not-a-number")
   :type 'wrong-type-argument))

;;; Python Tests (Hash-based comments)

(ert-deftest test-padded-divider-python-basic ()
  "Should generate padded divider with Python comment syntax."
  (let ((result (test-padded-divider-at-column 0 "#" "" "=" "Section" 70 2)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^# =" result))
    (should (string-match-p "#   Section" result))))

(ert-deftest test-padded-divider-python-indented ()
  "Should handle indented Python comments with padding."
  (let ((result (test-padded-divider-at-column 4 "#" "" "-" "Function Section" 70 4)))
    (should (string-prefix-p "    #" result))
    (should (string-match-p "Function Section" result))))

;;; C Tests (C-style comments)

(ert-deftest test-padded-divider-c-block-comments ()
  "Should generate padded divider with C block comment syntax."
  (let ((result (test-padded-divider-at-column 0 "/*" "*/" "=" "Section" 70 2)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\* =" result))
    (should (string-match-p "/\\*   Section" result))
    ;; Should include comment-end
    (should (string-match-p "\\*/" result))))

(provide 'test-custom-comments-comment-padded-divider)
;;; test-custom-comments-comment-padded-divider.el ends here
