;;; test-custom-comments-comment-block-banner.el --- Tests for cj/comment-block-banner -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/comment-block-banner function from custom-comments.el
;;
;; This function generates a 3-line block banner comment (JSDoc/Doxygen style):
;; - Top line: comment-start (e.g., /*) + decoration chars
;; - Text line: space + decoration char + space + text
;; - Bottom line: space + decoration chars + comment-end (e.g., */)
;;
;; This style is common in C, JavaScript, Java, and other languages that use
;; block comments.
;;
;; We test the NON-INTERACTIVE implementation (cj/--comment-block-banner)
;; to avoid mocking user prompts. This follows our testing best practice
;; of separating business logic from UI interaction.
;;
;; Cross-Language Testing Strategy:
;; - Comprehensive testing in C (the primary language for this style)
;; - Representative testing in JavaScript/Java (similar block comment syntax)
;; - This style is specifically designed for block comments, so we focus
;;   testing on languages that use /* */ syntax
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

(defun test-block-banner-at-column (column-pos comment-start comment-end decoration-char text length)
  "Test cj/--comment-block-banner at COLUMN-POS indentation.
Insert spaces to reach COLUMN-POS, then call cj/--comment-block-banner with
COMMENT-START, COMMENT-END, DECORATION-CHAR, TEXT, and LENGTH.
Returns the buffer string for assertions."
  (with-temp-buffer
    (when (> column-pos 0)
      (insert (make-string column-pos ?\s)))
    (cj/--comment-block-banner comment-start comment-end decoration-char text length)
    (buffer-string)))

;;; C/JavaScript/Java Tests (Block Comment Languages - Comprehensive Coverage)

;;; Normal Cases

(ert-deftest test-block-banner-c-basic ()
  "Should generate 3-line block banner in C style."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "Section Header" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; First line should start with /*
    (should (string-match-p "^/\\*\\*" result))
    ;; Middle line should contain text
    (should (string-match-p "\\* Section Header" result))
    ;; Last line should end with */
    (should (string-match-p "\\*/$" result))))

(ert-deftest test-block-banner-c-custom-decoration ()
  "Should use custom decoration character."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "#" "Header" 70)))
    (should (string-match-p "/\\*#" result))
    (should (string-match-p " # Header" result))))

(ert-deftest test-block-banner-c-custom-text ()
  "Should include custom text in banner."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "Custom Text Here" 70)))
    (should (string-match-p "Custom Text Here" result))))

(ert-deftest test-block-banner-c-empty-text ()
  "Should handle empty text string."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "" 70)))
    ;; Should still generate 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Should have comment delimiters
    (should (string-match-p "/\\*" result))
    (should (string-match-p "\\*/$" result))))

(ert-deftest test-block-banner-c-at-column-0 ()
  "Should work at column 0."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "Header" 70)))
    ;; First character should be /
    (should (string-prefix-p "/*" result))))

(ert-deftest test-block-banner-c-indented ()
  "Should work when indented."
  (let ((result (test-block-banner-at-column 4 "/*" "*/" "*" "Header" 70)))
    ;; First line should start with spaces
    (should (string-prefix-p "    /*" result))
    ;; Other lines should be indented
    (let ((lines (split-string result "\n" t)))
      (should (string-prefix-p "     " (nth 1 lines)))  ; text line has extra space
      (should (string-prefix-p "     " (nth 2 lines)))))) ; bottom line has extra space

(ert-deftest test-block-banner-c-short-text ()
  "Should handle short text properly."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "X" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Text should be present
    (should (string-match-p "X" result))))

(ert-deftest test-block-banner-c-long-text ()
  "Should handle longer text."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "This is a longer header text" 70)))
    ;; Should have 3 lines
    (should (= 3 (length (split-string result "\n" t))))
    ;; Text should be present
    (should (string-match-p "This is a longer header text" result))))

(ert-deftest test-block-banner-c-custom-length ()
  "Should respect custom length."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "Header" 50)))
    ;; Top line should be approximately 50 chars
    (let ((first-line (car (split-string result "\n" t))))
      (should (<= (length first-line) 51))
      (should (>= (length first-line) 48)))))

;;; Boundary Cases

(ert-deftest test-block-banner-c-minimum-length ()
  "Should work with minimum viable length."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "X" 10)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "X" result))))

(ert-deftest test-block-banner-c-very-long-length ()
  "Should handle very long length."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "Header" 200)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; Top line should be very long
    (let ((first-line (car (split-string result "\n" t))))
      (should (> (length first-line) 100)))))

(ert-deftest test-block-banner-c-unicode-decoration ()
  "Should handle unicode decoration character."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "✦" "Header" 70)))
    (should (string-match-p "✦" result))))

(ert-deftest test-block-banner-c-unicode-text ()
  "Should handle unicode in text."
  (let ((result (test-block-banner-at-column 0 "/*" "*/" "*" "Hello 👋 مرحبا café" 70)))
    (should (string-match-p "👋" result))
    (should (string-match-p "مرحبا" result))
    (should (string-match-p "café" result))))

(ert-deftest test-block-banner-c-very-long-text ()
  "Should handle very long text."
  (let* ((long-text (make-string 100 ?x))
         (result (test-block-banner-at-column 0 "/*" "*/" "*" long-text 70)))
    ;; Should still generate output
    (should (= 3 (length (split-string result "\n" t))))
    ;; Middle line should contain some of the text
    (should (string-match-p "xxx" result))))

(ert-deftest test-block-banner-c-max-indentation ()
  "Should handle maximum practical indentation."
  (let ((result (test-block-banner-at-column 60 "/*" "*/" "*" "Header" 100)))
    (should (= 3 (length (split-string result "\n" t))))
    ;; First line should start with 60 spaces
    (should (string-prefix-p (make-string 60 ?\s) result))))

;;; Error Cases

(ert-deftest test-block-banner-c-length-too-small ()
  "Should error when length is too small."
  (should-error
   (test-block-banner-at-column 0 "/*" "*/" "*" "Header" 3)
   :type 'error))

(ert-deftest test-block-banner-c-negative-length ()
  "Should error with negative length."
  (should-error
   (test-block-banner-at-column 0 "/*" "*/" "*" "Header" -10)
   :type 'error))

(ert-deftest test-block-banner-c-zero-length ()
  "Should error with zero length."
  (should-error
   (test-block-banner-at-column 0 "/*" "*/" "*" "Header" 0)
   :type 'error))

(ert-deftest test-block-banner-c-nil-decoration ()
  "Should error when decoration-char is nil."
  (should-error
   (test-block-banner-at-column 0 "/*" "*/" nil "Header" 70)
   :type 'wrong-type-argument))

(ert-deftest test-block-banner-c-nil-text ()
  "Should error when text is nil."
  (should-error
   (test-block-banner-at-column 0 "/*" "*/" "*" nil 70)
   :type 'wrong-type-argument))

(ert-deftest test-block-banner-c-non-integer-length ()
  "Should error when length is not an integer."
  (should-error
   (test-block-banner-at-column 0 "/*" "*/" "*" "Header" "not-a-number")
   :type 'wrong-type-argument))

;;; Alternative Block Comment Styles

(ert-deftest test-block-banner-java-style ()
  "Should work with Java-style block comments."
  (let ((result (test-block-banner-at-column 0 "/**" "*/" "*" "JavaDoc Comment" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-match-p "^/\\*\\*\\*" result))
    (should (string-match-p "JavaDoc Comment" result))))

(ert-deftest test-block-banner-js-style ()
  "Should work with JavaScript-style block comments."
  (let ((result (test-block-banner-at-column 2 "/*" "*/" "*" "Function Documentation" 70)))
    (should (= 3 (length (split-string result "\n" t))))
    (should (string-prefix-p "  /*" result))
    (should (string-match-p "Function Documentation" result))))

(provide 'test-custom-comments-comment-block-banner)
;;; test-custom-comments-comment-block-banner.el ends here
