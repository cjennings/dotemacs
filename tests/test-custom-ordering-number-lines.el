;;; test-custom-ordering-number-lines.el --- Tests for cj/--number-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--number-lines function from custom-ordering.el
;;
;; This function numbers lines in a region with a customizable format.
;; The format string uses "N" as a placeholder for the line number.
;; Optionally supports zero-padding for alignment.
;;
;; Examples:
;; Input:  "apple\nbanana\ncherry"
;; Format: "N. "
;; Output: "1. apple\n2. banana\n3. cherry"
;;
;; With zero-padding and 100 lines:
;; "001. line\n002. line\n...\n100. line"
;;
;; We test the NON-INTERACTIVE implementation (cj/--number-lines) to avoid
;; mocking user input. This follows our testing best practice of
;; separating business logic from UI interaction.

;;; Code:

(require 'ert)
(require 'testutil-general)
(require 'cl-lib)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-ordering)

;;; Test Helpers

(defun test-number-lines (input-text format-string zero-pad)
  "Test cj/--number-lines on INPUT-TEXT.
FORMAT-STRING is the format template.
ZERO-PAD enables zero-padding.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--number-lines (point-min) (point-max) format-string zero-pad)))

;;; Normal Cases - Standard Format "N. "

(ert-deftest test-number-lines-standard-format ()
  "Should number lines with standard format."
  (let ((result (test-number-lines "apple\nbanana\ncherry" "N. " nil)))
    (should (string= result "1. apple\n2. banana\n3. cherry"))))

(ert-deftest test-number-lines-two-lines ()
  "Should number two lines."
  (let ((result (test-number-lines "first\nsecond" "N. " nil)))
    (should (string= result "1. first\n2. second"))))

(ert-deftest test-number-lines-single-line ()
  "Should number single line."
  (let ((result (test-number-lines "only" "N. " nil)))
    (should (string= result "1. only"))))

;;; Normal Cases - Alternative Formats

(ert-deftest test-number-lines-parenthesis-format ()
  "Should number with parenthesis format."
  (let ((result (test-number-lines "a\nb\nc" "N) " nil)))
    (should (string= result "1) a\n2) b\n3) c"))))

(ert-deftest test-number-lines-bracket-format ()
  "Should number with bracket format."
  (let ((result (test-number-lines "x\ny\nz" "[N] " nil)))
    (should (string= result "[1] x\n[2] y\n[3] z"))))

(ert-deftest test-number-lines-no-space-format ()
  "Should number without space."
  (let ((result (test-number-lines "a\nb" "N." nil)))
    (should (string= result "1.a\n2.b"))))

(ert-deftest test-number-lines-custom-format ()
  "Should number with custom format."
  (let ((result (test-number-lines "foo\nbar" "Item N: " nil)))
    (should (string= result "Item 1: foo\nItem 2: bar"))))

;;; Normal Cases - Zero Padding

(ert-deftest test-number-lines-zero-pad-single-digit ()
  "Should not pad when max is single digit."
  (let ((result (test-number-lines "a\nb\nc" "N. " t)))
    (should (string= result "1. a\n2. b\n3. c"))))

(ert-deftest test-number-lines-zero-pad-double-digit ()
  "Should pad to 2 digits when max is 10-99."
  (let* ((lines (make-list 12 "line"))
         (input (mapconcat #'identity lines "\n"))
         (result (test-number-lines input "N. " t))
         (result-lines (split-string result "\n")))
    (should (string-prefix-p "01. " (nth 0 result-lines)))
    (should (string-prefix-p "09. " (nth 8 result-lines)))
    (should (string-prefix-p "10. " (nth 9 result-lines)))
    (should (string-prefix-p "12. " (nth 11 result-lines)))))

(ert-deftest test-number-lines-zero-pad-triple-digit ()
  "Should pad to 3 digits when max is 100+."
  (let* ((lines (make-list 105 "x"))
         (input (mapconcat #'identity lines "\n"))
         (result (test-number-lines input "N. " t))
         (result-lines (split-string result "\n")))
    (should (string-prefix-p "001. " (nth 0 result-lines)))
    (should (string-prefix-p "099. " (nth 98 result-lines)))
    (should (string-prefix-p "100. " (nth 99 result-lines)))
    (should (string-prefix-p "105. " (nth 104 result-lines)))))

;;; Boundary Cases

(ert-deftest test-number-lines-empty-string ()
  "Should handle empty string."
  (let ((result (test-number-lines "" "N. " nil)))
    (should (string= result "1. "))))

(ert-deftest test-number-lines-empty-lines ()
  "Should number empty lines."
  (let ((result (test-number-lines "\n\n" "N. " nil)))
    (should (string= result "1. \n2. \n3. "))))

(ert-deftest test-number-lines-with-existing-numbers ()
  "Should number lines that already have content."
  (let ((result (test-number-lines "1. old\n2. old" "N. " nil)))
    (should (string= result "1. 1. old\n2. 2. old"))))

(ert-deftest test-number-lines-multiple-N-in-format ()
  "Should replace multiple N occurrences."
  (let ((result (test-number-lines "a\nb" "N-N. " nil)))
    (should (string= result "1-1. a\n2-2. b"))))

(ert-deftest test-number-lines-long-content ()
  "Should number lines with long content."
  (let* ((long-line (make-string 100 ?x))
         (input (format "%s\n%s" long-line long-line))
         (result (test-number-lines input "N. " nil)))
    (should (string-prefix-p "1. " result))
    (should (string-match "2\\. " result))))

;;; Normal Cases - No Zero Padding vs Zero Padding

(ert-deftest test-number-lines-comparison-no-pad-vs-pad ()
  "Should show difference between no padding and padding."
  (let* ((input "a\nb\nc\nd\ne\nf\ng\nh\ni\nj")
         (no-pad (test-number-lines input "N. " nil))
         (with-pad (test-number-lines input "N. " t))
         (no-pad-lines (split-string no-pad "\n"))
         (with-pad-lines (split-string with-pad "\n")))
    ;; Without padding: "1. ", "10. "
    (should (string-prefix-p "1. " (nth 0 no-pad-lines)))
    (should (string-prefix-p "10. " (nth 9 no-pad-lines)))
    ;; With padding: "01. ", "10. "
    (should (string-prefix-p "01. " (nth 0 with-pad-lines)))
    (should (string-prefix-p "10. " (nth 9 with-pad-lines)))))

;;; Error Cases

(ert-deftest test-number-lines-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "line1\nline2")
     (cj/--number-lines (point-max) (point-min) "N. " nil))
   :type 'error))

(ert-deftest test-number-lines-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "1. " (cj/--number-lines pos pos "N. " nil))))))

(provide 'test-custom-ordering-number-lines)
;;; test-custom-ordering-number-lines.el ends here
