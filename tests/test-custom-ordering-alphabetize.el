;;; test-custom-ordering-alphabetize.el --- Tests for cj/--alphabetize-region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--alphabetize-region function from custom-ordering.el
;;
;; This function alphabetically sorts words in a region.
;; It splits by whitespace and commas, sorts alphabetically, and joins with ", ".
;;
;; Examples:
;; Input:  "zebra apple banana"
;; Output: "apple, banana, zebra"
;;
;; Input:  "dog, cat, bird"
;; Output: "bird, cat, dog"
;;
;; We test the NON-INTERACTIVE implementation (cj/--alphabetize-region) to avoid
;; mocking region selection. This follows our testing best practice of
;; separating business logic from UI interaction.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-ordering)

;;; Test Helpers

(defun test-alphabetize (input-text)
  "Test cj/--alphabetize-region on INPUT-TEXT.
Returns the sorted, comma-separated string."
  (with-temp-buffer
    (insert input-text)
    (cj/--alphabetize-region (point-min) (point-max))))

;;; Normal Cases - Simple Words

(ert-deftest test-alphabetize-simple-words ()
  "Should alphabetize simple words."
  (let ((result (test-alphabetize "zebra apple banana")))
    (should (string= result "apple, banana, zebra"))))

(ert-deftest test-alphabetize-already-sorted ()
  "Should handle already sorted words."
  (let ((result (test-alphabetize "apple banana cherry")))
    (should (string= result "apple, banana, cherry"))))

(ert-deftest test-alphabetize-reverse-order ()
  "Should alphabetize reverse-ordered words."
  (let ((result (test-alphabetize "zebra yankee xray")))
    (should (string= result "xray, yankee, zebra"))))

(ert-deftest test-alphabetize-two-words ()
  "Should alphabetize two words."
  (let ((result (test-alphabetize "world hello")))
    (should (string= result "hello, world"))))

;;; Normal Cases - With Commas

(ert-deftest test-alphabetize-comma-separated ()
  "Should alphabetize comma-separated words."
  (let ((result (test-alphabetize "dog, cat, bird")))
    (should (string= result "bird, cat, dog"))))

(ert-deftest test-alphabetize-comma-separated-with-spaces ()
  "Should handle comma-separated with various spacing."
  (let ((result (test-alphabetize "dog,cat,bird")))
    (should (string= result "bird, cat, dog"))))

;;; Normal Cases - With Newlines

(ert-deftest test-alphabetize-multiline ()
  "Should alphabetize words across multiple lines."
  (let ((result (test-alphabetize "zebra\napple\nbanana")))
    (should (string= result "apple, banana, zebra"))))

(ert-deftest test-alphabetize-mixed-separators ()
  "Should alphabetize with mixed separators (spaces, commas, newlines)."
  (let ((result (test-alphabetize "zebra, apple\nbanana cherry")))
    (should (string= result "apple, banana, cherry, zebra"))))

;;; Normal Cases - Case Sensitivity

(ert-deftest test-alphabetize-case-sensitive ()
  "Should sort case-sensitively (uppercase before lowercase)."
  (let ((result (test-alphabetize "zebra Apple banana")))
    ;; string-lessp sorts uppercase before lowercase
    (should (string= result "Apple, banana, zebra"))))

(ert-deftest test-alphabetize-mixed-case ()
  "Should handle mixed case words."
  (let ((result (test-alphabetize "ZEBRA apple BANANA")))
    (should (string= result "BANANA, ZEBRA, apple"))))

;;; Normal Cases - Numbers and Special Characters

(ert-deftest test-alphabetize-with-numbers ()
  "Should alphabetize numbers as strings."
  (let ((result (test-alphabetize "10 2 1 20")))
    ;; Alphabetic sort: "1", "10", "2", "20"
    (should (string= result "1, 10, 2, 20"))))

(ert-deftest test-alphabetize-mixed-alphanumeric ()
  "Should alphabetize mixed alphanumeric content."
  (let ((result (test-alphabetize "item2 item1 item10")))
    (should (string= result "item1, item10, item2"))))

(ert-deftest test-alphabetize-with-punctuation ()
  "Should alphabetize words with punctuation."
  (let ((result (test-alphabetize "world! hello? test.")))
    (should (string= result "hello?, test., world!"))))

;;; Boundary Cases

(ert-deftest test-alphabetize-empty-string ()
  "Should handle empty string."
  (let ((result (test-alphabetize "")))
    (should (string= result ""))))

(ert-deftest test-alphabetize-single-word ()
  "Should handle single word."
  (let ((result (test-alphabetize "hello")))
    (should (string= result "hello"))))

(ert-deftest test-alphabetize-only-whitespace ()
  "Should handle whitespace-only text."
  (let ((result (test-alphabetize "   \n\n\t\t   ")))
    (should (string= result ""))))

(ert-deftest test-alphabetize-duplicates ()
  "Should handle duplicate words."
  (let ((result (test-alphabetize "apple banana apple cherry")))
    (should (string= result "apple, apple, banana, cherry"))))

(ert-deftest test-alphabetize-many-commas ()
  "Should handle multiple consecutive commas."
  (let ((result (test-alphabetize "apple,,,banana,,,cherry")))
    (should (string= result "apple, banana, cherry"))))

(ert-deftest test-alphabetize-very-long-list ()
  "Should handle very long list."
  (let* ((words (mapcar (lambda (i) (format "word%03d" i)) (number-sequence 100 1 -1)))
         (input (mapconcat #'identity words " "))
         (result (test-alphabetize input))
         (sorted-words (split-string result ", ")))
    (should (= 100 (length sorted-words)))
    (should (string= "word001" (car sorted-words)))
    (should (string= "word100" (car (last sorted-words))))))

;;; Error Cases

(ert-deftest test-alphabetize-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--alphabetize-region (point-max) (point-min)))
   :type 'error))

(ert-deftest test-alphabetize-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "" (cj/--alphabetize-region pos pos))))))

(provide 'test-custom-ordering-alphabetize)
;;; test-custom-ordering-alphabetize.el ends here
