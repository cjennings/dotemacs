;;; test-custom-misc-count-words.el --- Tests for cj/--count-words -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--count-words function from custom-misc.el
;;
;; This function counts words in a region using Emacs's built-in count-words.
;; A word is defined by Emacs's word boundaries, which generally means
;; sequences of word-constituent characters separated by whitespace or punctuation.
;;
;; We test the NON-INTERACTIVE implementation (cj/--count-words) to avoid
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
(require 'custom-misc)

;;; Test Helpers

(defun test-count-words (input-text)
  "Test cj/--count-words on INPUT-TEXT.
Returns the word count."
  (with-temp-buffer
    (insert input-text)
    (cj/--count-words (point-min) (point-max))))

;;; Normal Cases

(ert-deftest test-count-words-multiple-words ()
  "Should count multiple words."
  (should (= 5 (test-count-words "The quick brown fox jumps"))))

(ert-deftest test-count-words-single-word ()
  "Should count single word."
  (should (= 1 (test-count-words "hello"))))

(ert-deftest test-count-words-with-punctuation ()
  "Should count words with punctuation."
  (should (= 5 (test-count-words "Hello, world! How are you?"))))

(ert-deftest test-count-words-multiple-spaces ()
  "Should count words separated by multiple spaces."
  (should (= 3 (test-count-words "hello    world    test"))))

(ert-deftest test-count-words-with-newlines ()
  "Should count words across newlines."
  (should (= 6 (test-count-words "line one\nline two\nline three"))))

(ert-deftest test-count-words-with-tabs ()
  "Should count words separated by tabs."
  (should (= 3 (test-count-words "hello\tworld\ttest"))))

(ert-deftest test-count-words-mixed-whitespace ()
  "Should count words with mixed whitespace."
  (should (= 4 (test-count-words "hello \t world\n\ntest  end"))))

(ert-deftest test-count-words-hyphenated ()
  "Should count hyphenated words."
  ;; Emacs treats hyphens as word separators in count-words
  (should (= 7 (test-count-words "This is state-of-the-art technology"))))

(ert-deftest test-count-words-contractions ()
  "Should count contractions."
  ;; Emacs treats apostrophes as word separators in count-words
  (should (= 6 (test-count-words "don't can't won't"))))

(ert-deftest test-count-words-numbers ()
  "Should count numbers as words."
  (should (= 6 (test-count-words "The year 2024 has 365 days"))))

;;; Boundary Cases

(ert-deftest test-count-words-empty-string ()
  "Should return 0 for empty string."
  (should (= 0 (test-count-words ""))))

(ert-deftest test-count-words-only-whitespace ()
  "Should return 0 for whitespace-only text."
  (should (= 0 (test-count-words "   \t\n\n   "))))

(ert-deftest test-count-words-only-punctuation ()
  "Should count punctuation-only text."
  ;; Emacs may count consecutive punctuation as a word
  (should (= 1 (test-count-words "!@#$%^&*()"))))

(ert-deftest test-count-words-leading-trailing-spaces ()
  "Should count words ignoring leading/trailing spaces."
  (should (= 3 (test-count-words "   hello world test   "))))

(ert-deftest test-count-words-unicode ()
  "Should count Unicode words."
  (should (= 3 (test-count-words "café résumé naïve"))))

(ert-deftest test-count-words-very-long-text ()
  "Should handle very long text."
  (let ((long-text (mapconcat (lambda (_) "word") (make-list 1000 nil) " ")))
    (should (= 1000 (test-count-words long-text)))))

(ert-deftest test-count-words-multiline-paragraph ()
  "Should count words in multi-line paragraph."
  (let ((text "This is a paragraph
that spans multiple
lines with various
words in it."))
    (should (= 13 (test-count-words text)))))

;;; Error Cases

(ert-deftest test-count-words-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--count-words (point-max) (point-min)))
   :type 'error))

(ert-deftest test-count-words-empty-region ()
  "Should return 0 for empty region (start == end)."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (= 0 (cj/--count-words pos pos))))))

(ert-deftest test-count-words-partial-region ()
  "Should count words only in specified region."
  (with-temp-buffer
    (insert "one two three four five")
    ;; Count only "two three four" (positions roughly in middle)
    (goto-char (point-min))
    (search-forward "two")
    (let ((start (match-beginning 0)))
      (search-forward "four")
      (let ((end (match-end 0)))
        (should (= 3 (cj/--count-words start end)))))))

(provide 'test-custom-misc-count-words)
;;; test-custom-misc-count-words.el ends here
