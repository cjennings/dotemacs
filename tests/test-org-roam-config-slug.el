;;; test-org-roam-config-slug.el --- Tests for cj/--generate-roam-slug -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--generate-roam-slug function from org-roam-config.el
;;
;; This function converts a title to a filename-safe slug by:
;; 1. Converting to lowercase
;; 2. Replacing non-alphanumeric characters with hyphens
;; 3. Removing leading and trailing hyphens
;;
;; Examples:
;; Input:  "My Project Name"
;; Output: "my-project-name"
;;
;; Input:  "Hello, World!"
;; Output: "hello-world"

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Now load the actual production module
(require 'org-roam-config)

;;; Test Helpers

(defun test-slug (title)
  "Test cj/--generate-roam-slug on TITLE.
Returns the slugified string."
  (cj/--generate-roam-slug title))

;;; Normal Cases - Simple Titles

(ert-deftest test-slug-simple-word ()
  "Should return lowercase simple word."
  (let ((result (test-slug "Hello")))
    (should (string= result "hello"))))

(ert-deftest test-slug-multiple-words ()
  "Should replace spaces with hyphens."
  (let ((result (test-slug "My Project Name")))
    (should (string= result "my-project-name"))))

(ert-deftest test-slug-already-lowercase ()
  "Should handle already lowercase text."
  (let ((result (test-slug "simple")))
    (should (string= result "simple"))))

(ert-deftest test-slug-mixed-case ()
  "Should convert mixed case to lowercase."
  (let ((result (test-slug "MixedCaseTitle")))
    (should (string= result "mixedcasetitle"))))

;;; Normal Cases - Punctuation

(ert-deftest test-slug-with-comma ()
  "Should remove commas."
  (let ((result (test-slug "Hello, World")))
    (should (string= result "hello-world"))))

(ert-deftest test-slug-with-period ()
  "Should remove periods."
  (let ((result (test-slug "Version 2.0")))
    (should (string= result "version-2-0"))))

(ert-deftest test-slug-with-exclamation ()
  "Should remove exclamation marks."
  (let ((result (test-slug "Hello World!")))
    (should (string= result "hello-world"))))

(ert-deftest test-slug-with-question ()
  "Should remove question marks."
  (let ((result (test-slug "What Is This?")))
    (should (string= result "what-is-this"))))

(ert-deftest test-slug-with-colon ()
  "Should remove colons."
  (let ((result (test-slug "Note: Important")))
    (should (string= result "note-important"))))

(ert-deftest test-slug-with-parentheses ()
  "Should remove parentheses."
  (let ((result (test-slug "Item (copy)")))
    (should (string= result "item-copy"))))

;;; Normal Cases - Numbers

(ert-deftest test-slug-with-numbers ()
  "Should preserve numbers."
  (let ((result (test-slug "Chapter 42")))
    (should (string= result "chapter-42"))))

(ert-deftest test-slug-only-numbers ()
  "Should handle titles with only numbers."
  (let ((result (test-slug "123")))
    (should (string= result "123"))))

(ert-deftest test-slug-mixed-alphanumeric ()
  "Should preserve alphanumeric characters."
  (let ((result (test-slug "Test123ABC")))
    (should (string= result "test123abc"))))

;;; Boundary Cases - Multiple Consecutive Special Chars

(ert-deftest test-slug-multiple-spaces ()
  "Should collapse multiple spaces into single hyphen."
  (let ((result (test-slug "Hello    World")))
    (should (string= result "hello-world"))))

(ert-deftest test-slug-mixed-punctuation ()
  "Should collapse mixed punctuation into single hyphen."
  (let ((result (test-slug "Hello, ... World!")))
    (should (string= result "hello-world"))))

(ert-deftest test-slug-consecutive-hyphens ()
  "Should collapse consecutive hyphens."
  (let ((result (test-slug "Hello---World")))
    (should (string= result "hello-world"))))

;;; Boundary Cases - Leading/Trailing Special Chars

(ert-deftest test-slug-leading-space ()
  "Should remove leading hyphen from leading space."
  (let ((result (test-slug "  Hello")))
    (should (string= result "hello"))))

(ert-deftest test-slug-trailing-space ()
  "Should remove trailing hyphen from trailing space."
  (let ((result (test-slug "Hello  ")))
    (should (string= result "hello"))))

(ert-deftest test-slug-leading-punctuation ()
  "Should remove leading hyphen from leading punctuation."
  (let ((result (test-slug "...Hello")))
    (should (string= result "hello"))))

(ert-deftest test-slug-trailing-punctuation ()
  "Should remove trailing hyphen from trailing punctuation."
  (let ((result (test-slug "Hello!!!")))
    (should (string= result "hello"))))

(ert-deftest test-slug-leading-and-trailing ()
  "Should remove both leading and trailing hyphens."
  (let ((result (test-slug "  Hello World  ")))
    (should (string= result "hello-world"))))

;;; Boundary Cases - Empty and Short

(ert-deftest test-slug-empty-string ()
  "Should return empty string for empty input."
  (let ((result (test-slug "")))
    (should (string= result ""))))

(ert-deftest test-slug-only-punctuation ()
  "Should return empty string for only punctuation."
  (let ((result (test-slug "!!!")))
    (should (string= result ""))))

(ert-deftest test-slug-only-spaces ()
  "Should return empty string for only spaces."
  (let ((result (test-slug "   ")))
    (should (string= result ""))))

(ert-deftest test-slug-single-char ()
  "Should handle single character."
  (let ((result (test-slug "A")))
    (should (string= result "a"))))

;;; Edge Cases - Special Characters

(ert-deftest test-slug-with-underscore ()
  "Should replace underscores with hyphens."
  (let ((result (test-slug "my_variable_name")))
    (should (string= result "my-variable-name"))))

(ert-deftest test-slug-with-slash ()
  "Should remove slashes."
  (let ((result (test-slug "path/to/file")))
    (should (string= result "path-to-file"))))

(ert-deftest test-slug-with-at-sign ()
  "Should remove at signs."
  (let ((result (test-slug "user@example")))
    (should (string= result "user-example"))))

(ert-deftest test-slug-with-hash ()
  "Should remove hash symbols."
  (let ((result (test-slug "#hashtag")))
    (should (string= result "hashtag"))))

(ert-deftest test-slug-with-dollar ()
  "Should remove dollar signs."
  (let ((result (test-slug "$price")))
    (should (string= result "price"))))

;;; Edge Cases - Unicode (if supported)

(ert-deftest test-slug-with-unicode ()
  "Should remove unicode characters."
  (let ((result (test-slug "Café")))
    (should (string= result "caf"))))

(ert-deftest test-slug-with-emoji ()
  "Should remove emoji."
  (let ((result (test-slug "Hello 😀 World")))
    (should (string= result "hello-world"))))

;;; Edge Cases - Long Titles

(ert-deftest test-slug-very-long-title ()
  "Should handle very long titles."
  (let* ((long-title (mapconcat #'identity (make-list 20 "word") " "))
         (result (test-slug long-title)))
    (should (string-prefix-p "word-" result))
    (should (string-suffix-p "-word" result))
    (should (not (string-match-p " " result)))))

(provide 'test-org-roam-config-slug)
;;; test-org-roam-config-slug.el ends here
