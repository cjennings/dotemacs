;;; test-org-noter--generate-notes-template.el --- Tests for cj/org-noter--generate-notes-template -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the generate-notes-template function used in org-noter workflow.
;; Tests cover normal, boundary, and error cases.

;;; Code:

(require 'ert)
(require 'org-noter-config)

;;; Test Helpers

(defun test-org-noter--template-has-property (template property value)
  "Check if TEMPLATE contains PROPERTY with VALUE in properties drawer."
  (string-match-p (format ":%s: %s" property (regexp-quote value)) template))

(defun test-org-noter--template-has-keyword (template keyword value)
  "Check if TEMPLATE contains #+KEYWORD: VALUE."
  (string-match-p (format "#\\+%s: %s" keyword (regexp-quote value)) template))

;;; Normal Cases

(ert-deftest test-org-noter--generate-notes-template-normal-basic ()
  "Normal case: Basic template generation."
  (let ((template (cj/org-noter--generate-notes-template "Test Book" "/path/to/book.pdf")))
    (should (stringp template))
    (should (string-match-p ":PROPERTIES:" template))
    (should (string-match-p ":END:" template))
    (should (string-match-p "\\* Notes" template))))

(ert-deftest test-org-noter--generate-notes-template-normal-has-id ()
  "Normal case: Template has ID property."
  (let ((template (cj/org-noter--generate-notes-template "Test Book" "/path/to/book.pdf")))
    (should (string-match-p ":ID: [a-f0-9-]+" template))))

(ert-deftest test-org-noter--generate-notes-template-normal-has-noter-document ()
  "Normal case: Template has NOTER_DOCUMENT property."
  (let ((template (cj/org-noter--generate-notes-template "Test Book" "/path/to/book.pdf")))
    (should (test-org-noter--template-has-property template "NOTER_DOCUMENT" "/path/to/book.pdf"))))

(ert-deftest test-org-noter--generate-notes-template-normal-has-roam-refs ()
  "Normal case: Template has ROAM_REFS property."
  (let ((template (cj/org-noter--generate-notes-template "Test Book" "/path/to/book.pdf")))
    (should (test-org-noter--template-has-property template "ROAM_REFS" "/path/to/book.pdf"))))

(ert-deftest test-org-noter--generate-notes-template-normal-has-title ()
  "Normal case: Template has title with book name."
  (let ((template (cj/org-noter--generate-notes-template "The Great Gatsby" "/books/gatsby.epub")))
    (should (test-org-noter--template-has-keyword template "title" "Notes on The Great Gatsby"))))

(ert-deftest test-org-noter--generate-notes-template-normal-has-filetags ()
  "Normal case: Template has ReadingNotes filetag."
  (let ((template (cj/org-noter--generate-notes-template "Test Book" "/path/to/book.pdf")))
    (should (test-org-noter--template-has-keyword template "FILETAGS" ":ReadingNotes:"))))

(ert-deftest test-org-noter--generate-notes-template-normal-has-category ()
  "Normal case: Template has CATEGORY set to book title."
  (let ((template (cj/org-noter--generate-notes-template "Clean Code" "/books/clean-code.pdf")))
    (should (test-org-noter--template-has-keyword template "CATEGORY" "Clean Code"))))

;;; Boundary Cases

(ert-deftest test-org-noter--generate-notes-template-boundary-long-title ()
  "Boundary case: Very long title."
  (let* ((long-title "This Is An Incredibly Long Book Title That Goes On And On")
         (template (cj/org-noter--generate-notes-template long-title "/books/long.pdf")))
    (should (test-org-noter--template-has-keyword template "title" (format "Notes on %s" long-title)))
    (should (test-org-noter--template-has-keyword template "CATEGORY" long-title))))

(ert-deftest test-org-noter--generate-notes-template-boundary-special-chars-in-title ()
  "Boundary case: Special characters in title."
  (let ((template (cj/org-noter--generate-notes-template "C++: A Guide" "/books/cpp.pdf")))
    (should (test-org-noter--template-has-keyword template "title" "Notes on C++: A Guide"))))

(ert-deftest test-org-noter--generate-notes-template-boundary-special-chars-in-path ()
  "Boundary case: Special characters in path."
  (let ((template (cj/org-noter--generate-notes-template "Test" "/path/with spaces/book.pdf")))
    (should (test-org-noter--template-has-property template "NOTER_DOCUMENT" "/path/with spaces/book.pdf"))))

(ert-deftest test-org-noter--generate-notes-template-boundary-epub-path ()
  "Boundary case: EPUB file path."
  (let ((template (cj/org-noter--generate-notes-template "Novel" "/library/novel.epub")))
    (should (test-org-noter--template-has-property template "NOTER_DOCUMENT" "/library/novel.epub"))))

;;; Structure Tests

(ert-deftest test-org-noter--generate-notes-template-structure-properties-first ()
  "Structure: Properties drawer comes first."
  (let ((template (cj/org-noter--generate-notes-template "Test" "/path.pdf")))
    (should (string-match "\\`:PROPERTIES:" template))))

(ert-deftest test-org-noter--generate-notes-template-structure-notes-heading ()
  "Structure: Has Notes heading for content."
  (let ((template (cj/org-noter--generate-notes-template "Test" "/path.pdf")))
    (should (string-match-p "^\\* Notes$" template))))

(ert-deftest test-org-noter--generate-notes-template-structure-unique-ids ()
  "Structure: Each call generates unique ID."
  (let ((template1 (cj/org-noter--generate-notes-template "Test1" "/path1.pdf"))
        (template2 (cj/org-noter--generate-notes-template "Test2" "/path2.pdf")))
    (string-match ":ID: \\([a-f0-9-]+\\)" template1)
    (let ((id1 (match-string 1 template1)))
      (string-match ":ID: \\([a-f0-9-]+\\)" template2)
      (let ((id2 (match-string 1 template2)))
        (should-not (equal id1 id2))))))

(provide 'test-org-noter--generate-notes-template)
;;; test-org-noter--generate-notes-template.el ends here
