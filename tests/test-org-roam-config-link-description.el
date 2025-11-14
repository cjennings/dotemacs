;;; test-org-roam-config-link-description.el --- Tests for cj/org-link-get-description -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/org-link-get-description function from org-roam-config.el
;;
;; This function extracts the description from an org link, or returns the text unchanged.
;; If TEXT contains an org link like [[url][description]], it returns description.
;; If TEXT contains multiple links, only the first one is processed.
;; Otherwise it returns TEXT unchanged.
;;
;; Examples:
;; Input:  "[[https://example.com][Example Site]]"
;; Output: "Example Site"
;;
;; Input:  "[[https://example.com]]"
;; Output: "https://example.com"
;;
;; Input:  "Plain text"
;; Output: "Plain text"

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Now load the actual production module
(require 'org-roam-config)

;;; Test Helpers

(defun test-link-description (text)
  "Test cj/org-link-get-description on TEXT.
Returns the extracted description or text unchanged."
  (cj/org-link-get-description text))

;;; Normal Cases - Link with Description

(ert-deftest test-link-with-description ()
  "Should extract description from link with description."
  (let ((result (test-link-description "[[https://example.com][Example Site]]")))
    (should (string= result "Example Site"))))

(ert-deftest test-link-with-multiword-description ()
  "Should extract multi-word description."
  (let ((result (test-link-description "[[url][Multiple Word Description]]")))
    (should (string= result "Multiple Word Description"))))

(ert-deftest test-link-with-special-chars-in-description ()
  "Should extract description with special characters."
  (let ((result (test-link-description "[[url][Description: with, punctuation!]]")))
    (should (string= result "Description: with, punctuation!"))))

(ert-deftest test-link-file-path-with-description ()
  "Should extract description from file link."
  (let ((result (test-link-description "[[file:~/document.pdf][My Document]]")))
    (should (string= result "My Document"))))

(ert-deftest test-link-with-numbers-in-description ()
  "Should extract description containing numbers."
  (let ((result (test-link-description "[[url][Chapter 42]]")))
    (should (string= result "Chapter 42"))))

;;; Normal Cases - Link without Description

(ert-deftest test-link-without-description-url ()
  "Should return URL when no description is present."
  (let ((result (test-link-description "[[https://example.com]]")))
    (should (string= result "https://example.com"))))

(ert-deftest test-link-without-description-file ()
  "Should return file path when no description."
  (let ((result (test-link-description "[[file:~/notes.org]]")))
    (should (string= result "file:~/notes.org"))))

(ert-deftest test-link-without-description-id ()
  "Should return ID when no description."
  (let ((result (test-link-description "[[id:abc123]]")))
    (should (string= result "id:abc123"))))

;;; Normal Cases - No Link

(ert-deftest test-plain-text ()
  "Should return plain text unchanged."
  (let ((result (test-link-description "Plain text without link")))
    (should (string= result "Plain text without link"))))

(ert-deftest test-text-with-brackets-but-not-link ()
  "Should return text with single brackets unchanged."
  (let ((result (test-link-description "Text [with] brackets")))
    (should (string= result "Text [with] brackets"))))

(ert-deftest test-text-with-partial-link-syntax ()
  "Should return text with partial link syntax unchanged."
  (let ((result (test-link-description "[[incomplete link")))
    (should (string= result "[[incomplete link"))))

;;; Boundary Cases - Multiple Links

(ert-deftest test-multiple-links-extracts-first ()
  "Should extract description from first link only."
  (let ((result (test-link-description "[[url1][First]] and [[url2][Second]]")))
    (should (string= result "First"))))

(ert-deftest test-multiple-links-first-has-no-description ()
  "Should extract URL from first link when it has no description."
  (let ((result (test-link-description "[[url1]] and [[url2][Second]]")))
    (should (string= result "url1"))))

;;; Boundary Cases - Empty and Edge Cases

(ert-deftest test-empty-string ()
  "Should return empty string unchanged."
  (let ((result (test-link-description "")))
    (should (string= result ""))))

(ert-deftest test-link-with-empty-description ()
  "Should return text unchanged when description brackets are empty."
  (let ((result (test-link-description "[[https://example.com][]]")))
    ;; Regex requires at least one char in description, so no match
    (should (string= result "[[https://example.com][]]"))))

(ert-deftest test-link-with-empty-url ()
  "Should return text unchanged when link is completely empty."
  (let ((result (test-link-description "[[]]")))
    ;; Regex requires at least one char in URL, so no match, returns unchanged
    (should (string= result "[[]]"))))

(ert-deftest test-link-with-empty-url-and-description ()
  "Should handle completely empty link."
  (let ((result (test-link-description "[][]")))
    (should (string= result "[][]"))))

;;; Edge Cases - Special Link Types

(ert-deftest test-internal-link ()
  "Should extract description from internal link."
  (let ((result (test-link-description "[[*Heading][My Heading]]")))
    (should (string= result "My Heading"))))

(ert-deftest test-internal-link-without-description ()
  "Should return heading target from internal link without description."
  (let ((result (test-link-description "[[*Heading]]")))
    (should (string= result "*Heading"))))

(ert-deftest test-custom-id-link ()
  "Should handle custom ID links."
  (let ((result (test-link-description "[[#custom-id][Custom Section]]")))
    (should (string= result "Custom Section"))))

;;; Edge Cases - Link with Surrounding Text

(ert-deftest test-link-with-prefix-text ()
  "Should extract description from link with prefix text."
  (let ((result (test-link-description "See [[url][documentation]] for details")))
    (should (string= result "documentation"))))

(ert-deftest test-link-at-start ()
  "Should extract description from link at start of text."
  (let ((result (test-link-description "[[url][Link]] at beginning")))
    (should (string= result "Link"))))

(ert-deftest test-link-at-end ()
  "Should extract description from link at end of text."
  (let ((result (test-link-description "Text with [[url][link]]")))
    (should (string= result "link"))))

;;; Edge Cases - Special Characters in URL

(ert-deftest test-link-with-query-params ()
  "Should handle URL with query parameters."
  (let ((result (test-link-description "[[https://example.com?q=test&foo=bar][Search]]")))
    (should (string= result "Search"))))

(ert-deftest test-link-with-anchor ()
  "Should handle URL with anchor."
  (let ((result (test-link-description "[[https://example.com#section][Section]]")))
    (should (string= result "Section"))))

(ert-deftest test-link-with-spaces-in-description ()
  "Should preserve spaces in description."
  (let ((result (test-link-description "[[url][Multiple   Spaces]]")))
    (should (string= result "Multiple   Spaces"))))

(provide 'test-org-roam-config-link-description)
;;; test-org-roam-config-link-description.el ends here
