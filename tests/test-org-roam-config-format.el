;;; test-org-roam-config-format.el --- Tests for cj/--format-roam-node -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--format-roam-node function from org-roam-config.el
;;
;; This function formats org-roam node file content with title, node-id, and body content.
;; It creates a complete org-roam file with properties, title, category, and filetags.
;;
;; Example:
;; Input:  title: "My Note", node-id: "abc123", content: "* Content\n"
;; Output: Full org-roam file with metadata and content

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Now load the actual production module
(require 'org-roam-config)

;;; Test Helpers

(defun test-format (title node-id content)
  "Test cj/--format-roam-node with TITLE, NODE-ID, and CONTENT.
Returns the formatted file content."
  (cj/--format-roam-node title node-id content))

;;; Normal Cases

(ert-deftest test-format-simple-node ()
  "Should format simple node with all components."
  (let ((result (test-format "Test Title" "id-123" "* Content\n")))
    (should (string-match-p ":PROPERTIES:" result))
    (should (string-match-p ":ID:       id-123" result))
    (should (string-match-p "#\\+TITLE: Test Title" result))
    (should (string-match-p "#\\+CATEGORY: Test Title" result))
    (should (string-match-p "#\\+FILETAGS: Topic" result))
    (should (string-match-p "\\* Content" result))))

(ert-deftest test-format-properties-first ()
  "Should place properties at the beginning."
  (let ((result (test-format "Title" "id" "content")))
    (should (string-prefix-p ":PROPERTIES:\n" result))))

(ert-deftest test-format-id-after-properties ()
  "Should place ID in properties block."
  (let ((result (test-format "Title" "test-id-456" "content")))
    (should (string-match-p ":PROPERTIES:\n:ID:       test-id-456\n:END:" result))))

(ert-deftest test-format-title-after-properties ()
  "Should place title after properties."
  (let ((result (test-format "My Title" "id" "content")))
    (should (string-match-p ":END:\n#\\+TITLE: My Title\n" result))))

(ert-deftest test-format-category-matches-title ()
  "Should set category to match title."
  (let ((result (test-format "Project Name" "id" "content")))
    (should (string-match-p "#\\+TITLE: Project Name\n#\\+CATEGORY: Project Name\n" result))))

(ert-deftest test-format-filetags-topic ()
  "Should set filetags to Topic."
  (let ((result (test-format "Title" "id" "content")))
    (should (string-match-p "#\\+FILETAGS: Topic\n" result))))

(ert-deftest test-format-content-at-end ()
  "Should place content after metadata."
  (let ((result (test-format "Title" "id" "* Heading\nBody text\n")))
    (should (string-suffix-p "* Heading\nBody text\n" result))))

;;; Edge Cases - Various Titles

(ert-deftest test-format-title-with-spaces ()
  "Should handle title with spaces."
  (let ((result (test-format "Multi Word Title" "id" "content")))
    (should (string-match-p "#\\+TITLE: Multi Word Title" result))
    (should (string-match-p "#\\+CATEGORY: Multi Word Title" result))))

(ert-deftest test-format-title-with-punctuation ()
  "Should handle title with punctuation."
  (let ((result (test-format "Title: With, Punctuation!" "id" "content")))
    (should (string-match-p "#\\+TITLE: Title: With, Punctuation!" result))))

(ert-deftest test-format-title-with-numbers ()
  "Should handle title with numbers."
  (let ((result (test-format "Version 2.0" "id" "content")))
    (should (string-match-p "#\\+TITLE: Version 2\\.0" result))))

;;; Edge Cases - Various Node IDs

(ert-deftest test-format-uuid-style-id ()
  "Should handle UUID-style ID."
  (let ((result (test-format "Title" "a1b2c3d4-e5f6-7890-abcd-ef1234567890" "content")))
    (should (string-match-p ":ID:       a1b2c3d4-e5f6-7890-abcd-ef1234567890" result))))

(ert-deftest test-format-short-id ()
  "Should handle short ID."
  (let ((result (test-format "Title" "1" "content")))
    (should (string-match-p ":ID:       1" result))))

(ert-deftest test-format-long-id ()
  "Should handle long ID."
  (let* ((long-id (make-string 100 ?a))
         (result (test-format "Title" long-id "content")))
    (should (string-match-p (concat ":ID:       " long-id) result))))

;;; Edge Cases - Various Content

(ert-deftest test-format-empty-content ()
  "Should handle empty content."
  (let ((result (test-format "Title" "id" "")))
    (should (string-suffix-p "#+FILETAGS: Topic\n\n" result))))

(ert-deftest test-format-multiline-content ()
  "Should handle multiline content."
  (let ((result (test-format "Title" "id" "* H1\nText\n** H2\nMore\n")))
    (should (string-suffix-p "* H1\nText\n** H2\nMore\n" result))))

(ert-deftest test-format-content-with-properties ()
  "Should handle content that already has properties."
  (let ((result (test-format "Title" "id" "* Heading\n:PROPERTIES:\n:CUSTOM: value\n:END:\n")))
    (should (string-match-p ":CUSTOM: value" result))))

;;; Integration Tests - Structure

(ert-deftest test-format-complete-structure ()
  "Should create proper org-roam file structure."
  (let ((result (test-format "My Note" "abc-123" "* Content\n")))
    ;; Check order of components
    (should (< (string-match ":PROPERTIES:" result)
               (string-match ":ID:" result)))
    (should (< (string-match ":ID:" result)
               (string-match ":END:" result)))
    (should (< (string-match ":END:" result)
               (string-match "#\\+TITLE:" result)))
    (should (< (string-match "#\\+TITLE:" result)
               (string-match "#\\+CATEGORY:" result)))
    (should (< (string-match "#\\+CATEGORY:" result)
               (string-match "#\\+FILETAGS:" result)))
    (should (< (string-match "#\\+FILETAGS:" result)
               (string-match "\\* Content" result)))))

(ert-deftest test-format-double-newline-after-metadata ()
  "Should have double newline between metadata and content."
  (let ((result (test-format "Title" "id" "* Content")))
    (should (string-match-p "#\\+FILETAGS: Topic\n\n\\* Content" result))))

(provide 'test-org-roam-config-format)
;;; test-org-roam-config-format.el ends here
