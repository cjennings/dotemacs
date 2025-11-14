;;; test-org-webclipper-process.el --- Tests for cj/--process-webclip-content -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--process-webclip-content function from org-webclipper.el
;;
;; This function processes webclipped org-mode content by:
;; 1. Removing the first top-level heading
;; 2. Removing any initial blank lines
;; 3. Demoting all remaining headings by one level
;;
;; Examples:
;; Input:  "* Title\nContent\n** Sub\n"
;; Output: "Content\n*** Sub\n"
;;
;; Input:  "* Title\n\n\n** Sub\n"
;; Output: "*** Sub\n"

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Now load the actual production module
(require 'org-webclipper)

;;; Test Helpers

(defun test-process-webclip (content)
  "Test cj/--process-webclip-content on CONTENT.
Returns the processed content."
  (cj/--process-webclip-content content))

;;; Normal Cases - Single Heading Removal

(ert-deftest test-process-removes-first-heading ()
  "Should remove the first top-level heading."
  (let ((result (test-process-webclip "* Title\nContent\n")))
    (should (string= result "Content\n"))))

(ert-deftest test-process-removes-heading-with-text ()
  "Should remove first heading preserving body text."
  (let ((result (test-process-webclip "* Page Title\nParagraph text\n")))
    (should (string= result "Paragraph text\n"))))

(ert-deftest test-process-removes-heading-with-tags ()
  "Should remove first heading even with tags."
  (let ((result (test-process-webclip "* Title :tag1:tag2:\nContent\n")))
    (should (string= result "Content\n"))))

(ert-deftest test-process-removes-heading-with-todo ()
  "Should remove first heading even with TODO keyword."
  (let ((result (test-process-webclip "* TODO Task\nContent\n")))
    (should (string= result "Content\n"))))

;;; Normal Cases - Blank Line Removal

(ert-deftest test-process-removes-single-blank-line ()
  "Should remove single blank line after heading removal."
  (let ((result (test-process-webclip "* Title\n\nContent\n")))
    (should (string= result "Content\n"))))

(ert-deftest test-process-removes-multiple-blank-lines ()
  "Should remove multiple blank lines after heading removal."
  (let ((result (test-process-webclip "* Title\n\n\n\nContent\n")))
    (should (string= result "Content\n"))))

(ert-deftest test-process-removes-blank-lines-with-spaces ()
  "Should remove blank lines that contain only spaces."
  (let ((result (test-process-webclip "* Title\n  \n\t\nContent\n")))
    (should (string= result "Content\n"))))

(ert-deftest test-process-preserves-blank-lines-in-content ()
  "Should preserve blank lines within the content."
  (let ((result (test-process-webclip "* Title\nPara 1\n\nPara 2\n")))
    (should (string= result "Para 1\n\nPara 2\n"))))

;;; Normal Cases - Heading Demotion

(ert-deftest test-process-demotes-second-level ()
  "Should demote level 2 heading to level 3."
  (let ((result (test-process-webclip "* Title\n** Section\n")))
    (should (string= result "*** Section\n"))))

(ert-deftest test-process-demotes-third-level ()
  "Should demote level 3 heading to level 4."
  (let ((result (test-process-webclip "* Title\n*** Subsection\n")))
    (should (string= result "**** Subsection\n"))))

(ert-deftest test-process-demotes-multiple-headings ()
  "Should demote all headings in the content."
  (let ((result (test-process-webclip "* Title\n** Section 1\n** Section 2\n")))
    (should (string= result "*** Section 1\n*** Section 2\n"))))

(ert-deftest test-process-demotes-nested-hierarchy ()
  "Should demote nested heading structure."
  (let ((result (test-process-webclip "* Title\n** Section\n*** Subsection\n")))
    (should (string= result "*** Section\n**** Subsection\n"))))

;;; Normal Cases - Combined Processing

(ert-deftest test-process-full-workflow ()
  "Should remove heading, blank lines, and demote remaining headings."
  (let ((result (test-process-webclip "* Article Title\n\n** Introduction\nText\n** Conclusion\n")))
    (should (string= result "*** Introduction\nText\n*** Conclusion\n"))))

(ert-deftest test-process-with-properties ()
  "Should preserve properties in demoted headings."
  (let ((result (test-process-webclip "* Title\n** Heading\n:PROPERTIES:\n:ID: 123\n:END:\n")))
    (should (string= result "*** Heading\n:PROPERTIES:\n:ID: 123\n:END:\n"))))

(ert-deftest test-process-with-mixed-content ()
  "Should handle mixed text and headings."
  (let ((result (test-process-webclip "* Title\nIntro text\n** Section\nBody text\n")))
    (should (string= result "Intro text\n*** Section\nBody text\n"))))

;;; Edge Cases - Empty and Minimal Content

(ert-deftest test-process-empty-string ()
  "Should return empty string for empty input."
  (let ((result (test-process-webclip "")))
    (should (string= result ""))))

(ert-deftest test-process-only-heading ()
  "Should return empty string when only first heading present."
  (let ((result (test-process-webclip "* Title\n")))
    (should (string= result ""))))

(ert-deftest test-process-only-blank-lines ()
  "Should return empty string for only blank lines after heading."
  (let ((result (test-process-webclip "* Title\n\n\n")))
    (should (string= result ""))))

(ert-deftest test-process-no-heading ()
  "Should handle content without any heading."
  (let ((result (test-process-webclip "Just plain text\n")))
    (should (string= result "Just plain text\n"))))

(ert-deftest test-process-heading-no-newline ()
  "Should demote heading without trailing newline (doesn't match removal pattern)."
  (let ((result (test-process-webclip "* Title")))
    (should (string= result "** Title"))))

;;; Edge Cases - Heading Variations

(ert-deftest test-process-heading-without-space ()
  "Should not match heading without space after stars."
  (let ((result (test-process-webclip "*Title\nContent\n")))
    (should (string= result "*Title\nContent\n"))))

(ert-deftest test-process-multiple-top-level-headings ()
  "Should only remove first top-level heading."
  (let ((result (test-process-webclip "* Title 1\n* Title 2\n")))
    (should (string= result "** Title 2\n"))))

(ert-deftest test-process-heading-with-priority ()
  "Should remove heading with priority marker."
  (let ((result (test-process-webclip "* [#A] Important\nContent\n")))
    (should (string= result "Content\n"))))

(ert-deftest test-process-heading-with-links ()
  "Should remove heading containing links."
  (let ((result (test-process-webclip "* [[url][Link Title]]\nContent\n")))
    (should (string= result "Content\n"))))

;;; Edge Cases - Special Content

(ert-deftest test-process-preserves-lists ()
  "Should preserve list formatting."
  (let ((result (test-process-webclip "* Title\n- Item 1\n- Item 2\n")))
    (should (string= result "- Item 1\n- Item 2\n"))))

(ert-deftest test-process-preserves-code-blocks ()
  "Should preserve code block content."
  (let ((result (test-process-webclip "* Title\n#+BEGIN_SRC python\nprint('hi')\n#+END_SRC\n")))
    (should (string= result "#+BEGIN_SRC python\nprint('hi')\n#+END_SRC\n"))))

(ert-deftest test-process-preserves-tables ()
  "Should preserve org table content."
  (let ((result (test-process-webclip "* Title\n| A | B |\n| 1 | 2 |\n")))
    (should (string= result "| A | B |\n| 1 | 2 |\n"))))

;;; Edge Cases - Deep Nesting

(ert-deftest test-process-very-deep-headings ()
  "Should demote very deep heading structures."
  (let ((result (test-process-webclip "* Title\n****** Level 6\n")))
    (should (string= result "******* Level 6\n"))))

(ert-deftest test-process-complex-document ()
  "Should handle complex document structure."
  (let ((result (test-process-webclip "* Main Title\n\n** Section 1\nText 1\n*** Subsection 1.1\nText 2\n** Section 2\nText 3\n")))
    (should (string= result "*** Section 1\nText 1\n**** Subsection 1.1\nText 2\n*** Section 2\nText 3\n"))))

;;; Integration Tests

(ert-deftest test-process-realistic-webpage ()
  "Should process realistic webclipped content."
  (let ((result (test-process-webclip "* How to Program in Emacs Lisp\n\n** Introduction\nEmacs Lisp is powerful.\n\n** Getting Started\nFirst, open Emacs.\n\n*** Installation\nDownload from gnu.org\n")))
    (should (string= result "*** Introduction\nEmacs Lisp is powerful.\n\n*** Getting Started\nFirst, open Emacs.\n\n**** Installation\nDownload from gnu.org\n"))))

(ert-deftest test-process-article-with-metadata ()
  "Should handle article with org metadata."
  (let ((result (test-process-webclip "* Article Title :article:web:\n#+DATE: 2024-01-01\n\n** Content\nBody text\n")))
    (should (string= result "#+DATE: 2024-01-01\n\n*** Content\nBody text\n"))))

(provide 'test-org-webclipper-process)
;;; test-org-webclipper-process.el ends here
