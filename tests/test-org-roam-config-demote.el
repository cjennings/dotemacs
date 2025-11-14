;;; test-org-roam-config-demote.el --- Tests for cj/--demote-org-subtree -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--demote-org-subtree function from org-roam-config.el
;;
;; This function demotes org subtree content from one level to another.
;; All headings in the tree are adjusted proportionally, with a minimum level of 1.
;;
;; Examples:
;; Input:  "*** Heading\n**** Sub", from: 3, to: 1
;; Output: "* Heading\n** Sub"
;;
;; Input:  "** Heading\n*** Sub", from: 2, to: 1
;; Output: "* Heading\n** Sub"

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Now load the actual production module
(require 'org-roam-config)

;;; Test Helpers

(defun test-demote (content from-level to-level)
  "Test cj/--demote-org-subtree on CONTENT.
FROM-LEVEL is the current top level, TO-LEVEL is the desired top level.
Returns the demoted content."
  (cj/--demote-org-subtree content from-level to-level))

;;; Normal Cases - Single Heading

(ert-deftest test-demote-level2-to-level1 ()
  "Should demote level 2 heading to level 1."
  (let ((result (test-demote "** Heading\n" 2 1)))
    (should (string= result "* Heading\n"))))

(ert-deftest test-demote-level3-to-level1 ()
  "Should demote level 3 heading to level 1."
  (let ((result (test-demote "*** Heading\n" 3 1)))
    (should (string= result "* Heading\n"))))

(ert-deftest test-demote-level4-to-level1 ()
  "Should demote level 4 heading to level 1."
  (let ((result (test-demote "**** Heading\n" 4 1)))
    (should (string= result "* Heading\n"))))

(ert-deftest test-demote-level3-to-level2 ()
  "Should demote level 3 heading to level 2."
  (let ((result (test-demote "*** Heading\n" 3 2)))
    (should (string= result "** Heading\n"))))

;;; Normal Cases - Multiple Headings at Same Level

(ert-deftest test-demote-multiple-same-level ()
  "Should demote multiple headings at same level."
  (let ((result (test-demote "** First\n** Second\n** Third\n" 2 1)))
    (should (string= result "* First\n* Second\n* Third\n"))))

;;; Normal Cases - Hierarchical Structure

(ert-deftest test-demote-with-subheading ()
  "Should demote heading and subheading proportionally."
  (let ((result (test-demote "** Heading\n*** Subheading\n" 2 1)))
    (should (string= result "* Heading\n** Subheading\n"))))

(ert-deftest test-demote-three-levels ()
  "Should demote three-level hierarchy."
  (let ((result (test-demote "** Main\n*** Sub\n**** SubSub\n" 2 1)))
    (should (string= result "* Main\n** Sub\n*** SubSub\n"))))

(ert-deftest test-demote-complex-hierarchy ()
  "Should demote complex hierarchy maintaining relative structure."
  (let ((result (test-demote "*** Top\n**** Sub1\n***** Deep\n**** Sub2\n" 3 1)))
    (should (string= result "* Top\n** Sub1\n*** Deep\n** Sub2\n"))))

;;; Normal Cases - With Content

(ert-deftest test-demote-heading-with-text ()
  "Should demote heading preserving body text."
  (let ((result (test-demote "** Heading\nBody text\n" 2 1)))
    (should (string= result "* Heading\nBody text\n"))))

(ert-deftest test-demote-with-properties ()
  "Should demote heading preserving properties."
  (let ((result (test-demote "** Heading\n:PROPERTIES:\n:ID: 123\n:END:\n" 2 1)))
    (should (string= result "* Heading\n:PROPERTIES:\n:ID: 123\n:END:\n"))))

(ert-deftest test-demote-with-mixed-content ()
  "Should demote headings preserving all content."
  (let ((result (test-demote "** H1\nText\n*** H2\nMore text\n" 2 1)))
    (should (string= result "* H1\nText\n** H2\nMore text\n"))))

;;; Boundary Cases - No Demotion Needed

(ert-deftest test-demote-same-level ()
  "Should return content unchanged when from equals to."
  (let ((result (test-demote "* Heading\n" 1 1)))
    (should (string= result "* Heading\n"))))

(ert-deftest test-demote-promote-ignored ()
  "Should return content unchanged when to > from (promotion)."
  (let ((result (test-demote "* Heading\n" 1 2)))
    (should (string= result "* Heading\n"))))

;;; Boundary Cases - Minimum Level

(ert-deftest test-demote-respects-minimum-level ()
  "Should not demote below level 1."
  (let ((result (test-demote "** Main\n*** Sub\n" 2 1)))
    (should (string= result "* Main\n** Sub\n"))
    ;; Sub went from 3 to 2, not below 1
    (should (string-match-p "^\\*\\* Sub" result))))

(ert-deftest test-demote-deep-hierarchy-min-level ()
  "Should respect minimum level for deep hierarchies."
  (let ((result (test-demote "**** L4\n***** L5\n****** L6\n" 4 1)))
    (should (string= result "* L4\n** L5\n*** L6\n"))))

;;; Boundary Cases - Empty and Edge Cases

(ert-deftest test-demote-empty-string ()
  "Should handle empty string."
  (let ((result (test-demote "" 2 1)))
    (should (string= result ""))))

(ert-deftest test-demote-no-headings ()
  "Should return non-heading content unchanged."
  (let ((result (test-demote "Just plain text\nNo headings here\n" 2 1)))
    (should (string= result "Just plain text\nNo headings here\n"))))

(ert-deftest test-demote-heading-without-space ()
  "Should not match headings without space after stars."
  (let ((result (test-demote "**Not a heading\n** Real Heading\n" 2 1)))
    (should (string= result "**Not a heading\n* Real Heading\n"))))

;;; Edge Cases - Special Heading Content

(ert-deftest test-demote-heading-with-tags ()
  "Should demote heading preserving tags."
  (let ((result (test-demote "** Heading :tag1:tag2:\n" 2 1)))
    (should (string= result "* Heading :tag1:tag2:\n"))))

(ert-deftest test-demote-heading-with-todo ()
  "Should demote heading preserving TODO keyword."
  (let ((result (test-demote "** TODO Task\n" 2 1)))
    (should (string= result "* TODO Task\n"))))

(ert-deftest test-demote-heading-with-priority ()
  "Should demote heading preserving priority."
  (let ((result (test-demote "** [#A] Important\n" 2 1)))
    (should (string= result "* [#A] Important\n"))))

;;; Edge Cases - Whitespace

(ert-deftest test-demote-preserves-indentation ()
  "Should preserve indentation in body text."
  (let ((result (test-demote "** Heading\n  Indented text\n" 2 1)))
    (should (string= result "* Heading\n  Indented text\n"))))

(ert-deftest test-demote-multiple-spaces-after-stars ()
  "Should handle multiple spaces after stars."
  (let ((result (test-demote "**  Heading\n" 2 1)))
    (should (string= result "*  Heading\n"))))

;;; Edge Cases - Large Demotion

(ert-deftest test-demote-large-level-difference ()
  "Should handle large level differences."
  (let ((result (test-demote "****** Level 6\n******* Level 7\n" 6 1)))
    (should (string= result "* Level 6\n** Level 7\n"))))

(ert-deftest test-demote-to-level-2 ()
  "Should demote to level 2 when specified."
  (let ((result (test-demote "***** Level 5\n****** Level 6\n" 5 2)))
    (should (string= result "** Level 5\n*** Level 6\n"))))

(provide 'test-org-roam-config-demote)
;;; test-org-roam-config-demote.el ends here
