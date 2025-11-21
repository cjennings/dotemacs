;;; test-org-noter--title-to-slug.el --- Tests for cj/org-noter--title-to-slug -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the title-to-slug function used in org-noter workflow.
;; Tests cover normal, boundary, and error cases.

;;; Code:

(require 'ert)
(require 'org-noter-config)

;;; Normal Cases

(ert-deftest test-org-noter--title-to-slug-normal-simple-title ()
  "Normal case: Simple title with spaces."
  (should (equal (cj/org-noter--title-to-slug "The Pragmatic Programmer")
                 "the-pragmatic-programmer")))

(ert-deftest test-org-noter--title-to-slug-normal-single-word ()
  "Normal case: Single word title."
  (should (equal (cj/org-noter--title-to-slug "Dune")
                 "dune")))

(ert-deftest test-org-noter--title-to-slug-normal-with-numbers ()
  "Normal case: Title with numbers."
  (should (equal (cj/org-noter--title-to-slug "1984 by George Orwell")
                 "1984-by-george-orwell")))

(ert-deftest test-org-noter--title-to-slug-normal-mixed-case ()
  "Normal case: Title with mixed case."
  (should (equal (cj/org-noter--title-to-slug "SICP Structure and Interpretation")
                 "sicp-structure-and-interpretation")))

;;; Boundary Cases

(ert-deftest test-org-noter--title-to-slug-boundary-special-chars ()
  "Boundary case: Title with special characters."
  (should (equal (cj/org-noter--title-to-slug "C++: The Complete Guide")
                 "c-the-complete-guide")))

(ert-deftest test-org-noter--title-to-slug-boundary-punctuation ()
  "Boundary case: Title with punctuation."
  (should (equal (cj/org-noter--title-to-slug "Why's (Poignant) Guide to Ruby")
                 "why-s-poignant-guide-to-ruby")))

(ert-deftest test-org-noter--title-to-slug-boundary-leading-special ()
  "Boundary case: Title starting with special character."
  (should (equal (cj/org-noter--title-to-slug "...And Then There Were None")
                 "and-then-there-were-none")))

(ert-deftest test-org-noter--title-to-slug-boundary-trailing-special ()
  "Boundary case: Title ending with special character."
  (should (equal (cj/org-noter--title-to-slug "What Is This Thing Called Love?")
                 "what-is-this-thing-called-love")))

(ert-deftest test-org-noter--title-to-slug-boundary-multiple-spaces ()
  "Boundary case: Title with multiple consecutive spaces."
  (should (equal (cj/org-noter--title-to-slug "The   Great    Gatsby")
                 "the-great-gatsby")))

(ert-deftest test-org-noter--title-to-slug-boundary-underscores ()
  "Boundary case: Title with underscores."
  (should (equal (cj/org-noter--title-to-slug "file_name_example")
                 "file-name-example")))

(ert-deftest test-org-noter--title-to-slug-boundary-hyphens ()
  "Boundary case: Title with existing hyphens."
  (should (equal (cj/org-noter--title-to-slug "Self-Reliance")
                 "self-reliance")))

(ert-deftest test-org-noter--title-to-slug-boundary-all-numbers ()
  "Boundary case: Title that is all numbers."
  (should (equal (cj/org-noter--title-to-slug "2001")
                 "2001")))

;;; Edge Cases

(ert-deftest test-org-noter--title-to-slug-edge-empty-string ()
  "Edge case: Empty string."
  (should (equal (cj/org-noter--title-to-slug "")
                 "")))

(ert-deftest test-org-noter--title-to-slug-edge-only-special-chars ()
  "Edge case: Only special characters."
  (should (equal (cj/org-noter--title-to-slug "!@#$%^&*()")
                 "")))

(ert-deftest test-org-noter--title-to-slug-edge-unicode ()
  "Edge case: Title with unicode characters."
  (should (equal (cj/org-noter--title-to-slug "Café au Lait")
                 "caf-au-lait")))

(ert-deftest test-org-noter--title-to-slug-edge-long-title ()
  "Edge case: Very long title."
  (let ((long-title "The Absolutely Incredibly Long Title of This Book That Goes On and On"))
    (should (equal (cj/org-noter--title-to-slug long-title)
                   "the-absolutely-incredibly-long-title-of-this-book-that-goes-on-and-on"))))

(provide 'test-org-noter--title-to-slug)
;;; test-org-noter--title-to-slug.el ends here
