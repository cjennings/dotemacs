;;; test-org-config-tag-alignment.el --- Right-aligned org tags -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/org-tag-line-re and cj/org--tag-align-spec drive the display-property
;; right-alignment of org headline tags (org-config.el).  The regexp isolates
;; the gap before the tags (group 1) and the tag string (group 2); the spec
;; helper turns a tag string into the (space :align-to (- right WIDTH)) display
;; spec that pins the tags to the window's right edge.  These exercise the pure
;; logic directly, without driving font-lock.

;;; Code:

(require 'ert)
(require 'org-config)

(ert-deftest test-org-config-tag-align-spec-single-tag ()
  "Normal: the spec pins a tag to the right edge less its width plus margin."
  (should (equal (cj/org--tag-align-spec ":work:")
                 `(space :align-to (- right ,(+ 6 cj/org-tag-right-margin))))))

(ert-deftest test-org-config-tag-align-spec-widths ()
  "Boundary: shortest and multi-tag strings carry their width plus the margin."
  (should (equal (cj/org--tag-align-spec ":a:")
                 `(space :align-to (- right ,(+ 3 cj/org-tag-right-margin)))))
  (should (equal (cj/org--tag-align-spec ":a:b:")
                 `(space :align-to (- right ,(+ 5 cj/org-tag-right-margin))))))

(ert-deftest test-org-config-tag-line-re-matches-tagged-heading ()
  "Normal: a tagged headline matches with the gap and tags captured."
  (let ((line "** TODO Something :work:"))
    (should (string-match cj/org-tag-line-re line))
    (should (equal (match-string 1 line) " "))
    (should (equal (match-string 2 line) ":work:"))))

(ert-deftest test-org-config-tag-line-re-captures-last-gap-with-padding ()
  "Boundary: with baked padding, group 1 is the single space before the tags."
  (let ((line "*** TODO [#C] Foo      :a:b:"))
    (should (string-match cj/org-tag-line-re line))
    (should (equal (match-string 1 line) " "))
    (should (equal (match-string 2 line) ":a:b:"))))

(ert-deftest test-org-config-tag-line-re-rejects-untagged-and-non-heading ()
  "Error: an untagged heading and a non-heading line do not match."
  (should-not (string-match cj/org-tag-line-re "** TODO Foo"))
  (should-not (string-match cj/org-tag-line-re "not a heading :work:")))

(provide 'test-org-config-tag-alignment)
;;; test-org-config-tag-alignment.el ends here
