;;; test-org-capture-config--find-or-create-top-heading.el --- Tests for the shared find-or-create helper -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--org-find-or-create-top-heading is the search-or-append positioning block
;; extracted from cj/org-capture--goto-file-headline, cj/--org-capture-goto-open-work,
;; and cj/--org-capture-goto-exact-headline.  The three call sites stay covered by
;; test-org-capture-config-project-target.el (open-work, exact-headline) and the
;; target-cache test; these cover the generic helper directly with a plain regexp
;; (so the test doesn't depend on org's complex-heading format).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

(ert-deftest test-org-find-or-create-top-heading-finds-existing ()
  "Normal: an existing heading is found; point lands at its line start and the
buffer is unchanged."
  (with-temp-buffer
    (insert "* Alpha\nbody\n* Target\nmore\n")
    (cj/--org-find-or-create-top-heading "^\\* Target$" "* Target")
    (should (looking-at-p "\\* Target$"))
    (should (equal (buffer-string) "* Alpha\nbody\n* Target\nmore\n"))))

(ert-deftest test-org-find-or-create-top-heading-creates-when-absent ()
  "Boundary: with no match, the heading line is appended (a separating newline
added because the buffer doesn't end in one) and point lands on it."
  (with-temp-buffer
    (insert "some text")                ; no trailing newline
    (cj/--org-find-or-create-top-heading "^\\* Missing$" "* Missing")
    (should (equal (buffer-string) "some text\n* Missing\n"))
    (should (looking-at-p "\\* Missing$"))))

(ert-deftest test-org-find-or-create-top-heading-empty-buffer ()
  "Boundary: in an empty buffer the heading is inserted at the top, no extra
leading newline."
  (with-temp-buffer
    (cj/--org-find-or-create-top-heading "^\\* X$" "* X")
    (should (equal (buffer-string) "* X\n"))
    (should (looking-at-p "\\* X$"))))

(provide 'test-org-capture-config--find-or-create-top-heading)
;;; test-org-capture-config--find-or-create-top-heading.el ends here
