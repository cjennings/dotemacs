;;; test-org-noter-config--title-to-slug.el --- Tests for cj/org-noter--title-to-slug -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/org-noter--title-to-slug'. The function lowercases its
;; input, replaces every run of non-alphanumeric characters with a
;; single hyphen, and trims leading/trailing hyphens. It's used to
;; turn a book title into a filename-safe slug.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'org-noter-config)

(ert-deftest test-org-noter-config-title-to-slug-multi-word ()
  "Normal: a multi-word title becomes hyphen-separated lowercase."
  (should (equal "the-pragmatic-programmer"
                 (cj/org-noter--title-to-slug "The Pragmatic Programmer"))))

(ert-deftest test-org-noter-config-title-to-slug-single-word ()
  "Normal: a single-word lowercase title is unchanged."
  (should (equal "anaphora" (cj/org-noter--title-to-slug "anaphora"))))

(ert-deftest test-org-noter-config-title-to-slug-mixed-punctuation ()
  "Boundary: punctuation is collapsed into single hyphens."
  (should (equal "godel-escher-bach"
                 (cj/org-noter--title-to-slug "Godel, Escher, Bach!"))))

(ert-deftest test-org-noter-config-title-to-slug-leading-trailing-non-alnum ()
  "Boundary: leading and trailing non-alnum runs are trimmed."
  (should (equal "title"
                 (cj/org-noter--title-to-slug "  ...title!?? "))))

(ert-deftest test-org-noter-config-title-to-slug-collapses-multiple-non-alnum ()
  "Boundary: a run of non-alnum chars in the middle becomes one hyphen."
  (should (equal "a-b" (cj/org-noter--title-to-slug "a   ---   b"))))

(ert-deftest test-org-noter-config-title-to-slug-numbers-preserved ()
  "Boundary: digits are preserved as-is."
  (should (equal "1984" (cj/org-noter--title-to-slug "1984"))))

(ert-deftest test-org-noter-config-title-to-slug-empty-string ()
  "Error: empty input yields empty string."
  (should (equal "" (cj/org-noter--title-to-slug ""))))

(provide 'test-org-noter-config--title-to-slug)
;;; test-org-noter-config--title-to-slug.el ends here
