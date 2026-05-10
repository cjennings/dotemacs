;;; test-dirvish-config-duplicate-file-name.el --- Tests for the duplicate-file-name helper -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--duplicate-file-name' is the pure name-mangling half of
;; `cj/dirvish-duplicate-file': given an absolute file path, return the
;; new path with `-copy' inserted before the extension.  The interactive
;; wrapper handles the dired side effects (existence check, copy, revert).

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

(ert-deftest test-cj--duplicate-file-name-with-extension ()
  "Normal: a file with an extension gets `-copy' before the dot."
  (should (equal (cj/--duplicate-file-name "/tmp/report.pdf")
                 "/tmp/report-copy.pdf")))

(ert-deftest test-cj--duplicate-file-name-elisp-extension ()
  "Normal: works for elisp files (the project's daily case)."
  (should (equal (cj/--duplicate-file-name "/home/foo/script.el")
                 "/home/foo/script-copy.el")))

(ert-deftest test-cj--duplicate-file-name-no-extension ()
  "Boundary: an extensionless file appends `-copy' at the end."
  (should (equal (cj/--duplicate-file-name "/dir/README")
                 "/dir/README-copy")))

(ert-deftest test-cj--duplicate-file-name-multiple-dots ()
  "Boundary: only the last dot counts as the extension separator."
  (should (equal (cj/--duplicate-file-name "/tmp/archive.tar.gz")
                 "/tmp/archive.tar-copy.gz")))

(ert-deftest test-cj--duplicate-file-name-dotfile ()
  "Boundary: a leading-dot file (no real extension) keeps the dot in the base."
  (should (equal (cj/--duplicate-file-name "/home/foo/.bashrc")
                 "/home/foo/.bashrc-copy")))

(ert-deftest test-cj--duplicate-file-name-relative-path ()
  "Boundary: relative paths preserve their relative form."
  (should (equal (cj/--duplicate-file-name "doc.txt")
                 "doc-copy.txt")))

(ert-deftest test-cj--duplicate-file-name-spaces-in-name ()
  "Boundary: spaces in the base name are preserved."
  (should (equal (cj/--duplicate-file-name "/tmp/my file.txt")
                 "/tmp/my file-copy.txt")))

(provide 'test-dirvish-config-duplicate-file-name)
;;; test-dirvish-config-duplicate-file-name.el ends here
