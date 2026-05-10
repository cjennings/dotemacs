;;; test-dirvish-config-dired-line-directory.el --- Tests for the directory-line predicate -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--dired-line-is-directory-p' is the testable predicate behind
;; `cj/dired-mark-all-visible-files'.  Dired buffers prefix each file
;; line with a one-char mark column followed by the `ls -l' output, so
;; column 2 is the file-type letter (`d' for directory, `-' for regular
;; file).  The wrapper iterates the buffer and skips lines this
;; predicate returns t for; the iteration stays dired-coupled and
;; untested, but the line-classification logic is now isolated.

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

(ert-deftest test-cj--dired-line-is-directory-p-unmarked-directory ()
  "Normal: an unmarked directory line (`  drwx...') matches."
  (should (cj/--dired-line-is-directory-p
           "  drwxr-xr-x 1 me me 4096 May 10 13:00 subdir/")))

(ert-deftest test-cj--dired-line-is-directory-p-marked-directory ()
  "Normal: a star-marked directory line (`* drwx...') matches."
  (should (cj/--dired-line-is-directory-p
           "* drwxr-xr-x 1 me me 4096 May 10 13:00 subdir/")))

(ert-deftest test-cj--dired-line-is-directory-p-regular-file ()
  "Normal: a regular file line (`  -rw...') does not match."
  (should-not (cj/--dired-line-is-directory-p
               "  -rw-r--r-- 1 me me 42 May 10 13:00 notes.txt")))

(ert-deftest test-cj--dired-line-is-directory-p-symlink-line ()
  "Boundary: a symlink line (`  lrwx...') does not match -- only `d' is a dir."
  (should-not (cj/--dired-line-is-directory-p
               "  lrwxrwxrwx 1 me me 12 May 10 13:00 link -> target")))

(ert-deftest test-cj--dired-line-is-directory-p-empty-line ()
  "Boundary: an empty string does not match."
  (should-not (cj/--dired-line-is-directory-p "")))

(ert-deftest test-cj--dired-line-is-directory-p-header-line ()
  "Boundary: a dired header (`  /path/to:') or `total' line does not match."
  (should-not (cj/--dired-line-is-directory-p "  /home/me/projects:"))
  (should-not (cj/--dired-line-is-directory-p "  total 24")))

(provide 'test-dirvish-config-dired-line-directory)
;;; test-dirvish-config-dired-line-directory.el ends here
