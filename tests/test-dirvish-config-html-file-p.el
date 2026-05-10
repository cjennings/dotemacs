;;; test-dirvish-config-html-file-p.el --- Tests for the html-file predicate -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--html-file-p' is the predicate behind `cj/dirvish-open-html-in-eww'.
;; The earlier inline regex was case-sensitive (`.html?\='), but most
;; browsers treat `.HTML' as HTML too, so the helper is case-insensitive.
;; The predicate also anchors on the trailing extension so files with
;; "html" embedded in the middle of the name don't match.

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

(ert-deftest test-cj--html-file-p-html-extension-matches ()
  "Normal: `.html' extension matches."
  (should (cj/--html-file-p "/tmp/index.html")))

(ert-deftest test-cj--html-file-p-htm-extension-matches ()
  "Normal: `.htm' extension matches (legacy 8.3 form)."
  (should (cj/--html-file-p "/tmp/page.htm")))

(ert-deftest test-cj--html-file-p-uppercase-html-matches ()
  "Boundary: `.HTML' (uppercase) matches -- match is case-insensitive."
  (should (cj/--html-file-p "/tmp/index.HTML")))

(ert-deftest test-cj--html-file-p-mixed-case-html-matches ()
  "Boundary: mixed case (`.Html') matches."
  (should (cj/--html-file-p "/tmp/index.Html")))

(ert-deftest test-cj--html-file-p-embedded-html-does-not-match ()
  "Boundary: `html' in the middle of the name doesn't match (anchor at end)."
  (should-not (cj/--html-file-p "/tmp/html-thing.org")))

(ert-deftest test-cj--html-file-p-non-html-extension-does-not-match ()
  "Error: a non-html file returns nil."
  (should-not (cj/--html-file-p "/tmp/notes.txt")))

(ert-deftest test-cj--html-file-p-no-extension-does-not-match ()
  "Boundary: a file without an extension does not match."
  (should-not (cj/--html-file-p "/tmp/README")))

(provide 'test-dirvish-config-html-file-p)
;;; test-dirvish-config-html-file-p.el ends here
