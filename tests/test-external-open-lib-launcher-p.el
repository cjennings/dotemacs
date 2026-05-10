;;; test-external-open-lib-launcher-p.el --- Tests for cj/external-open-launcher-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/external-open-launcher-p' in external-open-lib.el.
;; The predicate returns t for desktop launcher commands (xdg-open,
;; open, start) that need `call-process' with a zero buffer argument
;; to fully detach from Emacs.  Anything else returns nil.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'external-open-lib)

;;; Normal cases

(ert-deftest test-eolp-xdg-open-is-launcher ()
  "Normal: \"xdg-open\" (Linux launcher) returns t."
  (should (eq t (cj/external-open-launcher-p "xdg-open"))))

(ert-deftest test-eolp-open-is-launcher ()
  "Normal: \"open\" (macOS launcher) returns t."
  (should (eq t (cj/external-open-launcher-p "open"))))

(ert-deftest test-eolp-start-is-launcher ()
  "Normal: \"start\" (Windows launcher) returns t."
  (should (eq t (cj/external-open-launcher-p "start"))))

;;; Boundary cases

(ert-deftest test-eolp-non-launcher-command-returns-nil ()
  "Boundary: a non-launcher command (e.g. gimp) returns nil."
  (should-not (cj/external-open-launcher-p "gimp")))

(ert-deftest test-eolp-empty-string-returns-nil ()
  "Boundary: empty string is not a launcher."
  (should-not (cj/external-open-launcher-p "")))

(ert-deftest test-eolp-case-sensitive ()
  "Boundary: launcher check is case-sensitive (\"Open\" is not \"open\")."
  (should-not (cj/external-open-launcher-p "Open"))
  (should-not (cj/external-open-launcher-p "XDG-OPEN")))

;;; Error cases

(ert-deftest test-eolp-nil-argument-returns-nil ()
  "Error: nil input is handled gracefully (not in the launcher list)."
  (should-not (cj/external-open-launcher-p nil)))

(provide 'test-external-open-lib-launcher-p)
;;; test-external-open-lib-launcher-p.el ends here
