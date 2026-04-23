;;; test-system-utils--open-with-is-launcher-p.el --- Tests for cj/--open-with-is-launcher-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--open-with-is-launcher-p' in system-utils.el.
;; The predicate returns t for desktop launcher commands (xdg-open,
;; open, start) that need `call-process' with a zero buffer argument
;; to fully detach from Emacs.  Anything else returns nil.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-utils)

;;; Normal cases

(ert-deftest test-owilp-xdg-open-is-launcher ()
  "Normal: \"xdg-open\" (Linux launcher) returns t."
  (should (eq t (cj/--open-with-is-launcher-p "xdg-open"))))

(ert-deftest test-owilp-open-is-launcher ()
  "Normal: \"open\" (macOS launcher) returns t."
  (should (eq t (cj/--open-with-is-launcher-p "open"))))

(ert-deftest test-owilp-start-is-launcher ()
  "Normal: \"start\" (Windows launcher) returns t."
  (should (eq t (cj/--open-with-is-launcher-p "start"))))

;;; Boundary cases

(ert-deftest test-owilp-non-launcher-command-returns-nil ()
  "Boundary: a non-launcher command (e.g. gimp) returns nil."
  (should-not (cj/--open-with-is-launcher-p "gimp")))

(ert-deftest test-owilp-empty-string-returns-nil ()
  "Boundary: empty string is not a launcher."
  (should-not (cj/--open-with-is-launcher-p "")))

(ert-deftest test-owilp-case-sensitive ()
  "Boundary: launcher check is case-sensitive (\"Open\" is not \"open\")."
  (should-not (cj/--open-with-is-launcher-p "Open"))
  (should-not (cj/--open-with-is-launcher-p "XDG-OPEN")))

;;; Error cases

(ert-deftest test-owilp-nil-argument-returns-nil ()
  "Error: nil input is handled gracefully (not in the launcher list)."
  (should-not (cj/--open-with-is-launcher-p nil)))

(provide 'test-system-utils--open-with-is-launcher-p)
;;; test-system-utils--open-with-is-launcher-p.el ends here
