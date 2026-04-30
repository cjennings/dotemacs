;;; test-host-environment--platform-predicates.el --- Tests for env-linux/bsd/macos/windows-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the platform predicates in host-environment.el.  Each is a
;; thin wrapper around `system-type'.  Tests rely on `system-type'
;; being a special variable (so a `let'-binding shadows the global
;; value) and walk every supported platform to confirm the right
;; predicate returns t.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'host-environment)

(ert-deftest test-host-environment-linux-p-true-on-gnu-linux ()
  "Normal: env-linux-p returns t when system-type is gnu/linux."
  (let ((system-type 'gnu/linux))
    (should (env-linux-p))))

(ert-deftest test-host-environment-linux-p-false-on-other-platforms ()
  "Boundary: env-linux-p returns nil on every non-Linux platform."
  (dolist (other '(darwin berkeley-unix windows-nt cygwin ms-dos))
    (let ((system-type other))
      (should-not (env-linux-p)))))

(ert-deftest test-host-environment-bsd-p-true-on-berkeley-unix ()
  "Normal: env-bsd-p returns t when system-type is berkeley-unix."
  (let ((system-type 'berkeley-unix))
    (should (env-bsd-p))))

(ert-deftest test-host-environment-bsd-p-false-on-other-platforms ()
  "Boundary: env-bsd-p returns nil on every non-BSD platform."
  (dolist (other '(gnu/linux darwin windows-nt cygwin ms-dos))
    (let ((system-type other))
      (should-not (env-bsd-p)))))

(ert-deftest test-host-environment-macos-p-true-on-darwin ()
  "Normal: env-macos-p returns t when system-type is darwin."
  (let ((system-type 'darwin))
    (should (env-macos-p))))

(ert-deftest test-host-environment-macos-p-false-on-other-platforms ()
  "Boundary: env-macos-p returns nil on every non-Darwin platform."
  (dolist (other '(gnu/linux berkeley-unix windows-nt cygwin ms-dos))
    (let ((system-type other))
      (should-not (env-macos-p)))))

(ert-deftest test-host-environment-windows-p-true-on-each-windows-variant ()
  "Normal: env-windows-p returns t on cygwin, windows-nt, and ms-dos."
  (dolist (win '(cygwin windows-nt ms-dos))
    (let ((system-type win))
      (should (env-windows-p)))))

(ert-deftest test-host-environment-windows-p-false-on-unix-platforms ()
  "Boundary: env-windows-p returns nil on Unix-family platforms."
  (dolist (other '(gnu/linux darwin berkeley-unix))
    (let ((system-type other))
      (should-not (env-windows-p)))))

(provide 'test-host-environment--platform-predicates)
;;; test-host-environment--platform-predicates.el ends here
