;;; test-external-open-command.el --- Tests for cj/external-open-command -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/external-open-command' in external-open.el.  The
;; function dispatches on host-environment predicates to return the
;; appropriate "open" command: xdg-open on Linux, open on macOS,
;; start on Windows.  Returns nil for unsupported hosts (callers that
;; require a command should error on nil with a contextual message).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'external-open)

(defmacro test-eoc--with-host (linux macos windows &rest body)
  "Run BODY with env-*-p predicates stubbed to LINUX, MACOS, WINDOWS."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'env-linux-p)   (lambda () ,linux))
             ((symbol-function 'env-macos-p)   (lambda () ,macos))
             ((symbol-function 'env-windows-p) (lambda () ,windows)))
     ,@body))

;;; Normal cases

(ert-deftest test-eoc-linux-returns-xdg-open ()
  "Normal: Linux host returns \"xdg-open\"."
  (test-eoc--with-host t nil nil
    (should (string= "xdg-open" (cj/external-open-command)))))

(ert-deftest test-eoc-macos-returns-open ()
  "Normal: macOS host returns \"open\"."
  (test-eoc--with-host nil t nil
    (should (string= "open" (cj/external-open-command)))))

(ert-deftest test-eoc-windows-returns-start ()
  "Normal: Windows host returns \"start\"."
  (test-eoc--with-host nil nil t
    (should (string= "start" (cj/external-open-command)))))

;;; Boundary cases

(ert-deftest test-eoc-dispatch-order-linux-wins ()
  "Boundary: Linux check runs first; wins when predicates disagree.
Documents the dispatch order.  A real host only returns t from one
of these anyway -- but if something goes wrong, Linux takes priority."
  (test-eoc--with-host t t t
    (should (string= "xdg-open" (cj/external-open-command)))))

(ert-deftest test-eoc-unsupported-host-returns-nil ()
  "Boundary: when no platform predicate returns non-nil, returns nil.
Callers requiring a command must handle the nil case explicitly --
this is a behavior change from the prior `cj/identify-external-open-command'
which signaled an error.  The wrapper `cj/xdg-open' converts nil to a
user-error with a clear message."
  (test-eoc--with-host nil nil nil
    (should-not (cj/external-open-command))))

(provide 'test-external-open-command)
;;; test-external-open-command.el ends here
