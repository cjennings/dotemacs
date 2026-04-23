;;; test-system-utils-identify-external-open-command.el --- Tests for cj/identify-external-open-command -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/identify-external-open-command' in system-utils.el.
;; The function dispatches on host-environment predicates to return the
;; appropriate "open" command: xdg-open on Linux, open on macOS,
;; start on Windows.  Anything else is a fatal error.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-utils)

(defmacro test-siuec--with-host (linux macos windows &rest body)
  "Run BODY with env-*-p predicates stubbed to LINUX, MACOS, WINDOWS."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'env-linux-p)   (lambda () ,linux))
			 ((symbol-function 'env-macos-p)   (lambda () ,macos))
			 ((symbol-function 'env-windows-p) (lambda () ,windows)))
	 ,@body))

;;; Normal cases

(ert-deftest test-siuec-linux-returns-xdg-open ()
  "Normal: Linux host returns \"xdg-open\"."
  (test-siuec--with-host t nil nil
	(should (string= "xdg-open" (cj/identify-external-open-command)))))

(ert-deftest test-siuec-macos-returns-open ()
  "Normal: macOS host returns \"open\"."
  (test-siuec--with-host nil t nil
	(should (string= "open" (cj/identify-external-open-command)))))

(ert-deftest test-siuec-windows-returns-start ()
  "Normal: Windows host returns \"start\"."
  (test-siuec--with-host nil nil t
	(should (string= "start" (cj/identify-external-open-command)))))

;;; Boundary cases

(ert-deftest test-siuec-dispatch-order-linux-wins ()
  "Boundary: Linux check runs first; wins when predicates disagree.
Documents the dispatch order.  A real host only returns t from one
of these anyway — but if something goes wrong, Linux takes priority."
  (test-siuec--with-host t t t
	(should (string= "xdg-open" (cj/identify-external-open-command)))))

;;; Error cases

(ert-deftest test-siuec-unsupported-host-signals-error ()
  "Error: when no platform predicate returns non-nil, signals an error."
  (test-siuec--with-host nil nil nil
	(should-error (cj/identify-external-open-command))))

(provide 'test-system-utils-identify-external-open-command)
;;; test-system-utils-identify-external-open-command.el ends here
