;;; test-system-lib-confirm-strong.el --- Tests for cj/confirm-strong -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `cj/confirm-strong', the typed-"yes" confirmation used for
;; irreversible actions.  The behavior under test is the long-form guarantee:
;; the prompt demands a typed yes/no even when the global single-key default
;; (`use-short-answers') is in effect.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'system-lib)

(ert-deftest test-system-lib-confirm-strong-returns-t-on-yes ()
  "Normal: passes a t answer through from `yes-or-no-p'."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (should (eq (cj/confirm-strong "Really? ") t))))

(ert-deftest test-system-lib-confirm-strong-returns-nil-on-no ()
  "Normal: passes a nil answer through from `yes-or-no-p'."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
    (should (eq (cj/confirm-strong "Really? ") nil))))

(ert-deftest test-system-lib-confirm-strong-forces-long-form ()
  "Boundary: binds `use-short-answers' to nil for the call even when it is
globally t, so the irreversible prompt requires a typed yes/no regardless of
the single-key default."
  (let ((use-short-answers t)
        (seen 'unset))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (setq seen use-short-answers) t)))
      (cj/confirm-strong "Really? ")
      (should (eq seen nil)))))

(provide 'test-system-lib-confirm-strong)
;;; test-system-lib-confirm-strong.el ends here
