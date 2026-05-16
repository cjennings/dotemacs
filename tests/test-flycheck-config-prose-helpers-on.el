;;; test-flycheck-config-prose-helpers-on.el --- Tests for cj/prose-helpers-on -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `cj/prose-helpers-on'.  The function must enable
;; `abbrev-mode' and `flycheck-mode' with an explicit positive
;; argument, and must be a no-op when both are already on.
;;
;; Regression: a prior shape
;;   (if (not (abbrev-mode)) (abbrev-mode))
;; called `abbrev-mode' with no argument -- the toggle signature, not
;; a query.  When the mode was already on the function flipped it off
;; then on (two transitions per invocation, firing the disable/enable
;; hooks each time) instead of being a no-op.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'flycheck-config)

;; `abbrev-mode' is preloaded (defvar in core); `flycheck-mode' is autoloaded
;; via use-package with =:defer t= so its defvar hasn't run by the time the
;; tests dynamically let-bind these variables.  Declare both as special here
;; so `let' creates dynamic bindings that `bound-and-true-p' inside the
;; production code can read -- otherwise lexical-binding=t makes the let
;; lexical-only and the inner reads can't see the test's state.
(defvar abbrev-mode)
(defvar flycheck-mode)

(defmacro test-flycheck-config--with-prose-spies (abbrev-state flycheck-state &rest body)
  "Run BODY with `abbrev-mode' / `flycheck-mode' stubbed and dynamic.
ABBREV-STATE and FLYCHECK-STATE seed the dynamic values of the mode
variables so `bound-and-true-p' inside the production code reads
them.  Inside BODY, `abbrev-calls' / `flycheck-calls' carry the args
each stub received in call order.  The stubs return their arg so the
buggy no-arg toggle shape terminates without infinite recursion."
  (declare (indent 2))
  `(let ((abbrev-calls '())
         (flycheck-calls '())
         (abbrev-mode ,abbrev-state)
         (flycheck-mode ,flycheck-state))
     (cl-letf (((symbol-function 'abbrev-mode)
                (lambda (&optional arg)
                  (setq abbrev-calls (append abbrev-calls (list arg)))
                  arg))
               ((symbol-function 'flycheck-mode)
                (lambda (&optional arg)
                  (setq flycheck-calls (append flycheck-calls (list arg)))
                  arg)))
       ,@body)))

(ert-deftest test-flycheck-config-prose-helpers-on-normal-both-off-enables-both ()
  "Normal: both modes off -> each enabled with explicit positive arg."
  (test-flycheck-config--with-prose-spies nil nil
    (cj/prose-helpers-on)
    (should (= 1 (length abbrev-calls)))
    (should (> (prefix-numeric-value (car abbrev-calls)) 0))
    (should (= 1 (length flycheck-calls)))
    (should (> (prefix-numeric-value (car flycheck-calls)) 0))))

(ert-deftest test-flycheck-config-prose-helpers-on-boundary-both-on-is-noop ()
  "Boundary: both modes already on -> neither mode function called.
Catches the no-arg toggle shape; the bug records at least one call
with a nil arg."
  (test-flycheck-config--with-prose-spies t t
    (cj/prose-helpers-on)
    (should (null abbrev-calls))
    (should (null flycheck-calls))))

(ert-deftest test-flycheck-config-prose-helpers-on-boundary-abbrev-on-flycheck-off ()
  "Boundary: abbrev on, flycheck off -> only flycheck enabled."
  (test-flycheck-config--with-prose-spies t nil
    (cj/prose-helpers-on)
    (should (null abbrev-calls))
    (should (= 1 (length flycheck-calls)))
    (should (> (prefix-numeric-value (car flycheck-calls)) 0))))

(ert-deftest test-flycheck-config-prose-helpers-on-boundary-flycheck-on-abbrev-off ()
  "Boundary: flycheck on, abbrev off -> only abbrev enabled."
  (test-flycheck-config--with-prose-spies nil t
    (cj/prose-helpers-on)
    (should (= 1 (length abbrev-calls)))
    (should (> (prefix-numeric-value (car abbrev-calls)) 0))
    (should (null flycheck-calls))))

(provide 'test-flycheck-config-prose-helpers-on)
;;; test-flycheck-config-prose-helpers-on.el ends here
