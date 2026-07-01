;;; test-eat-config--charset-never-nil.el --- Tests for the EAT charset nil-guard -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--eat-charset-never-nil', the `:filter-args' guard on
;; `eat--t-set-charset'.  eat 0.9.4 accepts more charset-designation final
;; bytes in its parser than its store step maps, so an unmapped designation
;; (e.g. ESC ( A) stores nil as that slot's charset; the next character write
;; then trips (cl-assert charset) in `eat--t-write', repeatedly, via the
;; output-queue timer.  The guard coerces a nil charset to `us-ascii' before
;; it is ever stored, so the assertion can't fire.

;;; Code:

(require 'ert)

;; Stub keymap dep before loading the module (matches the other module tests).
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'eat-config)

(ert-deftest test-eat-config-charset-never-nil-normal-real-charset-unchanged ()
  "Normal: a real charset value passes through unchanged."
  (should (equal (cj/--eat-charset-never-nil '(g0 us-ascii))
                 '(g0 us-ascii)))
  (should (equal (cj/--eat-charset-never-nil '(g1 dec-line-drawing))
                 '(g1 dec-line-drawing))))

(ert-deftest test-eat-config-charset-never-nil-error-nil-becomes-us-ascii ()
  "Error: a nil charset (unmapped designation) is coerced to `us-ascii'."
  (should (equal (cj/--eat-charset-never-nil '(g0 nil))
                 '(g0 us-ascii)))
  (should (equal (cj/--eat-charset-never-nil '(g3 nil))
                 '(g3 us-ascii))))

(ert-deftest test-eat-config-charset-never-nil-boundary-preserves-slot ()
  "Boundary: the slot symbol is preserved for every slot, coerced or not."
  (dolist (slot '(g0 g1 g2 g3))
    (should (eq (car (cj/--eat-charset-never-nil (list slot nil))) slot))
    (should (eq (cadr (cj/--eat-charset-never-nil (list slot nil))) 'us-ascii))))

(provide 'test-eat-config--charset-never-nil)
;;; test-eat-config--charset-never-nil.el ends here
