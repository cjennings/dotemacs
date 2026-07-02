;;; test-org-babel-config.el --- Tests for babel confirmation toggle -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers cj/org-babel-toggle-confirm, which flips `org-confirm-babel-evaluate'
;; between t (the safe default — confirm before running a block) and nil, and
;; the C-; O b binding (on `cj/org-map') that invokes it.

;;; Code:

(require 'ert)
(require 'org-config)  ;; provides cj/org-map, where the toggle is bound
(require 'org-babel-config)

;; org defines this as a defcustom, but org is not loaded in batch; declare it
;; special here so the let-bindings below are dynamic.
(defvar org-confirm-babel-evaluate t)

(ert-deftest test-org-babel-toggle-confirm-flips-from-t-to-nil ()
  "Normal: toggling when confirmation is on turns it off."
  (let ((org-confirm-babel-evaluate t))
    (cj/org-babel-toggle-confirm)
    (should-not org-confirm-babel-evaluate)))

(ert-deftest test-org-babel-toggle-confirm-flips-from-nil-to-t ()
  "Normal: toggling when confirmation is off turns it on."
  (let ((org-confirm-babel-evaluate nil))
    (cj/org-babel-toggle-confirm)
    (should (eq t org-confirm-babel-evaluate))))

(ert-deftest test-org-babel-toggle-confirm-bound-to-key ()
  "Smoke: C-; O b (org menu) invokes the toggle command."
  (should (eq (keymap-lookup cj/org-map "b")
              #'cj/org-babel-toggle-confirm)))

(ert-deftest test-org-babel-toggle-confirm-old-binding-freed ()
  "Boundary: the old C-; k global binding is gone."
  (should-not (eq (keymap-lookup (current-global-map) "C-; k")
                  #'cj/org-babel-toggle-confirm)))

(provide 'test-org-babel-config)
;;; test-org-babel-config.el ends here
