;;; test-system-lib-font-lock-global-modes.el --- Tests for the font-lock exclusion helper -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `cj/--font-lock-global-modes-excluding', the pure transform
;; behind `cj/exclude-from-global-font-lock'.  Some major modes (dashboard,
;; mu4e) paint their buffers with manual `face' text properties; global
;; font-lock then strips those.  The helper adds a mode to the
;; `font-lock-global-modes' exclusion, handling its three shapes: t (all
;; modes on), a (not M...) exclusion list, and an (M...) inclusion list.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'system-lib)

(ert-deftest test-system-lib-flgm-from-t-builds-not-list ()
  "Normal: t (all modes on) becomes a (not MODE) exclusion."
  (let ((r (cj/--font-lock-global-modes-excluding t 'dashboard-mode)))
    (should (eq (car r) 'not))
    (should (memq 'dashboard-mode (cdr r)))))

(ert-deftest test-system-lib-flgm-adds-to-existing-not-list ()
  "Normal: a second mode is added to an existing (not ...) list."
  (let ((r (cj/--font-lock-global-modes-excluding '(not dashboard-mode) 'mu4e-headers-mode)))
    (should (eq (car r) 'not))
    (should (memq 'dashboard-mode (cdr r)))
    (should (memq 'mu4e-headers-mode (cdr r)))))

(ert-deftest test-system-lib-flgm-idempotent-on-already-excluded ()
  "Boundary: excluding an already-excluded mode does not duplicate it."
  (let ((r (cj/--font-lock-global-modes-excluding '(not a-mode) 'a-mode)))
    (should (eq (car r) 'not))
    (should (= 1 (cl-count 'a-mode (cdr r))))))

(ert-deftest test-system-lib-flgm-removes-from-inclusion-list ()
  "Boundary: in an (M...) inclusion list, excluding a mode removes it."
  (should (equal (cj/--font-lock-global-modes-excluding '(foo-mode bar-mode) 'foo-mode)
                 '(bar-mode))))

(ert-deftest test-system-lib-flgm-nil-stays-nil ()
  "Boundary: nil (no mode gets global font-lock) already excludes everything."
  (should (equal (cj/--font-lock-global-modes-excluding nil 'x-mode) nil)))

(provide 'test-system-lib-font-lock-global-modes)
;;; test-system-lib-font-lock-global-modes.el ends here
