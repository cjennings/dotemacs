;;; test-mousetrap-mode--get-profile-for-mode.el --- Tests for profile lookup -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for mouse-trap--get-profile-for-mode function.
;; Tests profile lookup logic including exact matches, inheritance,
;; and fallback to default profile.

;;; Code:

(require 'ert)
(require 'mousetrap-mode)

;;; Normal Cases

(ert-deftest test-mousetrap-mode--get-profile-for-mode-normal-exact-match-returns-profile ()
  "Test exact mode match returns mapped profile."
  (let ((major-mode 'dashboard-mode))
    (should (eq 'primary-click (mouse-trap--get-profile-for-mode)))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-normal-inherited-mode-returns-parent-profile ()
  "Test that org-mode inherits disabled profile from text-mode."
  (with-temp-buffer
    (org-mode)
    (should (eq 'disabled (mouse-trap--get-profile-for-mode)))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-normal-unmapped-mode-returns-default ()
  "Test unmapped mode returns default profile."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (eq 'disabled (mouse-trap--get-profile-for-mode)))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-normal-special-mode-derivative-returns-disabled ()
  "Test that help-mode inherits disabled from special-mode."
  (with-temp-buffer
    (help-mode)
    (should (eq 'disabled (mouse-trap--get-profile-for-mode)))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-normal-pdf-view-mode-returns-full ()
  "Test pdf-view-mode returns full profile."
  (let ((major-mode 'pdf-view-mode))
    (should (eq 'full (mouse-trap--get-profile-for-mode)))))

;;; Boundary Cases

(ert-deftest test-mousetrap-mode--get-profile-for-mode-boundary-empty-mode-profiles-returns-default ()
  "Test empty mode profiles list returns default."
  (let ((mouse-trap-mode-profiles nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (eq 'disabled (mouse-trap--get-profile-for-mode))))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-boundary-exact-match-priority-over-inheritance ()
  "Test exact mode match takes priority over inherited match."
  (let ((major-mode 'text-mode))
    ;; text-mode is explicitly mapped to disabled
    (should (eq 'disabled (mouse-trap--get-profile-for-mode)))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-boundary-first-parent-match-wins ()
  "Test first matching parent profile wins with multiple inheritance.
When a mode could match multiple parent profiles, the first one
in mouse-trap-mode-profiles should win."
  (let ((mouse-trap-mode-profiles
         '((special-mode . disabled)
           (text-mode . scroll-only)))
        (major-mode 'derived-test-mode))
    ;; Simulate a mode that derives from special-mode
    (put 'derived-test-mode 'derived-mode-parent 'special-mode)
    (with-temp-buffer
      (setq major-mode 'derived-test-mode)
      (should (eq 'disabled (mouse-trap--get-profile-for-mode))))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-boundary-deeply-nested-inheritance ()
  "Test profile lookup works through deep inheritance chain."
  ;; Create a deep inheritance chain: level3 -> level2 -> level1 -> text-mode
  (let ((mouse-trap-mode-profiles
         '((text-mode . disabled)))
        (major-mode 'level3-mode))
    (put 'level1-mode 'derived-mode-parent 'text-mode)
    (put 'level2-mode 'derived-mode-parent 'level1-mode)
    (put 'level3-mode 'derived-mode-parent 'level2-mode)
    (with-temp-buffer
      (setq major-mode 'level3-mode)
      (should (eq 'disabled (mouse-trap--get-profile-for-mode))))))

;;; Error Cases

(ert-deftest test-mousetrap-mode--get-profile-for-mode-error-nil-major-mode-returns-default ()
  "Test nil major-mode returns default profile gracefully."
  (let ((major-mode nil))
    (should (eq 'disabled (mouse-trap--get-profile-for-mode)))))

(ert-deftest test-mousetrap-mode--get-profile-for-mode-error-invalid-symbol-returns-default ()
  "Test invalid major-mode symbol returns default profile."
  (let ((major-mode 'not-a-real-mode-symbol))
    (should (eq 'disabled (mouse-trap--get-profile-for-mode)))))

(provide 'test-mousetrap-mode--get-profile-for-mode)
;;; test-mousetrap-mode--get-profile-for-mode.el ends here
