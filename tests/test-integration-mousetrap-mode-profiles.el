;;; test-integration-mousetrap-mode-profiles.el --- Integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for mousetrap-mode profile system.
;; Tests complete workflows including profile lookup, keymap building,
;; mode activation, inheritance, and dynamic reconfiguration.
;;
;; Components integrated:
;; - mouse-trap--get-profile-for-mode (profile lookup)
;; - mouse-trap--build-keymap (keymap generation)
;; - mouse-trap-mode (minor mode activation)
;; - derived-mode-p (Emacs mode inheritance)
;; - mouse-trap-maybe-enable (auto-activation logic)

;;; Code:

(require 'ert)
(require 'mousetrap-mode)

;;; Integration Tests - Normal Workflows

(ert-deftest test-integration-mousetrap-mode-profiles-org-mode-inherits-text-mode-disabled ()
  "Test org-mode inherits disabled profile from text-mode.

Components integrated:
- mouse-trap--get-profile-for-mode (lookup with inheritance)
- derived-mode-p (mode hierarchy checking)
- org-mode (real major mode)

Validates:
- Mode inheritance chain works correctly
- org-mode → text-mode → disabled profile"
  (with-temp-buffer
    (org-mode)
    (let ((profile (mouse-trap--get-profile-for-mode)))
      (should (eq 'disabled profile)))))

(ert-deftest test-integration-mousetrap-mode-profiles-pdf-view-full-allows-all-events ()
  "Test pdf-view-mode gets full profile with all events allowed.

Components integrated:
- mouse-trap--get-profile-for-mode (exact match lookup)
- mouse-trap--build-keymap (full profile keymap)

Validates:
- Full profile configuration
- All event categories allowed (empty/minimal keymap)"
  (let ((major-mode 'pdf-view-mode))
    (let ((profile (mouse-trap--get-profile-for-mode))
          (map (mouse-trap--build-keymap)))
      (should (eq 'full profile))
      (should (keymapp map))
      ;; All events should be allowed (not bound)
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) nil)))))

(ert-deftest test-integration-mousetrap-mode-profiles-dashboard-primary-click-only ()
  "Test dashboard-mode gets primary-click profile.

Components integrated:
- mouse-trap--get-profile-for-mode (lookup)
- mouse-trap--build-keymap (selective event binding)

Validates:
- Primary-click profile allows mouse-1
- Blocks mouse-2/3 and scroll events"
  (let ((major-mode 'dashboard-mode))
    (let ((profile (mouse-trap--get-profile-for-mode))
          (map (mouse-trap--build-keymap)))
      (should (eq 'primary-click profile))
      ;; mouse-1 allowed
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      ;; mouse-2/3 blocked
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore))
      ;; scroll blocked
      (should (eq (lookup-key map (kbd "<wheel-up>")) 'ignore)))))

(ert-deftest test-integration-mousetrap-mode-profiles-emacs-lisp-uses-default-disabled ()
  "Test unmapped mode uses default disabled profile.

Components integrated:
- mouse-trap--get-profile-for-mode (fallback to default)
- mouse-trap--build-keymap (disabled keymap)

Validates:
- Default profile fallback works
- All events blocked by default"
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((profile (mouse-trap--get-profile-for-mode))
          (map (mouse-trap--build-keymap)))
      (should (eq 'disabled profile))
      ;; All events blocked
      (should (eq (lookup-key map (kbd "<wheel-up>")) 'ignore))
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore)))))

(ert-deftest test-integration-mousetrap-mode-profiles-change-profile-no-reload ()
  "Test changing profiles and re-enabling mode without Emacs reload.

Components integrated:
- mouse-trap--get-profile-for-mode (re-reads configuration)
- mouse-trap--build-keymap (rebuilds dynamically)
- mouse-trap-mode (mode toggle)

Validates:
- KEY FEATURE: Dynamic reconfiguration
- Profile changes take effect without reload"
  (let ((original-profiles mouse-trap-mode-profiles))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          ;; Start with unmapped mode (gets default scroll-only)
          (setq mouse-trap-mode-profiles nil)
          (mouse-trap-mode 1)
          (let ((map mouse-trap-mode-map))
            (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore)))
          (mouse-trap-mode -1)

          ;; Change configuration
          (setq mouse-trap-mode-profiles '((emacs-lisp-mode . full)))

          ;; Re-enable and verify new profile
          (mouse-trap-mode 1)
          (let ((map mouse-trap-mode-map))
            ;; Full profile - all events allowed
            (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
            (should (eq (lookup-key map (kbd "<wheel-up>")) nil))))
      ;; Restore original configuration
      (setq mouse-trap-mode-profiles original-profiles))))

(ert-deftest test-integration-mousetrap-mode-profiles-switch-major-mode-updates-profile ()
  "Test switching major-mode and re-enabling updates profile.

Components integrated:
- mouse-trap--get-profile-for-mode (mode-sensitive lookup)
- Major mode switching
- Mode re-activation

Validates:
- Profile changes with major-mode
- Mode-sensitive behavior"
  (with-temp-buffer
    (text-mode)
    (mouse-trap-mode 1)
    (let ((map1 mouse-trap-mode-map))
      ;; text-mode = disabled (inherits from default), all blocked
      (should (eq (lookup-key map1 (kbd "<wheel-up>")) 'ignore))
      (should (eq (lookup-key map1 (kbd "<mouse-1>")) 'ignore))
      (mouse-trap-mode -1))

    ;; Switch to pdf-view-mode which has full profile
    (setq major-mode 'pdf-view-mode)
    (mouse-trap-mode 1)
    (let ((map2 mouse-trap-mode-map))
      ;; pdf-view-mode = full, all events allowed
      (should (eq (lookup-key map2 (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map2 (kbd "<mouse-1>")) nil)))))

(ert-deftest test-integration-mousetrap-mode-profiles-auto-enable-respects-exclusions ()
  "Test auto-enable respects exclusion list.

Components integrated:
- mouse-trap-maybe-enable (auto-activation logic)
- mouse-trap-excluded-modes (exclusion list)
- derived-mode-p (mode checking)

Validates:
- Exclusion list prevents auto-activation
- dired-mode is excluded"
  (with-temp-buffer
    (dired-mode default-directory)
    ;; Manually call maybe-enable
    (mouse-trap-maybe-enable)
    ;; Should NOT enable
    (should-not mouse-trap-mode)))

(ert-deftest test-integration-mousetrap-mode-profiles-manual-enable-in-excluded-mode ()
  "Test manual activation works in excluded modes.

Components integrated:
- mouse-trap-mode (manual activation)
- Exclusion list (should not affect manual activation)

Validates:
- Manual activation bypasses auto-enable exclusions
- Exclusions only affect hooks, not manual toggling"
  (with-temp-buffer
    (dired-mode default-directory)
    ;; Manually enable
    (mouse-trap-mode 1)
    ;; Should be enabled despite being in exclusion list
    (should mouse-trap-mode)))

;;; Integration Tests - Boundary Cases

(ert-deftest test-integration-mousetrap-mode-profiles-markdown-inherits-text-disabled ()
  "Test markdown-mode inherits disabled profile from text-mode.

Components integrated:
- Mode inheritance (markdown-mode → text-mode)
- Profile lookup with inheritance

Validates:
- Multi-level inheritance works
- Markdown gets disabled profile"
  (with-temp-buffer
    (when (fboundp 'markdown-mode)
      (markdown-mode)
      (let ((profile (mouse-trap--get-profile-for-mode)))
        (should (eq 'disabled profile))))))

(ert-deftest test-integration-mousetrap-mode-profiles-help-mode-inherits-special-disabled ()
  "Test help-mode inherits disabled from special-mode.

Components integrated:
- Mode inheritance (help-mode → special-mode)
- Profile lookup

Validates:
- special-mode inheritance works
- Help buffers get disabled profile"
  (with-temp-buffer
    (help-mode)
    (let ((profile (mouse-trap--get-profile-for-mode)))
      (should (eq 'disabled profile)))))

(ert-deftest test-integration-mousetrap-mode-profiles-toggle-multiple-times ()
  "Test toggling mode multiple times is stable.

Components integrated:
- mouse-trap-mode (activation/deactivation)
- Keymap building (multiple times)

Validates:
- Mode toggle robustness
- No errors on rapid toggling"
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Toggle multiple times
    (dotimes (_ 5)
      (mouse-trap-mode 1)
      (should mouse-trap-mode)
      (mouse-trap-mode -1)
      (should-not mouse-trap-mode))))

(ert-deftest test-integration-mousetrap-mode-profiles-multiple-buffers-independent ()
  "Test multiple buffers have independent profiles.

Components integrated:
- Buffer-local mode behavior
- Profile lookup per buffer
- Multiple mode activation

Validates:
- Buffer-local mode isolation
- Each buffer gets correct profile"
  (let ((buf1 (generate-new-buffer "test1"))
        (buf2 (generate-new-buffer "test2")))
    (unwind-protect
        (progn
          ;; Buffer 1: text-mode (disabled = default)
          (with-current-buffer buf1
            (text-mode)
            (mouse-trap-mode 1)
            (should mouse-trap-mode)
            (let ((map1 mouse-trap-mode-map))
              (should (eq (lookup-key map1 (kbd "<wheel-up>")) 'ignore))
              (should (eq (lookup-key map1 (kbd "<mouse-1>")) 'ignore))))

          ;; Buffer 2: pdf-view-mode (full profile)
          (with-current-buffer buf2
            (setq major-mode 'pdf-view-mode)
            (mouse-trap-mode 1)
            (should mouse-trap-mode)
            (let ((map2 mouse-trap-mode-map))
              ;; All events allowed
              (should (eq (lookup-key map2 (kbd "<wheel-up>")) nil))
              (should (eq (lookup-key map2 (kbd "<mouse-1>")) nil)))))

      ;; Cleanup
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; Integration Tests - Edge Cases

(ert-deftest test-integration-mousetrap-mode-profiles-change-default-profile ()
  "Test changing default profile takes effect.

Components integrated:
- mouse-trap-default-profile (configuration)
- Profile fallback logic
- Dynamic reconfiguration

Validates:
- Default profile configuration works
- Changes take effect on re-enable"
  (let ((original-default mouse-trap-default-profile)
        (original-profiles mouse-trap-mode-profiles))
    (unwind-protect
        (with-temp-buffer
          ;; Unmapped mode uses default
          (setq mouse-trap-mode-profiles nil)
          (setq mouse-trap-default-profile 'disabled)
          (emacs-lisp-mode)
          (mouse-trap-mode 1)
          (let ((map1 mouse-trap-mode-map))
            ;; Default = disabled, all blocked
            (should (eq (lookup-key map1 (kbd "<wheel-up>")) 'ignore))
            (should (eq (lookup-key map1 (kbd "<mouse-1>")) 'ignore))
            (mouse-trap-mode -1))

          ;; Change default
          (setq mouse-trap-default-profile 'full)
          (mouse-trap-mode 1)
          (let ((map2 mouse-trap-mode-map))
            ;; Default = full, all allowed
            (should (eq (lookup-key map2 (kbd "<wheel-up>")) nil))
            (should (eq (lookup-key map2 (kbd "<mouse-1>")) nil))))
      ;; Restore original configuration
      (setq mouse-trap-default-profile original-default)
      (setq mouse-trap-mode-profiles original-profiles))))

(ert-deftest test-integration-mousetrap-mode-profiles-add-new-profile-runtime ()
  "Test adding new profile at runtime.

Components integrated:
- mouse-trap-profiles (extensibility)
- Profile lookup
- Runtime configuration

Validates:
- Runtime extensibility
- New profiles work immediately"
  (let ((original-profiles mouse-trap-profiles)
        (original-mode-profiles mouse-trap-mode-profiles))
    (unwind-protect
        (with-temp-buffer
          (setq mouse-trap-profiles
                (append mouse-trap-profiles
                        '((custom-profile . (primary-click scroll)))))
          (setq mouse-trap-mode-profiles '((emacs-lisp-mode . custom-profile)))
          (emacs-lisp-mode)
          (mouse-trap-mode 1)
          (let ((map mouse-trap-mode-map))
            ;; Custom profile: primary-click and scroll allowed
            (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
            (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
            ;; Secondary click blocked
            (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore))))
      ;; Restore original configuration
      (setq mouse-trap-profiles original-profiles)
      (setq mouse-trap-mode-profiles original-mode-profiles))))

(ert-deftest test-integration-mousetrap-mode-profiles-remove-mode-mapping-uses-default ()
  "Test removing mode mapping falls back to default.

Components integrated:
- Profile lookup fallback
- Dynamic configuration

Validates:
- Graceful handling of removed mappings
- Fallback to default profile"
  (with-temp-buffer
    (let ((mouse-trap-mode-profiles nil)  ; Dashboard not mapped
          (mouse-trap-default-profile 'scroll-only)
          (major-mode 'dashboard-mode))
      (let ((profile (mouse-trap--get-profile-for-mode)))
        ;; Should fall back to default
        (should (eq 'scroll-only profile))))))

(provide 'test-integration-mousetrap-mode-profiles)
;;; test-integration-mousetrap-mode-profiles.el ends here
