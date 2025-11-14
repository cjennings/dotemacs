;;; test-integration-mousetrap-mode-lighter-click.el --- Integration tests for lighter clicking -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for mousetrap-mode lighter click functionality.
;; Tests that clicking the lighter properly toggles the mode AND
;; rebuilds the keymap based on the current major mode profile.

;;; Code:

(require 'ert)
(require 'mousetrap-mode)

;;; Integration Tests - Lighter Click Behavior

(ert-deftest test-integration-lighter-click-enables-mode-in-dashboard ()
  "Test clicking lighter in dashboard-mode enables mode with correct profile.
Dashboard uses primary-click profile which blocks scrolling but allows mouse-1."
  (with-temp-buffer
    (let ((major-mode 'dashboard-mode)
          (mouse-trap-mode nil))
      ;; Start with mode disabled
      (should-not mouse-trap-mode)

      ;; Simulate clicking lighter to enable (calls mouse-trap-mode with 1)
      (mouse-trap-mode 1)

      ;; Mode should be enabled
      (should mouse-trap-mode)

      ;; Keymap should be built for dashboard (primary-click profile)
      (should (keymapp mouse-trap-mode-map))

      ;; Verify profile-specific behavior: mouse-1 allowed, scroll blocked
      (should (eq (lookup-key mouse-trap-mode-map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key mouse-trap-mode-map (kbd "<wheel-up>")) 'ignore))

      ;; Keymap should be in minor-mode-map-alist
      (should (assq 'mouse-trap-mode minor-mode-map-alist)))))

(ert-deftest test-integration-lighter-click-disables-mode ()
  "Test clicking lighter when mode is enabled disables it and removes keymap."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (should mouse-trap-mode)
    (should (assq 'mouse-trap-mode minor-mode-map-alist))

    ;; Simulate clicking lighter to disable
    (mouse-trap-mode -1)

    ;; Mode should be disabled
    (should-not mouse-trap-mode)

    ;; Keymap should be removed from minor-mode-map-alist
    (should-not (assq 'mouse-trap-mode minor-mode-map-alist))))

(ert-deftest test-integration-lighter-click-toggle-updates-keymap ()
  "Test toggling mode via lighter click rebuilds keymap for current mode.
This is the critical test - when you click to enable, it should rebuild
the keymap based on the CURRENT major mode's profile."
  (with-temp-buffer
    (let ((major-mode 'dashboard-mode))
      ;; Start disabled
      (mouse-trap-mode -1)
      (should-not mouse-trap-mode)

      ;; Enable via click (simulate)
      (mouse-trap-mode 1)
      (should mouse-trap-mode)

      ;; Should have dashboard profile (primary-click)
      (let ((map1 mouse-trap-mode-map))
        (should (eq (lookup-key map1 (kbd "<mouse-1>")) nil))      ; allowed
        (should (eq (lookup-key map1 (kbd "<wheel-up>")) 'ignore)) ; blocked

        ;; Disable
        (mouse-trap-mode -1)
        (should-not mouse-trap-mode)

        ;; Change to different mode
        (setq major-mode 'pdf-view-mode)

        ;; Enable again
        (mouse-trap-mode 1)
        (should mouse-trap-mode)

        ;; Should now have pdf-view profile (full - all allowed)
        (let ((map2 mouse-trap-mode-map))
          (should (eq (lookup-key map2 (kbd "<mouse-1>")) nil))   ; allowed
          (should (eq (lookup-key map2 (kbd "<wheel-up>")) nil))) ; allowed now!

        ;; Verify maps are different
        (should-not (equal map1 mouse-trap-mode-map))))))

(ert-deftest test-integration-lighter-click-respects-buffer-local-mode ()
  "Test lighter click affects only current buffer (buffer-local behavior)."
  (let ((buf1 (generate-new-buffer "test1"))
        (buf2 (generate-new-buffer "test2")))
    (unwind-protect
        (progn
          ;; Buffer 1: enable mode manually
          (with-current-buffer buf1
            (setq major-mode 'text-mode)  ; Use setq to avoid hooks
            (mouse-trap-mode 1)
            (should mouse-trap-mode))

          ;; Buffer 2: mode should be independent (not auto-enabled)
          (with-current-buffer buf2
            (setq major-mode 'text-mode)  ; Use setq to avoid hooks
            (should-not mouse-trap-mode)

            ;; Enable in buf2
            (mouse-trap-mode 1)
            (should mouse-trap-mode))

          ;; Verify buf1 still enabled
          (with-current-buffer buf1
            (should mouse-trap-mode))

          ;; Disable buf2 via click
          (with-current-buffer buf2
            (mouse-trap-mode -1)
            (should-not mouse-trap-mode))

          ;; Verify buf1 unaffected
          (with-current-buffer buf1
            (should mouse-trap-mode)))

      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest test-integration-lighter-click-with-excluded-mode ()
  "Test lighter click works even in excluded modes.
Auto-enable is blocked, but manual toggle should still work."
  (with-temp-buffer
    (dired-mode default-directory)

    ;; Auto-enable is blocked for dired
    (mouse-trap-maybe-enable)
    (should-not mouse-trap-mode)

    ;; But manual toggle should work
    (mouse-trap-mode 1)
    (should mouse-trap-mode)
    (should (assq 'mouse-trap-mode minor-mode-map-alist))

    ;; Toggle off
    (mouse-trap-mode -1)
    (should-not mouse-trap-mode)
    (should-not (assq 'mouse-trap-mode minor-mode-map-alist))))

(ert-deftest test-integration-lighter-click-multiple-rapid-toggles ()
  "Test rapid clicking (multiple toggles) is stable and doesn't corrupt state."
  (with-temp-buffer
    (emacs-lisp-mode)

    ;; Rapid toggle 10 times
    (dotimes (i 10)
      (if (= (mod i 2) 0)
          (mouse-trap-mode 1)
        (mouse-trap-mode -1)))

    ;; Should end in disabled state (even number of toggles)
    (should-not mouse-trap-mode)
    (should-not (assq 'mouse-trap-mode minor-mode-map-alist))

    ;; Enable one more time to end enabled
    (mouse-trap-mode 1)
    (should mouse-trap-mode)
    (should (assq 'mouse-trap-mode minor-mode-map-alist))
    (should (keymapp mouse-trap-mode-map))))

(provide 'test-integration-mousetrap-mode-lighter-click)
;;; test-integration-mousetrap-mode-lighter-click.el ends here
