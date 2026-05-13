;;; test-ai-vterm--single-window-toggle.el --- F9 toggle round-trip when agent is the only window -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the bug where toggling off a single-window
;; agent (bury) then toggling on again redisplays the agent in a side
;; split instead of restoring the full-frame layout.
;;
;; The fix introduces a `cj/--ai-vterm-last-was-bury' flag set at
;; toggle-off when `one-window-p' was true.  At toggle-on the display
;; action consumes the flag and, if the frame is still single-window,
;; replaces the current window's buffer in place rather than calling
;; `display-buffer-in-direction'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

;;; Normal Cases

(ert-deftest test-ai-vterm--single-window-toggle-normal-roundtrip-preserves-fullscreen ()
  "Normal: agent in the only window, simulated bury, F9 (on) -> still single window with agent.

Reproduces Craig's report.  Before the fix the toggle-on path fell
through to `display-buffer-in-direction', which split the lone window
into two and left the agent in a side panel.

The bury step is simulated (set flag + swap window buffer to a non-
agent buffer) because batch-mode `bury-buffer' won't switch the
displayed buffer on a window with empty prev-buffers; the toggle-off
branch's *logic* is covered by the flag-set-on-bury test."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [single-window-roundtrip]")
        (other-name "*test-sw-roundtrip-other*")
        (cj/--ai-vterm-last-was-bury nil)
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (other-buf (get-buffer-create other-name)))
            (set-window-buffer (selected-window) agent-buf)
            (should (one-window-p))
            ;; Simulate the toggle-off bury path: capture state, set the
            ;; bury flag, and put a non-agent buffer in the window where
            ;; the real bury would have left one.  This isolates the
            ;; toggle-on behaviour without depending on batch-mode
            ;; `bury-buffer' (which is unreliable with empty prev-buffers).
            (cj/--ai-vterm-capture-state (selected-window))
            (setq cj/--ai-vterm-last-was-bury t)
            (set-window-buffer (selected-window) other-buf)
            (should (one-window-p))
            (should-not (cj/--ai-vterm-displayed-agent-window))
            ;; Toggle on -- should restore agent in the same lone window.
            (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
              (cj/ai-vterm))
            (should (one-window-p))
            (let ((win (cj/--ai-vterm-displayed-agent-window)))
              (should (windowp win))
              (should (eq (window-buffer win) agent-buf)))
            ;; Flag must be consumed by the display-saved action.
            (should-not cj/--ai-vterm-last-was-bury)))
      (when (get-buffer other-name) (kill-buffer other-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--single-window-toggle-normal-flag-set-on-bury ()
  "Normal: single-window toggle-off sets the bury flag."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-flag-set]")
        (cj/--ai-vterm-last-was-bury nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) agent-buf)
            (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
              (cj/ai-vterm)
              (should (eq cj/--ai-vterm-last-was-bury t)))))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--single-window-toggle-normal-flag-cleared-on-multi-window-off ()
  "Normal: multi-window toggle-off clears the bury flag.
Mirrors the existing `delete-window' branch of the dispatcher --
the flag should not carry over a prior bury into a delete-window
toggle-off."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-flag-clear]")
        (left-name "*test-sw-left*")
        (cj/--ai-vterm-last-was-bury t))    ; stale t from prior bury
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (left-buf (get-buffer-create left-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((agent-win (split-window (selected-window) nil 'right))
                   (display-buffer-alist (cj/--ai-vterm-display-rule-list)))
              (set-window-buffer agent-win agent-buf)
              (select-window agent-win)
              (cj/ai-vterm)
              (should-not cj/--ai-vterm-last-was-bury))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (cj/test--kill-agent-buffers))))

;;; Boundary Cases

(ert-deftest test-ai-vterm--single-window-toggle-boundary-flag-respected-only-when-still-one-window ()
  "Boundary: if the frame got split between toggle-off and toggle-on, the
saved-direction split applies as usual.  The flag is a fast-path for the
genuine single-window case, not an override for every redisplay."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [flag-fallback]")
        (cj/--ai-vterm-last-was-bury t)        ; flag pretends prior bury
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 40))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((other-buf (get-buffer-create "*test-sw-other*"))
                 (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) other-buf)
            ;; Frame is split (two windows) -- single-window precondition
            ;; for the flag no longer holds.
            (split-window-right)
            (should-not (one-window-p))
            (let (received-buf
                  (display-buffer-alist (cj/--ai-vterm-display-rule-list)))
              (cl-letf (((symbol-function 'display-buffer-in-direction)
                         (lambda (b _a)
                           (setq received-buf b)
                           (selected-window))))
                (cj/--ai-vterm-display-saved agent-buf nil))
              ;; The saved-direction split path ran (display-buffer-in-direction
              ;; was called) rather than the in-place fast path.
              (should (eq received-buf agent-buf))
              ;; And the flag is cleared either way.
              (should-not cj/--ai-vterm-last-was-bury))))
      (when (get-buffer "*test-sw-other*") (kill-buffer "*test-sw-other*"))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--single-window-toggle-boundary-flag-not-set-when-bury-not-used ()
  "Boundary: a fresh dispatcher run with the agent displayed multi-window leaves
the flag nil (no spurious set)."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-flag-untouched]")
        (cj/--ai-vterm-last-was-bury nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (left-buf (get-buffer-create "*test-sw-untouched-left*")))
            (set-window-buffer (selected-window) left-buf)
            (let* ((agent-win (split-window (selected-window) nil 'right))
                   (display-buffer-alist (cj/--ai-vterm-display-rule-list)))
              (set-window-buffer agent-win agent-buf)
              (select-window agent-win)
              (cj/ai-vterm)
              (should-not cj/--ai-vterm-last-was-bury))))
      (when (get-buffer "*test-sw-untouched-left*")
        (kill-buffer "*test-sw-untouched-left*"))
      (cj/test--kill-agent-buffers))))

(provide 'test-ai-vterm--single-window-toggle)
;;; test-ai-vterm--single-window-toggle.el ends here
