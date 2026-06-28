;;; test-ai-term--single-window-toggle.el --- F9 toggle round-trip when agent is the only window -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the bug where toggling off a single-window
;; agent (bury) then toggling on again redisplays the agent in a side
;; split instead of restoring the full-frame layout.
;;
;; The fix introduces a `cj/--ai-term-last-was-bury' flag set at
;; toggle-off when `one-window-p' was true.  At toggle-on the display
;; action consumes the flag and, if the frame is still single-window,
;; replaces the current window's buffer in place rather than calling
;; `display-buffer-in-direction'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'testutil-terminal-buffers)

;;; Normal Cases

(ert-deftest test-ai-term--single-window-toggle-normal-roundtrip-preserves-fullscreen ()
  "Normal: agent in the only window, F9 (off), F9 (on) -> still single window with agent.

Reproduces Craig's report.  Before the original fix the toggle-on path
fell through to `display-buffer-in-direction', which split the lone
window into two and left the agent in a side panel.  Before the
follow-up fix the toggle-off path could no-op entirely when
`bury-buffer' couldn't find a buffer to switch to, so the user saw
\"F9 does nothing\".  The dispatcher now forces the swap to a non-
agent buffer after bury so the toggle-off is observable in real and
batch use both."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [single-window-roundtrip]")
        (cj/--ai-term-last-was-bury nil)
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) agent-buf)
            (should (one-window-p))
            (let ((display-buffer-alist (cj/--ai-term-display-rule-list)))
              ;; Toggle off -- the dispatcher's force-swap should put the
              ;; window on a non-agent buffer.
              (cj/test--call-as-gui #'cj/ai-term)
              (should (one-window-p))
              (should-not (cj/--ai-term-displayed-agent-window))
              (should (eq cj/--ai-term-last-was-bury t))
              ;; Toggle on -- should restore agent in the same lone window.
              (cj/test--call-as-gui #'cj/ai-term)
              (should (one-window-p))
              (let ((win (cj/--ai-term-displayed-agent-window)))
                (should (windowp win))
                (should (eq (window-buffer win) agent-buf)))
              ;; Flag consumed by the display-saved action.
              (should-not cj/--ai-term-last-was-bury))))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--single-window-toggle-off-swaps-window-buffer ()
  "Normal: toggle-off in single-window state forces the window onto a non-
agent buffer when `bury-buffer' itself didn't swap.

Catches the regression Craig reported after the original fix shipped:
F9 in a lone-window agent did nothing visible.  The fix layer here
ensures the displayed buffer changes -- so the next F9 sees an empty
agent-window state and can route through the display-saved path."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-swap-observable]")
        (cj/--ai-term-last-was-bury nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((agent-buf (get-buffer-create agent-name))
                 (win (selected-window)))
            (set-window-buffer win agent-buf)
            (let ((display-buffer-alist (cj/--ai-term-display-rule-list)))
              (cj/test--call-as-gui #'cj/ai-term))
            (should (window-live-p win))
            (should-not (cj/--ai-term-buffer-p (window-buffer win)))))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--single-window-toggle-normal-flag-set-on-bury ()
  "Normal: single-window toggle-off sets the bury flag."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-flag-set]")
        (cj/--ai-term-last-was-bury nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) agent-buf)
            (let ((display-buffer-alist (cj/--ai-term-display-rule-list)))
              (cj/test--call-as-gui #'cj/ai-term)
              (should (eq cj/--ai-term-last-was-bury t)))))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--single-window-toggle-normal-flag-cleared-on-multi-window-off ()
  "Normal: multi-window toggle-off clears the bury flag.
Mirrors the existing `delete-window' branch of the dispatcher --
the flag should not carry over a prior bury into a delete-window
toggle-off."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-flag-clear]")
        (left-name "*test-sw-left*")
        (cj/--ai-term-last-was-bury t))    ; stale t from prior bury
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (left-buf (get-buffer-create left-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((agent-win (split-window (selected-window) nil 'right))
                   (display-buffer-alist (cj/--ai-term-display-rule-list)))
              (set-window-buffer agent-win agent-buf)
              (select-window agent-win)
              (cj/test--call-as-gui #'cj/ai-term)
              (should-not cj/--ai-term-last-was-bury))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (cj/test--kill-agent-buffers))))

;;; Boundary Cases

(ert-deftest test-ai-term--single-window-toggle-boundary-flag-respected-only-when-still-one-window ()
  "Boundary: if the frame got split between toggle-off and toggle-on, the
saved-direction split applies as usual.  The flag is a fast-path for the
genuine single-window case, not an override for every redisplay."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [flag-fallback]")
        (cj/--ai-term-last-was-bury t)        ; flag pretends prior bury
        (cj/--ai-term-last-direction 'right)
        (cj/--ai-term-last-size 40))
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
                  (display-buffer-alist (cj/--ai-term-display-rule-list)))
              (cl-letf (((symbol-function 'display-buffer-in-direction)
                         (lambda (b _a)
                           (setq received-buf b)
                           (selected-window))))
                (cj/--ai-term-display-saved agent-buf nil))
              ;; The saved-direction split path ran (display-buffer-in-direction
              ;; was called) rather than the in-place fast path.
              (should (eq received-buf agent-buf))
              ;; And the flag is cleared either way.
              (should-not cj/--ai-term-last-was-bury))))
      (when (get-buffer "*test-sw-other*") (kill-buffer "*test-sw-other*"))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--single-window-toggle-boundary-flag-not-set-when-bury-not-used ()
  "Boundary: a fresh dispatcher run with the agent displayed multi-window leaves
the flag nil (no spurious set)."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [bury-flag-untouched]")
        (cj/--ai-term-last-was-bury nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (left-buf (get-buffer-create "*test-sw-untouched-left*")))
            (set-window-buffer (selected-window) left-buf)
            (let* ((agent-win (split-window (selected-window) nil 'right))
                   (display-buffer-alist (cj/--ai-term-display-rule-list)))
              (set-window-buffer agent-win agent-buf)
              (select-window agent-win)
              (cj/test--call-as-gui #'cj/ai-term)
              (should-not cj/--ai-term-last-was-bury))))
      (when (get-buffer "*test-sw-untouched-left*")
        (kill-buffer "*test-sw-untouched-left*"))
      (cj/test--kill-agent-buffers))))

;;; Geometry tracking (Approach B: remember the agent's fullscreen state)

(ert-deftest test-ai-term--track-geometry-sole-sets-fullscreen ()
  "Normal: an agent window that is the sole window in its frame sets
`cj/--ai-term-last-fullscreen'."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [track-sole]")
        (cj/--ai-term-last-fullscreen nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) agent-buf)
            (should (one-window-p))
            (cj/--ai-term-track-geometry)
            (should (eq cj/--ai-term-last-fullscreen t))))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--track-geometry-split-clears-fullscreen ()
  "Normal: an agent window shown as a split clears `cj/--ai-term-last-fullscreen'.
The tracker must NOT re-capture dock direction/size here -- doing so on every
window change drifts the dock height per cycle; toggle-off owns that capture."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [track-split]")
        (left-name "*test-track-left*")
        (cj/--ai-term-last-fullscreen t)        ; pretend it was fullscreen
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (left-buf (get-buffer-create left-name)))
            (set-window-buffer (selected-window) left-buf)
            (let ((agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer agent-win agent-buf)
              (should-not (one-window-p))
              (cj/--ai-term-track-geometry)
              (should-not cj/--ai-term-last-fullscreen)   ; flag cleared
              (should-not cj/--ai-term-last-size))))       ; dock size NOT re-captured here
      (when (get-buffer left-name) (kill-buffer left-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--track-geometry-no-agent-retains-state ()
  "Boundary: with no agent window displayed, the tracker leaves the last-seen
fullscreen flag untouched -- that is the just-left state to replay."
  (cj/test--kill-agent-buffers)
  (let ((cj/--ai-term-last-fullscreen t))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window)
                             (get-buffer-create "*test-track-none*"))
          (should-not (cj/--ai-term-displayed-agent-window))
          (cj/--ai-term-track-geometry)
          (should (eq cj/--ai-term-last-fullscreen t)))   ; unchanged
      (when (get-buffer "*test-track-none*") (kill-buffer "*test-track-none*"))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--display-saved-restores-fullscreen-when-last-fullscreen ()
  "Normal: when the agent was last fullscreen and the target frame is a single
window, display-saved restores it in place rather than docking -- Craig's case
of leaving a fullscreen agent, switching to another fullscreen buffer, then
M-SPC.  A stale dock size is on record; the split path must NOT run."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [restore-fullscreen]")
        (cj/--ai-term-last-fullscreen t)
        (cj/--ai-term-last-was-bury nil)
        (cj/--ai-term-last-direction 'right)
        (cj/--ai-term-last-size 40))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((other-buf (get-buffer-create "*test-rfs-other*"))
                 (agent-buf (get-buffer-create agent-name))
                 (win (selected-window))
                 (split-called nil))
            (set-window-buffer win other-buf)
            (should (one-window-p))
            (cl-letf (((symbol-function 'display-buffer-in-direction)
                       (lambda (&rest _) (setq split-called t) (selected-window))))
              (cj/--ai-term-display-saved agent-buf nil))
            (should (one-window-p))                       ; no split -- stayed full-frame
            (should (eq (window-buffer win) agent-buf))   ; agent took the lone window
            (should-not split-called)))                   ; dock path never ran
      (when (get-buffer "*test-rfs-other*") (kill-buffer "*test-rfs-other*"))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--display-saved-docks-when-not-fullscreen ()
  "Boundary: without the fullscreen flag (or a bury), a single-window summon
docks via the saved-direction split.  The discriminator is the remembered
state, not merely `one-window-p', so first-open and ordinary summons still
dock rather than seizing the whole frame."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [dock-not-fullscreen]")
        (cj/--ai-term-last-fullscreen nil)
        (cj/--ai-term-last-was-bury nil)
        (cj/--ai-term-last-direction 'right)
        (cj/--ai-term-last-size 40))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((agent-buf (get-buffer-create agent-name))
                (split-called nil))
            (set-window-buffer (selected-window)
                               (get-buffer-create "*test-dock-other*"))
            (should (one-window-p))
            (cl-letf (((symbol-function 'display-buffer-in-direction)
                       (lambda (&rest _) (setq split-called t) (selected-window))))
              (cj/--ai-term-display-saved agent-buf nil))
            (should split-called)))            ; dock path ran despite one-window-p
      (when (get-buffer "*test-dock-other*") (kill-buffer "*test-dock-other*"))
      (cj/test--kill-agent-buffers))))

(provide 'test-ai-term--single-window-toggle)
;;; test-ai-term--single-window-toggle.el ends here
