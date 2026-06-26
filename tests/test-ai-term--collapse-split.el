;;; test-ai-term--collapse-split.el --- F9 collapses the agent split -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the F9 toggle-off behavior Craig reported: with
;; several agents alive, F9 should HIDE the agent split (collapse it back to the
;; working layout) rather than surfacing a different agent.  Two cases:
;;
;; - Multi-window: the agent occupies a split.  F9 deletes that window so the
;;   working buffer reclaims the frame -- never swaps in another agent.  The
;;   prior `quit-restore-window' path went stale after the slot was reused
;;   across agents (C-F9 switching), so it surfaced a different agent.
;; - Single-window: the agent fills the frame.  F9 returns to the most-recent
;;   NON-agent buffer (the file being worked on), not another agent -- the prior
;;   `other-buffer' call could pick another live agent.
;;
;; Also covers the `cj/--ai-term-most-recent-non-agent-buffer' helper.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'testutil-terminal-buffers)

;;; cj/--ai-term-most-recent-non-agent-buffer

(ert-deftest test-ai-term--most-recent-non-agent-buffer-skips-agents ()
  "Normal: returns a live non-agent buffer even when agents are most-recent."
  (cj/test--kill-agent-buffers)
  (let ((work (get-buffer-create "*test-mrna-work*"))
        (agent-a (get-buffer-create "agent [mrna-a]"))
        (agent-b (get-buffer-create "agent [mrna-b]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          ;; Make the agents most-recent in this window's history.
          (set-window-buffer (selected-window) work)
          (set-window-buffer (selected-window) agent-b)
          (set-window-buffer (selected-window) agent-a)
          (let ((result (cj/--ai-term-most-recent-non-agent-buffer)))
            (should (bufferp result))
            (should (buffer-live-p result))
            (should-not (cj/--ai-term-buffer-p result))))
      (when (get-buffer "*test-mrna-work*") (kill-buffer "*test-mrna-work*"))
      (cj/test--kill-agent-buffers))))

;;; Multi-window: F9 collapses the split

(ert-deftest test-ai-term--collapse-multi-window-deletes-agent-split ()
  "Normal/Regression: agent in a bottom split with other agents alive; F9
collapses the split so the working buffer reclaims the frame, and no agent is
surfaced.  Before the fix, `quit-restore-window' could switch the slot to a
different agent (stale quit-restore after slot reuse)."
  (cj/test--kill-agent-buffers)
  (let ((work (get-buffer-create "*test-collapse-work*"))
        (agent-a (get-buffer-create "agent [collapse-a]"))
        (agent-b (get-buffer-create "agent [collapse-b]"))
        (agent-c (get-buffer-create "agent [collapse-c]"))
        (cj/--ai-term-last-was-bury nil)
        ;; Isolate the layout-capture globals cj/ai-term writes on toggle-off,
        ;; so this test doesn't leak last-direction/last-size into others -- the
        ;; display-rule test splits via display-saved, which reads them.
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) work)
          (let ((agent-win (split-window (selected-window) nil 'below)))
            ;; Reuse the slot across agents (as C-F9 switching does) so the
            ;; window's prev-buffer history holds another agent.
            (set-window-buffer agent-win agent-a)
            (set-window-buffer agent-win agent-b)
            (set-window-buffer agent-win agent-c)
            (select-window agent-win)
            (should-not (one-window-p))
            (cj/test--call-as-gui #'cj/ai-term)
            (should (one-window-p))
            (should-not (cj/--ai-term-displayed-agent-window))
            (should (eq (window-buffer (selected-window)) work))))
      (when (get-buffer "*test-collapse-work*") (kill-buffer "*test-collapse-work*"))
      (cj/test--kill-agent-buffers))))

;;; Single-window: F9 returns to a non-agent buffer

(ert-deftest test-ai-term--collapse-single-window-returns-non-agent ()
  "Normal/Regression: agent fills the frame, other agents alive; F9 toggles back
to a NON-agent buffer (the working file), never another agent.  Before the fix,
`other-buffer' could pick another live agent."
  (cj/test--kill-agent-buffers)
  (let ((work (get-buffer-create "*test-collapse-sw-work*"))
        (agent-a (get-buffer-create "agent [collapse-sw-a]"))
        (agent-b (get-buffer-create "agent [collapse-sw-b]"))
        (cj/--ai-term-last-was-bury nil)
        ;; Isolate the layout-capture globals cj/ai-term writes on toggle-off,
        ;; so this test doesn't leak last-direction/last-size into others -- the
        ;; display-rule test splits via display-saved, which reads them.
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          ;; MRU: work, then agent-b, then agent-a (current).  `other-buffer'
          ;; would pick agent-b; the fix must skip it for a non-agent.
          (set-window-buffer (selected-window) work)
          (set-window-buffer (selected-window) agent-b)
          (set-window-buffer (selected-window) agent-a)
          (should (one-window-p))
          (let ((display-buffer-alist (cj/--ai-term-display-rule-list)))
            (cj/test--call-as-gui #'cj/ai-term))
          (should (one-window-p))
          (should-not (cj/--ai-term-buffer-p (window-buffer (selected-window)))))
      (when (get-buffer "*test-collapse-sw-work*") (kill-buffer "*test-collapse-sw-work*"))
      (cj/test--kill-agent-buffers))))

;;; Faithful toggle: reopen the SAME agent that was hidden

(ert-deftest test-ai-term--dispatch-prefers-last-hidden-agent ()
  "Regression: dispatch reopens the last-hidden agent, not the buffer-list MRU.
After F9 hides an agent, the next F9 must reopen the SAME one even when a
different agent is ahead of it in `buffer-list'.  Falls back to the MRU when
nothing was hidden yet or the remembered buffer was killed."
  (cj/test--kill-agent-buffers)
  (let ((a1 (get-buffer-create "agent [disp-mru]"))
        (a2 (get-buffer-create "agent [disp-shown]"))
        (cj/--ai-term-last-hidden-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-displayed-agent-window)
                   (lambda (&optional _f) nil))
                  ((symbol-function 'cj/--ai-term-agent-buffers)
                   (lambda () (list a1 a2))))      ; a1 is the MRU
          ;; No memory yet -> falls back to MRU (a1).
          (should (equal (cj/--ai-term-dispatch) (cons 'redisplay-recent a1)))
          ;; Remember a2 as last hidden -> dispatch prefers it.
          (setq cj/--ai-term-last-hidden-buffer a2)
          (should (equal (cj/--ai-term-dispatch) (cons 'redisplay-recent a2)))
          ;; A killed last-hidden buffer -> falls back to MRU.
          (let ((dead (get-buffer-create "agent [disp-dead]")))
            (setq cj/--ai-term-last-hidden-buffer dead)
            (kill-buffer dead))
          (should (equal (cj/--ai-term-dispatch) (cons 'redisplay-recent a1))))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-term--toggle-roundtrip-reopens-same-agent ()
  "Regression: hide then show brings back the agent that was on screen.
With several agents alive and a different one most-recent in `buffer-list',
F9 off then F9 on restores the SAME agent that was visible -- not a swap to
another.  Reproduces the \"the displayed buffer changes\" report."
  (cj/test--kill-agent-buffers)
  (let ((work (get-buffer-create "*test-roundtrip-work*"))
        (a1 (get-buffer-create "agent [rt-1]"))
        (a2 (get-buffer-create "agent [rt-2]"))
        (cj/--ai-term-last-was-bury nil)
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil)
        (cj/--ai-term-last-hidden-buffer nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) work)
          (let ((agent-win (split-window (selected-window) nil 'below)))
            ;; a2 is the visible agent; a1 sits ahead of it in buffer-list.
            (set-window-buffer agent-win a1)
            (bury-buffer a1)            ; a1 stays alive, demoted in MRU
            (set-window-buffer agent-win a2)
            (select-window agent-win)
            (should (eq (window-buffer (cj/--ai-term-displayed-agent-window)) a2))
            (let ((display-buffer-alist (cj/--ai-term-display-rule-list)))
              (cj/test--call-as-gui #'cj/ai-term)        ; off
              (should-not (cj/--ai-term-displayed-agent-window))
              (cj/test--call-as-gui #'cj/ai-term)        ; on -> must be a2
              (should (eq (window-buffer (cj/--ai-term-displayed-agent-window))
                          a2)))))
      (when (get-buffer "*test-roundtrip-work*") (kill-buffer "*test-roundtrip-work*"))
      (cj/test--kill-agent-buffers))))

(provide 'test-ai-term--collapse-split)
;;; test-ai-term--collapse-split.el ends here
