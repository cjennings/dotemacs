;;; test-ai-term--reuse-existing-agent.el --- Tests for reuse-existing-agent action -*- lexical-binding: t; -*-

;;; Commentary:
;; The action looks for any window in the selected frame whose buffer
;; satisfies `cj/--ai-term-buffer-p'.  When found, swaps that
;; window's buffer for the one being displayed and returns the
;; window.  When not found, returns nil so the next action in the
;; chain runs.
;;
;; This is the action that keeps C-F9 (project-switch) from stealing
;; a non-agent window when the user is focused inside agent.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'testutil-ghostel-buffers)

(ert-deftest test-ai-term--reuse-existing-agent-swaps-buffer-when-window-exists ()
  "Normal: an agent window exists -> swap its buffer, return the window."
  (cj/test--kill-agent-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let ((existing (get-buffer-create "agent [existing]"))
          (new-buf (get-buffer-create "agent [new]"))
          (split (split-window (selected-window) nil 'right)))
      (unwind-protect
          (progn
            (set-window-buffer split existing)
            (let ((result (cj/--ai-term-reuse-existing-agent new-buf nil)))
              (should (eq result split))
              (should (eq (window-buffer split) new-buf))))
        (kill-buffer existing)
        (kill-buffer new-buf)))))

(ert-deftest test-ai-term--reuse-existing-agent-returns-nil-when-no-agent-window ()
  "Boundary: no agent window in frame -> nil (chain continues to next action)."
  (cj/test--kill-agent-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let ((new-buf (get-buffer-create "agent [no-existing]")))
      (unwind-protect
          (should (null (cj/--ai-term-reuse-existing-agent new-buf nil)))
        (kill-buffer new-buf)))))

(ert-deftest test-ai-term--reuse-existing-agent-leaves-non-agent-windows-alone ()
  "Boundary: only non-agent windows in frame -> nil; other windows untouched."
  (cj/test--kill-agent-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let ((code-buf (get-buffer-create "*test-code-buffer*"))
          (new-agent (get-buffer-create "agent [new-here]"))
          (other-win (split-window (selected-window) nil 'right)))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) code-buf)
            (set-window-buffer other-win code-buf)
            (let ((result (cj/--ai-term-reuse-existing-agent
                           new-agent nil)))
              (should (null result))
              (should (eq (window-buffer (selected-window)) code-buf))
              (should (eq (window-buffer other-win) code-buf))))
        (kill-buffer code-buf)
        (kill-buffer new-agent)))))

(ert-deftest test-ai-term--reuse-existing-agent-preserves-non-agent-window-when-swapping ()
  "Normal: swap agent window only; the other window keeps its buffer.

This is the C-F9-from-agent regression: with agent at the bottom
and code on top, switching projects must replace the bottom window's
buffer, not the top window's."
  (cj/test--kill-agent-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let* ((code-buf (get-buffer-create "*test-code-top*"))
           (agent-a (get-buffer-create "agent [a]"))
           (agent-b (get-buffer-create "agent [b]"))
           (top-win (selected-window))
           (bottom-win (split-window top-win nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer top-win code-buf)
            (set-window-buffer bottom-win agent-a)
            ;; Focus the agent window -- this is the regression scenario.
            (select-window bottom-win)
            (let ((result (cj/--ai-term-reuse-existing-agent
                           agent-b nil)))
              (should (eq result bottom-win))
              (should (eq (window-buffer bottom-win) agent-b))
              (should (eq (window-buffer top-win) code-buf))))
        (kill-buffer code-buf)
        (kill-buffer agent-a)
        (kill-buffer agent-b)))))

(provide 'test-ai-term--reuse-existing-agent)
;;; test-ai-term--reuse-existing-agent.el ends here
