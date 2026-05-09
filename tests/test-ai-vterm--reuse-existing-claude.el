;;; test-ai-vterm--reuse-existing-claude.el --- Tests for reuse-existing-claude action -*- lexical-binding: t; -*-

;;; Commentary:
;; The action looks for any window in the selected frame whose buffer
;; satisfies `cj/--ai-vterm-buffer-p'.  When found, swaps that
;; window's buffer for the one being displayed and returns the
;; window.  When not found, returns nil so the next action in the
;; chain runs.
;;
;; This is the action that keeps C-F9 (project-switch) from stealing
;; a non-claude window when the user is focused inside claude.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(ert-deftest test-ai-vterm--reuse-existing-claude-swaps-buffer-when-window-exists ()
  "Normal: a claude window exists -> swap its buffer, return the window."
  (cj/test--kill-claude-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let ((existing (get-buffer-create "claude [existing]"))
          (new-buf (get-buffer-create "claude [new]"))
          (split (split-window (selected-window) nil 'right)))
      (unwind-protect
          (progn
            (set-window-buffer split existing)
            (let ((result (cj/--ai-vterm-reuse-existing-claude new-buf nil)))
              (should (eq result split))
              (should (eq (window-buffer split) new-buf))))
        (kill-buffer existing)
        (kill-buffer new-buf)))))

(ert-deftest test-ai-vterm--reuse-existing-claude-returns-nil-when-no-claude-window ()
  "Boundary: no claude window in frame -> nil (chain continues to next action)."
  (cj/test--kill-claude-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let ((new-buf (get-buffer-create "claude [no-existing]")))
      (unwind-protect
          (should (null (cj/--ai-vterm-reuse-existing-claude new-buf nil)))
        (kill-buffer new-buf)))))

(ert-deftest test-ai-vterm--reuse-existing-claude-leaves-non-claude-windows-alone ()
  "Boundary: only non-claude windows in frame -> nil; other windows untouched."
  (cj/test--kill-claude-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let ((code-buf (get-buffer-create "*test-code-buffer*"))
          (new-claude (get-buffer-create "claude [new-here]"))
          (other-win (split-window (selected-window) nil 'right)))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) code-buf)
            (set-window-buffer other-win code-buf)
            (let ((result (cj/--ai-vterm-reuse-existing-claude
                           new-claude nil)))
              (should (null result))
              (should (eq (window-buffer (selected-window)) code-buf))
              (should (eq (window-buffer other-win) code-buf))))
        (kill-buffer code-buf)
        (kill-buffer new-claude)))))

(ert-deftest test-ai-vterm--reuse-existing-claude-preserves-non-claude-window-when-swapping ()
  "Normal: swap claude window only; the other window keeps its buffer.

This is the C-F9-from-claude regression: with claude at the bottom
and code on top, switching projects must replace the bottom window's
buffer, not the top window's."
  (cj/test--kill-claude-buffers)
  (save-window-excursion
    (delete-other-windows)
    (let* ((code-buf (get-buffer-create "*test-code-top*"))
           (claude-a (get-buffer-create "claude [a]"))
           (claude-b (get-buffer-create "claude [b]"))
           (top-win (selected-window))
           (bottom-win (split-window top-win nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer top-win code-buf)
            (set-window-buffer bottom-win claude-a)
            ;; Focus the claude window -- this is the regression scenario.
            (select-window bottom-win)
            (let ((result (cj/--ai-vterm-reuse-existing-claude
                           claude-b nil)))
              (should (eq result bottom-win))
              (should (eq (window-buffer bottom-win) claude-b))
              (should (eq (window-buffer top-win) code-buf))))
        (kill-buffer code-buf)
        (kill-buffer claude-a)
        (kill-buffer claude-b)))))

(provide 'test-ai-vterm--reuse-existing-claude)
;;; test-ai-vterm--reuse-existing-claude.el ends here
