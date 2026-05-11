;;; test-undead-buffers--kill-other-window-buffer.el --- Tests for cj/kill-other-window-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/kill-other-window-buffer' kills (or buries, for `cj/undead-buffer-list'
;; buffers) the buffer shown in the other window, leaving that window and the
;; split intact -- the window then shows whatever bury/kill surfaces next.
;; Sibling of `cj/kill-other-window' (which deletes the other window); the
;; distinguishing trait here is that the split is preserved.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'undead-buffers)

(ert-deftest test-undead-buffers-kill-other-window-buffer-keeps-the-window ()
  "Normal: kills the other window's (non-undead) buffer but keeps the window
and the split -- the current window is left untouched."
  (let ((other (generate-new-buffer "test-kill-other-window-buffer"))
        (orig (copy-sequence cj/undead-buffer-list)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (split-window-right)
          (let* ((win-a (selected-window))
                 (buf-a (window-buffer win-a))
                 (win-b (next-window win-a)))
            (set-window-buffer win-b other)
            (select-window win-a)
            (cj/kill-other-window-buffer)
            (should (window-live-p win-b))
            (should-not (buffer-live-p other))
            (should (eq (window-buffer win-a) buf-a))))
      (setq cj/undead-buffer-list orig)
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest test-undead-buffers-kill-other-window-buffer-buries-undead ()
  "Boundary: an undead buffer in the other window is buried, not killed --
still alive, no longer shown there; the window stays."
  (let ((other (generate-new-buffer "test-kill-other-undead"))
        (orig (copy-sequence cj/undead-buffer-list)))
    (unwind-protect
        (save-window-excursion
          (add-to-list 'cj/undead-buffer-list (buffer-name other))
          (delete-other-windows)
          (split-window-right)
          (let* ((win-a (selected-window))
                 (win-b (next-window win-a)))
            (set-window-buffer win-b other)
            (select-window win-a)
            (cj/kill-other-window-buffer)
            (should (window-live-p win-b))
            (should (buffer-live-p other))
            (should-not (eq (window-buffer win-b) other))))
      (setq cj/undead-buffer-list orig)
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest test-undead-buffers-kill-other-window-buffer-errors-with-one-window ()
  "Error: with only one window there is no other window to act on."
  (save-window-excursion
    (delete-other-windows)
    (should-error (cj/kill-other-window-buffer) :type 'user-error)))

(ert-deftest test-undead-buffers-kill-other-window-buffer-bound-under-c-semicolon-b ()
  "Normal: reachable via C-; b K."
  (require 'custom-buffer-file)
  (should (eq (keymap-lookup cj/buffer-and-file-map "K")
              #'cj/kill-other-window-buffer)))

(provide 'test-undead-buffers--kill-other-window-buffer)
;;; test-undead-buffers--kill-other-window-buffer.el ends here
