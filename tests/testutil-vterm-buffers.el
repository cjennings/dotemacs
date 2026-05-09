;;; testutil-vterm-buffers.el --- Shared helpers for vterm/claude buffer tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Cleanup helpers and a fake-vterm constructor used across the
;; ai-vterm and vterm-toggle test files.  Before this module, each
;; test file re-implemented the same `(dolist (b (buffer-list))
;; (when (string-prefix-p ...) (kill-buffer b)))' loop with a
;; different prefix.

;;; Code:

(defun cj/test--kill-buffers-matching-prefix (prefix)
  "Kill all live buffers whose name starts with PREFIX."
  (dolist (b (buffer-list))
    (when (string-prefix-p prefix (buffer-name b))
      (kill-buffer b))))

(defun cj/test--kill-claude-buffers ()
  "Kill all live buffers whose name matches the AI-vterm prefix \"claude [\"."
  (cj/test--kill-buffers-matching-prefix "claude ["))

(defun cj/test--kill-test-vterm-buffers ()
  "Kill all live buffers whose name starts with \"*test-vterm\"."
  (cj/test--kill-buffers-matching-prefix "*test-vterm"))

(defun cj/test--make-fake-vterm-buffer (name)
  "Return a buffer named NAME with `major-mode' set to `vterm-mode'.

Avoids actually launching a vterm process by setting the mode
buffer-locally.  Used by tests that need a buffer satisfying the
vterm-mode predicate without the side-effects of `(vterm)'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq-local major-mode 'vterm-mode))
    buf))

(provide 'testutil-vterm-buffers)
;;; testutil-vterm-buffers.el ends here
