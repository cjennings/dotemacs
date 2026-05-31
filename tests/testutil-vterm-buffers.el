;;; testutil-vterm-buffers.el --- Shared helpers for vterm/agent buffer tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Cleanup helpers and a fake-vterm constructor used across the
;; ai-vterm and vterm-toggle test files.  Before this module, each
;; test file re-implemented the same `(dolist (b (buffer-list))
;; (when (string-prefix-p ...) (kill-buffer b)))' loop with a
;; different prefix.

;;; Code:

(require 'cl-lib)

(defun cj/test--call-as-gui (fn)
  "Call FN with `env-terminal-p' stubbed to return nil (a GUI frame).

The AI-vterm interactive commands refuse to run in a terminal frame
via `cj/--ai-vterm-refuse-in-terminal'.  A batch test run is itself a
terminal frame, so tests that exercise the GUI-frame window behavior
of those commands call them through this helper to present a GUI
context."
  (cl-letf (((symbol-function 'env-terminal-p) (lambda () nil)))
    (funcall fn)))

(defun cj/test--kill-buffers-matching-prefix (prefix)
  "Kill all live buffers whose name starts with PREFIX."
  (dolist (b (buffer-list))
    (when (string-prefix-p prefix (buffer-name b))
      (kill-buffer b))))

(defun cj/test--kill-agent-buffers ()
  "Kill all live buffers whose name matches the AI-vterm prefix \"agent [\"."
  (cj/test--kill-buffers-matching-prefix "agent ["))

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
