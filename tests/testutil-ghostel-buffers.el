;;; testutil-ghostel-buffers.el --- Shared helpers for ghostel/agent buffer tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Cleanup helpers and a fake-ghostel constructor used across the
;; ai-term and term-toggle test files.  Replaces the older
;; testutil-vterm-buffers helpers when the terminal engine moved from
;; vterm to ghostel.

;;; Code:

(require 'cl-lib)

(defun cj/test--call-as-gui (fn)
  "Call FN, stubbing `env-terminal-p' to return nil (a GUI frame).

The terminal refuse-guard was dropped when ghostel replaced vterm (ghostel
renders in TTY frames too), so this no longer gates behavior; it is kept as a
thin passthrough so window-behavior tests written against the old guard keep
working unchanged."
  (cl-letf (((symbol-function 'env-terminal-p) (lambda () nil)))
    (funcall fn)))

(defun cj/test--kill-buffers-matching-prefix (prefix)
  "Kill all live buffers whose name starts with PREFIX."
  (dolist (b (buffer-list))
    (when (string-prefix-p prefix (buffer-name b))
      (kill-buffer b))))

(defun cj/test--kill-agent-buffers ()
  "Kill all live buffers whose name matches the AI-term prefix \"agent [\"."
  (cj/test--kill-buffers-matching-prefix "agent ["))

(defun cj/test--kill-test-term-buffers ()
  "Kill all live buffers whose name starts with \"*test-term\"."
  (cj/test--kill-buffers-matching-prefix "*test-term"))

(defun cj/test--make-fake-ghostel-buffer (name)
  "Return a buffer named NAME with `major-mode' set to `ghostel-mode'.

Avoids actually launching a ghostel process by setting the mode
buffer-locally.  Used by tests that need a buffer satisfying the
ghostel-mode predicate without the side-effects of `(ghostel)'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq-local major-mode 'ghostel-mode))
    buf))

(defun cj/test--make-fake-eat-buffer (name)
  "Return a buffer named NAME with `major-mode' set to `eat-mode'.

Avoids actually launching an EAT process by setting the mode buffer-locally.
Used by the F12 toggle tests that need a buffer satisfying the eat-mode
predicate without the side-effects of `(eat)'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq-local major-mode 'eat-mode))
    buf))

(provide 'testutil-ghostel-buffers)
;;; testutil-ghostel-buffers.el ends here
