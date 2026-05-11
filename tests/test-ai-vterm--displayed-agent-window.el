;;; test-ai-vterm--displayed-agent-window.el --- Tests for the displayed-window helper -*- lexical-binding: t; -*-

;;; Commentary:
;; The helper returns a window in the selected frame whose buffer
;; satisfies `cj/--ai-vterm-buffer-p', or nil when no such window
;; exists.  Used by F9 dispatch and M-F9 in-place replacement.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(ert-deftest test-ai-vterm--displayed-agent-window-no-buffers-returns-nil ()
  "Boundary: no agent buffers anywhere -> nil."
  (cj/test--kill-agent-buffers)
  (save-window-excursion
    (delete-other-windows)
    (should-not (cj/--ai-vterm-displayed-agent-window))))

(ert-deftest test-ai-vterm--displayed-agent-window-not-displayed-returns-nil ()
  "Boundary: agent buffer exists but not in any window -> nil."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [hidden]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (should-not (cj/--ai-vterm-displayed-agent-window)))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--displayed-agent-window-returns-window-when-displayed ()
  "Normal: agent buffer in a window -> returns that window."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [shown]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win b1)
            (let ((result (cj/--ai-vterm-displayed-agent-window)))
              (should (windowp result))
              (should (eq (window-buffer result) b1)))))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--displayed-agent-window-ignores-non-agent-windows ()
  "Boundary: only a non-agent buffer is displayed -> nil."
  (cj/test--kill-agent-buffers)
  (let ((other (get-buffer-create "regular-displayed-buffer")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) other)
          (should-not (cj/--ai-vterm-displayed-agent-window)))
      (kill-buffer other))))

(provide 'test-ai-vterm--displayed-agent-window)
;;; test-ai-vterm--displayed-agent-window.el ends here
