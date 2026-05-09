;;; test-ai-vterm--displayed-claude-window.el --- Tests for the displayed-window helper -*- lexical-binding: t; -*-

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

(ert-deftest test-ai-vterm--displayed-claude-window-no-buffers-returns-nil ()
  "Boundary: no claude buffers anywhere -> nil."
  (cj/test--kill-claude-buffers)
  (save-window-excursion
    (delete-other-windows)
    (should-not (cj/--ai-vterm-displayed-claude-window))))

(ert-deftest test-ai-vterm--displayed-claude-window-not-displayed-returns-nil ()
  "Boundary: claude buffer exists but not in any window -> nil."
  (cj/test--kill-claude-buffers)
  (let ((b1 (get-buffer-create "claude [hidden]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (should-not (cj/--ai-vterm-displayed-claude-window)))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--displayed-claude-window-returns-window-when-displayed ()
  "Normal: claude buffer in a window -> returns that window."
  (cj/test--kill-claude-buffers)
  (let ((b1 (get-buffer-create "claude [shown]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((win (split-window-right)))
            (set-window-buffer win b1)
            (let ((result (cj/--ai-vterm-displayed-claude-window)))
              (should (windowp result))
              (should (eq (window-buffer result) b1)))))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--displayed-claude-window-ignores-non-claude-windows ()
  "Boundary: only a non-claude buffer is displayed -> nil."
  (cj/test--kill-claude-buffers)
  (let ((other (get-buffer-create "regular-displayed-buffer")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) other)
          (should-not (cj/--ai-vterm-displayed-claude-window)))
      (kill-buffer other))))

(provide 'test-ai-vterm--displayed-claude-window)
;;; test-ai-vterm--displayed-claude-window.el ends here
