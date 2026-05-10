;;; test-vterm-copy-mode-cursor.el --- Tests for cursor visibility in vterm-copy-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; vterm's C module sets `cursor-type' to nil when the underlying TUI
;; sends DECTCEM (`\e[?25l').  Most full-screen TUIs (Claude Code, htop,
;; etc.) hide the cursor on startup.  In `vterm-copy-mode' the user is
;; navigating the buffer, not watching the TUI, so the cursor must be
;; forced visible -- the hook in `vterm-config.el' handles that.  On
;; exit, the buffer-local override is killed so the live terminal goes
;; back to the TUI's chosen cursor state.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(defvar vterm-copy-mode nil)
(require 'vterm-config)

(ert-deftest test-vterm-copy-mode-cursor-restored-on-enter ()
  "Normal: entering copy-mode with cursor-type nil sets a visible cursor."
  (with-temp-buffer
    (setq-local cursor-type nil)
    (let ((vterm-copy-mode t))
      (cj/--vterm-copy-mode-restore-cursor))
    (should (equal cursor-type '(bar . 3)))))

(ert-deftest test-vterm-copy-mode-cursor-restored-when-prior-was-box ()
  "Boundary: entering copy-mode overrides any prior cursor-type with the bar."
  (with-temp-buffer
    (setq-local cursor-type 'box)
    (let ((vterm-copy-mode t))
      (cj/--vterm-copy-mode-restore-cursor))
    (should (equal cursor-type '(bar . 3)))))

(ert-deftest test-vterm-copy-mode-cursor-override-killed-on-exit ()
  "Normal: exiting copy-mode kills the buffer-local cursor-type override."
  (with-temp-buffer
    (setq-local cursor-type '(bar . 3))
    (should (local-variable-p 'cursor-type))
    (let ((vterm-copy-mode nil))
      (cj/--vterm-copy-mode-restore-cursor))
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest test-vterm-copy-mode-cursor-hook-installed ()
  "Normal: the cursor-restoration hook is registered on vterm-copy-mode-hook."
  (should (memq #'cj/--vterm-copy-mode-restore-cursor
                vterm-copy-mode-hook)))

(provide 'test-vterm-copy-mode-cursor)
;;; test-vterm-copy-mode-cursor.el ends here
