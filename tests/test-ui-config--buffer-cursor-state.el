;;; test-ui-config--buffer-cursor-state.el --- Tests for cursor-state classification -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/buffer-status-state' picks the buffer-state symbol the modeline
;; buffer-name indicator maps to a face via `cj/buffer-status-color'.  The
;; subtle case: a live ghostel terminal is
;; technically `buffer-read-only' but the user types into it -- keystrokes go
;; to the terminal process -- so it must report a writeable state, not
;; `read-only'.  ghostel's `copy' / `emacs' input modes are the exception:
;; there the buffer really is a read-only Emacs buffer the user navigates, so
;; `read-only' (the orange cursor) is correct and kept.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)
(defvar ghostel--input-mode nil)
(require 'ui-config)
(require 'testutil-ghostel-buffers)

(ert-deftest test-ui-config-buffer-cursor-state-readwrite-unmodified ()
  "Normal: a clean writeable buffer reports `unmodified'."
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (should (eq (cj/buffer-status-state) 'unmodified))))

(ert-deftest test-ui-config-buffer-cursor-state-readwrite-modified ()
  "Normal: a writeable buffer with unsaved changes reports `modified'."
  (with-temp-buffer
    (insert "x")
    (should (eq (cj/buffer-status-state) 'modified))))

(ert-deftest test-ui-config-buffer-cursor-state-read-only ()
  "Normal: a plain read-only buffer reports `read-only'."
  (with-temp-buffer
    (setq buffer-read-only t)
    (should (eq (cj/buffer-status-state) 'read-only))))

(ert-deftest test-ui-config-buffer-cursor-state-overwrite ()
  "Boundary: `overwrite-mode' wins over the modified/unmodified split."
  (with-temp-buffer
    (insert "x")
    (overwrite-mode 1)
    (should (eq (cj/buffer-status-state) 'overwrite))))

(ert-deftest test-ui-config-buffer-cursor-state-live-ghostel-is-writeable ()
  "Boundary: a live ghostel buffer is `buffer-read-only' but reports a
writeable state -- the user types into the terminal process there, so the
read-only (orange) cursor would be misleading."
  (let ((buf (cj/test--make-fake-ghostel-buffer "*test-ghostel-cursor-state*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-read-only t)            ; ghostel keeps the buffer read-only
          (setq-local ghostel--input-mode 'semi-char)
          (should-not (eq (cj/buffer-status-state) 'read-only)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-ui-config-buffer-cursor-state-ghostel-copy-mode-is-read-only ()
  "Boundary: in ghostel `copy' mode the buffer is a read-only Emacs buffer
the user navigates, so `read-only' (orange) is kept."
  (let ((buf (cj/test--make-fake-ghostel-buffer "*test-ghostel-cursor-state-copy*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-read-only t)
          (setq-local ghostel--input-mode 'copy)
          (should (eq (cj/buffer-status-state) 'read-only)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'test-ui-config--buffer-cursor-state)
;;; test-ui-config--buffer-cursor-state.el ends here
