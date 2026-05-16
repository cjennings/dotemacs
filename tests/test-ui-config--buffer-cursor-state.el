;;; test-ui-config--buffer-cursor-state.el --- Tests for cursor-state classification -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--buffer-cursor-state' picks the buffer-state symbol that
;; `cj/set-cursor-color-according-to-mode' maps to a cursor color via
;; `cj/buffer-status-colors'.  The subtle case: a live vterm buffer is
;; technically `buffer-read-only' (the `vterm-mode' body sets it) but the
;; user can type into it -- keystrokes go to the terminal process -- so it
;; must report a writeable state, not `read-only'.  `vterm-copy-mode' is
;; the exception: there the buffer really is a read-only Emacs buffer the
;; user navigates, so `read-only' (the orange cursor) is correct and kept.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)
(defvar vterm-copy-mode nil)
(require 'ui-config)
(require 'testutil-vterm-buffers)

(ert-deftest test-ui-config-buffer-cursor-state-readwrite-unmodified ()
  "Normal: a clean writeable buffer reports `unmodified'."
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (should (eq (cj/--buffer-cursor-state) 'unmodified))))

(ert-deftest test-ui-config-buffer-cursor-state-readwrite-modified ()
  "Normal: a writeable buffer with unsaved changes reports `modified'."
  (with-temp-buffer
    (insert "x")
    (should (eq (cj/--buffer-cursor-state) 'modified))))

(ert-deftest test-ui-config-buffer-cursor-state-read-only ()
  "Normal: a plain read-only buffer reports `read-only'."
  (with-temp-buffer
    (setq buffer-read-only t)
    (should (eq (cj/--buffer-cursor-state) 'read-only))))

(ert-deftest test-ui-config-buffer-cursor-state-overwrite ()
  "Boundary: `overwrite-mode' wins over the modified/unmodified split."
  (with-temp-buffer
    (insert "x")
    (overwrite-mode 1)
    (should (eq (cj/--buffer-cursor-state) 'overwrite))))

(ert-deftest test-ui-config-buffer-cursor-state-live-vterm-is-writeable ()
  "Boundary: a live vterm buffer is `buffer-read-only' but reports a
writeable state -- the user types into the terminal process there, so the
read-only (orange) cursor would be misleading."
  (let ((buf (cj/test--make-fake-vterm-buffer "*test-vterm-cursor-state*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-read-only t)            ; `vterm-mode' does this
          (setq-local vterm-copy-mode nil)
          (should-not (eq (cj/--buffer-cursor-state) 'read-only)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-ui-config-buffer-cursor-state-vterm-copy-mode-is-read-only ()
  "Boundary: in `vterm-copy-mode' the vterm buffer is a read-only Emacs
buffer the user navigates, so `read-only' (orange) is kept."
  (let ((buf (cj/test--make-fake-vterm-buffer "*test-vterm-cursor-state-copy*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-read-only t)
          (setq-local vterm-copy-mode t)
          (should (eq (cj/--buffer-cursor-state) 'read-only)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-ui-config-set-cursor-color-live-vterm-not-orange ()
  "Normal: in a live vterm the cursor-color hook picks a writeable color,
not the read-only orange -- even though the vterm buffer is read-only.
`display-graphic-p' is stubbed t so the function reaches its work body
in batch mode (the live function no-ops on TTY frames by design)."
  (let ((buf (cj/test--make-fake-vterm-buffer "*test-vterm-cursor-color*"))
        (applied 'unset))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-read-only t)
          (setq-local vterm-copy-mode nil)
          (let ((cj/-cursor-last-color nil)
                (cj/-cursor-last-buffer nil))
            (cl-letf (((symbol-function 'display-graphic-p) (lambda () t))
                      ((symbol-function 'set-cursor-color)
                       (lambda (c) (setq applied c))))
              (cj/set-cursor-color-according-to-mode)))
          (should (stringp applied))
          (should-not (equal applied
                             (alist-get 'read-only cj/buffer-status-colors))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'test-ui-config--buffer-cursor-state)
;;; test-ui-config--buffer-cursor-state.el ends here
