;;; test-undead-buffers--warnings-undead.el --- *Warnings* survives the buffer sweep -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs 31.1's warnings.el defers daemon-startup warnings into a one-shot
;; `after-make-frame-functions' closure that holds the *Warnings* buffer
;; object and displays it on the first client frame.  Killing that buffer
;; during startup leaves the closure holding a dead buffer, `display-buffer'
;; then signals inside `make-frame', server.el reports the window system as
;; unsupported, and emacsclient silently retries on $DISPLAY -- the first
;; frame of the session lands on XWayland instead of Wayland.
;;
;; `cj/dashboard-only' on `emacs-startup-hook' runs
;; `cj/kill-all-other-buffers-and-windows', which is exactly such a sweep.
;; These tests pin *Warnings* to the undead list so the sweep buries it
;; instead of killing it.  Error-path coverage of the predicate itself (a nil
;; or non-string name) lives in test-undead-buffers--buffer-undead-p.el.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'undead-buffers)

;;; Normal Cases

(ert-deftest test-undead-buffers-warnings-is-undead-by-default ()
  "Normal: the module's default list makes *Warnings* bury-only."
  (should (member "*Warnings*" cj/undead-buffer-list))
  (should (cj/--buffer-undead-p "*Warnings*")))

(ert-deftest test-undead-buffers-warnings-survives-kill-all-other-buffers ()
  "Normal: the startup sweep buries *Warnings* rather than killing it.
This is the sweep `cj/dashboard-only' runs from `emacs-startup-hook'."
  (delete-other-windows)
  (unwind-protect
      (let* ((main (current-buffer))
             (existing (get-buffer "*Warnings*"))
             (warnings (or existing (get-buffer-create "*Warnings*")))
             (victim (generate-new-buffer "*test-sweep-victim*")))
        (unwind-protect
            (progn
              (cj/kill-all-other-buffers-and-windows)
              (should (buffer-live-p main))
              (should (buffer-live-p warnings))
              (should-not (buffer-live-p victim)))
          (when (buffer-live-p victim) (kill-buffer victim))
          ;; Only remove what this test created.  `kill-buffer' the function
          ;; is not the remapped command, so the undead list doesn't apply.
          (when (and (not existing) (buffer-live-p warnings))
            (kill-buffer warnings))))
    (delete-other-windows)))

;;; Boundary Cases

(ert-deftest test-undead-buffers-warnings-match-is-exact ()
  "Boundary: only the exact name is undead; a uniquified *Warnings*<2> is not.
The list matches exact names, so a second warnings buffer made by
`generate-new-buffer' is an ordinary buffer to the sweep."
  (should-not (cj/--buffer-undead-p "*Warnings*<2>"))
  (should-not (cj/--buffer-undead-p " *Warnings*")))

(provide 'test-undead-buffers--warnings-undead)
;;; test-undead-buffers--warnings-undead.el ends here
