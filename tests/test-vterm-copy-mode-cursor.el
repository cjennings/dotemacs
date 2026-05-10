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
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(defvar vterm-copy-mode nil)
(require 'vterm-config)
(require 'vterm)

(defmacro test-vterm-copy-mode-cursor--in-fake-vterm-buffer (&rest body)
  "Run BODY in a temp buffer pretending to be a live vterm.
Stubs `vterm--enter-copy-mode' and `vterm--exit-copy-mode' so toggling
`vterm-copy-mode' doesn't try to talk to a real vterm process."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'vterm--enter-copy-mode) #'ignore)
             ((symbol-function 'vterm--exit-copy-mode) #'ignore))
     (with-temp-buffer
       (setq-local major-mode 'vterm-mode)
       ,@body)))

(ert-deftest test-vterm-copy-mode-cursor-restored-on-enter ()
  "Normal: entering copy-mode with cursor-type nil sets a visible cursor."
  (with-temp-buffer
    (setq-local cursor-type nil)
    (let ((vterm-copy-mode t))
      (cj/--vterm-copy-mode-restore-cursor))
    (should (equal cursor-type 'box))))

(ert-deftest test-vterm-copy-mode-cursor-restored-when-prior-was-hbar ()
  "Boundary: entering copy-mode overrides any prior cursor-type with the block."
  (with-temp-buffer
    (setq-local cursor-type 'hbar)
    (let ((vterm-copy-mode t))
      (cj/--vterm-copy-mode-restore-cursor))
    (should (equal cursor-type 'box))))

(ert-deftest test-vterm-copy-mode-cursor-override-killed-on-exit ()
  "Normal: exiting copy-mode kills the buffer-local cursor-type override."
  (with-temp-buffer
    (setq-local cursor-type 'box)
    (should (local-variable-p 'cursor-type))
    (let ((vterm-copy-mode nil))
      (cj/--vterm-copy-mode-restore-cursor))
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest test-vterm-copy-mode-cursor-hook-installed ()
  "Normal: the cursor-restoration hook is registered on vterm-copy-mode-hook."
  (should (memq #'cj/--vterm-copy-mode-restore-cursor
                vterm-copy-mode-hook)))

(ert-deftest test-vterm-copy-mode-cursor-end-to-end-via-mode-toggle ()
  "Normal: toggling `vterm-copy-mode' on then off via the real minor mode
command produces the visible cursor on entry and removes the override on
exit.  This exercises the full path -- mode body, hook registration, our
restore function -- not just the helper in isolation."
  (test-vterm-copy-mode-cursor--in-fake-vterm-buffer
    (setq-local cursor-type nil)
    ;; Enter copy-mode through the actual minor-mode command, not by
    ;; let-binding the variable.  This fires `vterm-copy-mode-hook'.
    (vterm-copy-mode 1)
    (should (eq vterm-copy-mode t))
    (should (equal cursor-type 'box))
    ;; Exit through the same path.
    (vterm-copy-mode -1)
    (should (eq vterm-copy-mode nil))
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest test-vterm-copy-mode-cursor-end-to-end-via-copy-done ()
  "Normal: `vterm-copy-mode-done' (M-w / RET binding) toggles copy-mode
off and triggers cursor restoration.  This is the path the user takes
most often -- copy and exit in one keystroke."
  (test-vterm-copy-mode-cursor--in-fake-vterm-buffer
    (setq-local cursor-type nil)
    (vterm-copy-mode 1)
    (should (eq vterm-copy-mode t))
    (should (equal cursor-type 'box))
    (insert "selectable text on this line")
    (set-mark (point-min))
    (goto-char (point-max))
    (vterm-copy-mode-done nil)
    (should (eq vterm-copy-mode nil))
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest test-vterm-copy-mode-cursor-end-to-end-via-cancel ()
  "Normal: `cj/vterm-copy-mode-cancel' (C-g / <escape> binding) toggles
copy-mode off and triggers cursor restoration even when no region was
selected -- the cancel path skips the kill-ring step entirely."
  (test-vterm-copy-mode-cursor--in-fake-vterm-buffer
    (setq-local cursor-type nil)
    (vterm-copy-mode 1)
    (should (equal cursor-type 'box))
    (cj/vterm-copy-mode-cancel)
    (should (eq vterm-copy-mode nil))
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest test-vterm-copy-mode-cursor-end-to-end-via-copy-done-no-region ()
  "Boundary: `vterm-copy-mode-done' called with no active region falls
into its line-selection branch.  The branch calls vterm-internal
helpers that aren't safe in a fake buffer, so stub them to point-min /
point-max.  The exit-and-fire-hook chain at the function's tail must
still run; cursor restoration must still happen."
  (cl-letf (((symbol-function 'vterm--get-beginning-of-line)
             (lambda (&rest _) (point-min)))
            ((symbol-function 'vterm--get-end-of-line)
             (lambda (&rest _) (point-max))))
    (test-vterm-copy-mode-cursor--in-fake-vterm-buffer
      (insert "line content")
      (setq-local cursor-type nil)
      (vterm-copy-mode 1)
      (should (equal cursor-type 'box))
      (deactivate-mark)
      (should-not (use-region-p))
      (vterm-copy-mode-done nil)
      (should (eq vterm-copy-mode nil))
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest test-vterm-copy-mode-cursor-survives-multiple-cycles ()
  "Boundary: enter/exit/enter/exit cycles don't accumulate buffer-local
state.  The cursor goes back and forth cleanly."
  (test-vterm-copy-mode-cursor--in-fake-vterm-buffer
    (setq-local cursor-type nil)
    (dotimes (_ 3)
      (vterm-copy-mode 1)
      (should (equal cursor-type 'box))
      (vterm-copy-mode -1)
      (should-not (local-variable-p 'cursor-type)))))

(provide 'test-vterm-copy-mode-cursor)
;;; test-vterm-copy-mode-cursor.el ends here
