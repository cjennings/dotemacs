;;; test-system-defaults--warning-display-dead-buffer.el --- Dead-buffer guard on deferred warnings -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs 31.1's warnings.el defers daemon-startup warnings into a one-shot
;; `after-make-frame-functions' closure that calls `warning--display-buffer'
;; on the first client frame with the *Warnings* buffer object it captured at
;; warning time.  If anything killed that buffer in between, `display-buffer'
;; signals inside `make-frame', server.el swallows the error as
;; "-window-system-unsupported", and emacsclient silently retries on $DISPLAY:
;; the session's first frame lands on XWayland.
;;
;; The root fix keeps *Warnings* alive (undead-buffers.el).  This is the
;; defense in depth: `cj/warning--display-buffer-if-live' wraps
;; `warning--display-buffer' so a dead buffer is skipped rather than passed
;; on.  Load happens once in the shared sandbox (testutil-system-defaults.el);
;; `warning--display-buffer' only exists from Emacs 31, so the end-to-end case
;; skips on older builds while the pure-function cases always run.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-system-defaults)

(test-system-defaults--with-load-environment
  (test-system-defaults--load))

(defun test-system-defaults--recording-orig ()
  "Return (ORIG . CALLS) where ORIG records every argument into CALLS."
  (let ((calls (list nil)))
    (cons (lambda (buffer)
            (push buffer (car calls))
            'displayed)
          calls)))

;;; Normal Cases

(ert-deftest test-system-defaults-warning-guard-passes-live-buffer-through ()
  "Normal: a live buffer reaches the original and its value is returned."
  (let* ((rec (test-system-defaults--recording-orig))
         (buf (generate-new-buffer " *warning-guard-live*")))
    (unwind-protect
        (progn
          (should (eq 'displayed
                      (cj/warning--display-buffer-if-live (car rec) buf)))
          (should (equal (list buf) (car (cdr rec)))))
      (kill-buffer buf))))

(ert-deftest test-system-defaults-warning-guard-is-installed ()
  "Normal: loading system-defaults installs the guard on the deferred display.
From Emacs 31 the advised symbol must actually be defined: pending advice on
an undefined symbol would still count as installed, so a rename upstream
would otherwise silently disable the backstop."
  (should (advice-member-p #'cj/warning--display-buffer-if-live
                           'warning--display-buffer))
  (when (>= emacs-major-version 31)
    (should (fboundp 'warning--display-buffer))))

(ert-deftest test-system-defaults-warning-guard-resolves-live-buffer-name ()
  "Normal: a live buffer's name is resolved and passed through as the buffer."
  (let* ((rec (test-system-defaults--recording-orig))
         (buf (generate-new-buffer " *warning-guard-named*")))
    (unwind-protect
        (progn
          (should (eq 'displayed
                      (cj/warning--display-buffer-if-live
                       (car rec) (buffer-name buf))))
          (should (equal (list buf) (car (cdr rec)))))
      (kill-buffer buf))))

;;; Boundary Cases

(ert-deftest test-system-defaults-warning-guard-skips-killed-buffer ()
  "Boundary: a killed buffer never reaches the original; result is nil."
  (let* ((rec (test-system-defaults--recording-orig))
         (buf (generate-new-buffer " *warning-guard-dead*")))
    (kill-buffer buf)
    (should-not (cj/warning--display-buffer-if-live (car rec) buf))
    (should-not (car (cdr rec)))))

(ert-deftest test-system-defaults-warning-guard-end-to-end-dead-buffer-does-not-signal ()
  "Boundary: the real deferred display survives a dead buffer.
Mirrors the live failure: a string condition in `display-buffer-alist' is
what `buffer-match-p' tripped over when the buffer name came back nil."
  (skip-unless (fboundp 'warning--display-buffer))
  (let ((buf (generate-new-buffer "*Warnings*"))
        (display-buffer-alist '(("^ \\*test-guard\\*" display-buffer-no-window))))
    (kill-buffer buf)
    (should-not (warning--display-buffer buf))))

;;; Error Cases

(ert-deftest test-system-defaults-warning-guard-rejects-non-buffer ()
  "Error: nil, or a name that resolves to no buffer, is skipped without a signal."
  (let ((rec (test-system-defaults--recording-orig))
        (missing " *warning-guard-no-such-buffer*"))
    (when (get-buffer missing) (kill-buffer missing))
    (should-not (cj/warning--display-buffer-if-live (car rec) nil))
    (should-not (cj/warning--display-buffer-if-live (car rec) missing))
    (should-not (car (cdr rec)))))

(provide 'test-system-defaults--warning-display-dead-buffer)
;;; test-system-defaults--warning-display-dead-buffer.el ends here
