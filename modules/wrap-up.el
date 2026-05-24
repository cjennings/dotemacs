;;; wrapup --- Functions Run Before Init Completion -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: S.
;; Load shape: eager.
;; Eager reason: runs end-of-startup cleanup, burying the scratch/dashboard
;;   buffers via a short startup timer.
;; Top-level side effects: a one-shot startup timer that buries buffers.
;; Runtime requires: system-lib.
;; Direct test load: yes.
;;
;;; Code:

(require 'system-lib)

;; -------------------------------- Bury Buffers -------------------------------
;; wait a few seconds then bury compile-related buffers.

(defun cj/bury-buffers ()
  "Bury comint and compilation buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
	  (when (or (derived-mode-p 'comint-mode)
				(derived-mode-p 'compilation-mode)
				(derived-mode-p 'debugger-mode)
				(derived-mode-p 'elisp-compile-mode)
				(derived-mode-p 'messages-buffer-mode)
				) ;; byte-compilations
        (bury-buffer)))))

(defun cj/bury-buffers-after-delay ()
  "Run cj/bury-buffers after a delay."
  (run-with-timer 1 nil 'cj/bury-buffers))
(unless noninteractive
  (add-hook 'emacs-startup-hook 'cj/bury-buffers-after-delay))

(cj/log-silently "<-- end of init file.")

(provide 'wrap-up)
;;; wrap-up.el ends here
