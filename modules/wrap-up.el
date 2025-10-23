;;; wrapup --- Functions Run Before Init Completion -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

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
(add-hook 'emacs-startup-hook 'cj/bury-buffers-after-delay)

(cj/log-silently "<-- end of init file.")

(provide 'wrap-up)
;;; wrap-up.el ends here
