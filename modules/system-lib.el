;;; system-lib.el --- System utility library functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This module provides low-level system utility functions for checking
;; the availability of external programs and system capabilities.
;;
;; Functions include:
;; - Checking if external programs are available in PATH
;; - Silent logging to *Messages* buffer
;;
;;; Code:

(defun cj/executable-exists-p (program)
  "Return non-nil if PROGRAM is available in PATH.
PROGRAM should be a string naming an executable program."
  (and (stringp program)
       (not (string-empty-p program))
       (executable-find program)))

(defun cj/log-silently (format-string &rest args)
  "Append formatted message (FORMAT-STRING with ARGS) to *Messages* buffer.
This does so without echoing in the minibuffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (apply #'format format-string args))
      (unless (bolp) (insert "\n")))))

(provide 'system-lib)
;;; system-lib.el ends here
