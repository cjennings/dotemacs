;;; system-lib.el --- System utility library functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This module provides low-level system utility functions for checking
;; the availability of external programs and system capabilities.
;;
;; Functions include:
;; - Checking if external programs are available in PATH
;;
;;; Code:

(defun cj/executable-exists-p (program)
  "Return non-nil if PROGRAM is available in PATH.
PROGRAM should be a string naming an executable program."
  (and (stringp program)
       (not (string-empty-p program))
       (executable-find program)))

(provide 'system-lib)
;;; system-lib.el ends here
