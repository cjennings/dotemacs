;;; tramp-config.el --- Tramp Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TRAMP NOTES:
;; note that you should disable your fancy prompt if connecting to the
;; remote server from tramp.  Here's what to add to the top of the file

;; [[ $TERM == "dumb" ]] && PS1='$ ' && return
;; [[ $TERM == "tramp" ]] && PS1='$ ' && return

;;; Code:

(use-package tramp
  :defer .5
  :ensure nil ;; built-in
  :config
  ;; uncomment for better debugging
  ;; (setq tramp-debug-buffer t)
  ;; (setq tramp-verbose 10)

   ;; terminal type reported by tramp to host
  (setq tramp-terminal-type "dumb")

  ;; use the path assigned to the remote user by the remote host
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; store auto-save files locally.
  (setq tramp-auto-save-directory
		(expand-file-name "tramp-auto-save" user-emacs-directory))

  ;; turn off the backup "$filename~" feature for remote files
  (setq remote-file-name-inhibit-auto-save-visited t)
  (add-to-list 'backup-directory-alist
			   (cons tramp-file-name-regexp nil))

  ;; set a more representative name for the persistency file.
  (setq tramp-persistency-file-name
		(expand-file-name "tramp-connection-history" user-emacs-directory))

  (setq tramp-copy-size-limit nil) ;; always use external program to copy
  (setq remote-file-name-inhibit-cache nil) ;; avoid cache invalidation

  ;; cache and don't expire passwords
  (setq password-cache t)
  (setq password-cache-expiry nil)

  ;; don't determine remote files vc status (for a performance gain)
  (setq vc-ignore-dir-regexp tramp-file-name-regexp))

(provide 'tramp-config)
;;; tramp-config.el ends here
