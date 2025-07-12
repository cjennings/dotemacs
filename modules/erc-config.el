;;; erc-config --- Preferences for Emacs Relay Chat (IRC Client) -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; The main entry point into ERC is the C-c C-c keybinding.
;; Channels in erc-autojoin-channels-alist will open in background.
;; After connecting, switch to any ERC buffer with C-c C-c.
;; Quit current channel with C-c C-p; Quit ERC altogether with C-c C-q

;;; Code:

;; ------------------------------------ ERC ------------------------------------
;; Emacs Relay Chat - an IRC client

(use-package erc
  :defer 1
  :ensure nil ;; built-in
  :commands (erc erc-tls)
  :bind
  ;; the global keybinding
  ("C-c I" . 'cj/erc-start-or-switch)
  (:map erc-mode-map
		;; overrides erc-toggle-interpret-controls
		("C-c I" . 'cj/erc-start-or-switch))
  :hook
  (erc-mode . emojify-mode)
  :custom
  (erc-modules
   '(autojoin
	 button
	 completion
	 fill
	 irccontrols
	 list
	 log
	 match
	 move-to-prompt
	 noncommands
	 notifications
	 readonly
	 services
	 stamp))

  (erc-autojoin-channels-alist
   '(("libera"
      "#erc"
	  "#ledger"
	  "#emacs"
	  "#emacs-social"
	  "#zfsonlinux"
	  "#systemcrafters"
	  "#org-mode")))

  (erc-nick "craigjennings")
  (erc-user-full-name user-whole-name)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-column 120)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)

  :config
  ;; use all text mode abbrevs in ercmode
  (abbrev-table-put erc-mode-abbrev-table :parents (list text-mode-abbrev-table))

  ;; create log directory if it doesn't exist
  (setq erc-log-channels-directory (concat user-emacs-directory "erc/logs/"))
  (if (not (file-exists-p erc-log-channels-directory))
	  (mkdir erc-log-channels-directory t)))

;; --------------------------------- ERC Image ---------------------------------
;; show inlined images (png/jpg/gif/svg) in erc buffers.

(use-package erc-image
  :defer 1
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;; -------------------------------- ERC Hl Nicks -------------------------------
;; uniquely identify names in ERC

(use-package erc-hl-nicks
  :defer 1
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

;; ------------------------------ ERC Yank To Gist -----------------------------
;; automatically create a Gist if pasting more than 5 lines
;; this module requires https://github.com/defunkt/gist
;; via ruby: 'gem install gist' via the aur: yay -S gist

(use-package erc-yank
  :defer 1
  :after erc
  :bind
  (:map erc-mode-map
		("C-y" . erc-yank)))

;; -------------------------------- Connect IRC --------------------------------
;; convenience function to auto-connect to irc.libera.chat

(defun cj/erc-start-or-switch ()
  "Start ERC or switch to ERC buffer if it has started already."
  (interactive)
  (if (get-buffer "Libera.Chat")
	  (erc-switch-to-buffer)
	(erc-tls
	 :server "irc.libera.chat"
	 :port 6697
	 :nick "craigjennings"
	 :full-name user-whole-name)))

(provide 'erc-config)
;;; erc-config.el ends here
