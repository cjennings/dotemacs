;;; dirvish-config.el --- Configuration for Tramp and the Dired File Manager -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; DIRVISH NOTES:
;; access the quick access directories by pressing 'g' (for "go")

;;; Code:

;; ----------------------------------- Dired -----------------------------------

(use-package dired
  :ensure nil ;; built-in
  :defer .5
  :bind
  (:map dired-mode-map
        ([remap dired-summary] . which-key-show-major-mode)
        ("E" . wdired-change-to-wdired-mode)           ;; edit names/properties in buffer
		("e" . cj/dired-ediff-files))                       ;; ediff files
  :custom
  (dired-use-ls-dired nil)                             ;; non GNU FreeBSD doesn't support a "--dired" switch
  :config
  (setq dired-listing-switches "-hl --almost-all --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-clean-up-buffers-too t)                  ;; offer to kill buffers associated deleted files and dirs
  (setq dired-clean-confirm-killing-deleted-buffers t) ;; don't ask whether to kill buffers associated with deleted files.
  (setq dired-kill-when-opening-new-dired-buffer t)    ;; stop littering buffers while navigating a directory tree
  (setq dired-recursive-copies (quote always))         ;; “always” means no asking
  (setq dired-recursive-deletes (quote top)))          ;; “top” means ask once

;; (add-hook 'dired-mode-hook 'auto-revert-mode)          ;; auto revert dired when files change


;; ------------------------ Dired Mark All Visible Files -----------------------
;; convenience function to mark all visible files in dired

(defun cj/dired-mark-all-visible-files ()
  "Mark all visible files for deletion in Dired mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (looking-at "^. d"))
          (dired-mark 1))
      (forward-line 1))))

;; ---------------------------------- Dirvish ----------------------------------

(use-package dirvish
  :defer .5
  :after dired
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                                 "home")
     ("rsb" "/sshx:cjennings@wolf.usbx.me:/home/cjennings/" "remote seedbox")
     ("rcj" "/sshx:cjennings@cjennings.net:~"  "remote cjennings.net")
     ("co" "~/code"                            "code")
     ("df" "~/.dotfiles/"                      "dotfiles")
     ("dn" "~/downloads/"                      "downloads")
     ("dr" "~/sync/org/drill/"                 "org drill files")
     ("dt" "~/downloads/torrents/complete/"    "torrents")
     ("dx" "~/documents/"                      "documents")
     ("gc" "~/code/golangcourse"               "golang course")
     ("lt" "~/.local/share/Trash"              "trash")
     ("mp" "~/sync/playlists/"                 "playlists")
     ("mv" "~/magic/video/"                    "magic/video")
     ("mx" "~/music/"                          "music")
     ("my" "~/magic/youtube/"                  "magic/youtube")
     ("or" "~/sync/org/"                       "sync")
     ("pl" "~/sync/playlists"                  "playlists")
     ("pr" "~/projects/"                       "projects")
     ("ps" "~/pictures/screenshots/"           "screenshots")
     ("pw" "~/pictures/wallpaper"              "wallpaper")
     ("px" "~/pictures/"                       "pictures")
     ("tg" "~/sync/org/text.games"             "text games")
     ("vx" "~/videos/"                         "videos")))
  (dirvish-attributes '(vscode-icon file-size))
  (dirvish-override-dired-mode t)
  (dirvish-preview-dispatchers '(image gif video audio epub pdf archive))
  :hook (dirvish-setup . dirvish-emerge-mode)
  :config
  (setq dirvish-use-mode-line nil)
  (setq dirvish-use-header-line nil)
  :bind
  (("C-x d"     . dirvish)
   ("C-x C-d"   . dirvish)
   ("C-x D"     . dirvish-override-dired-mode)
   ("<f11>"     . dirvish-side)

   :map dirvish-mode-map ; note: Dirvish inherits `dired-mode-map'
   ("g"       . dirvish-quick-access)
   ("G"       . revert-buffer)
   ("bg"      . (lambda () (interactive)  ; set background image
                  (shell-command (concat "nitrogen --save --set-zoom-fill "
                                         (dired-file-name-at-point) " >>/dev/null 2>&1" ))))
   ("Z"       . (lambda () (interactive) (cj/open-file-with-command "zathura")))
   ("p"       . (lambda () (interactive) (cj/open-file-with-command "gimp")))
   ("<left>"  . dired-up-directory)
   ("<right>" . dired-find-file)
   ("f"       . dirvish-file-info-menu)
   ("y"       . dirvish-yank-menu)
   ("N"       . dirvish-narrow)
   ("M"       . cj/dired-mark-all-visible-files)
   ("s"       . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"       . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB"     . dirvish-subtree-toggle)
   ("C-."     . dirvish-history-go-forward)
   ("C-,"     . dirvish-history-go-backward)
   ("M-l"     . dirvish-ls-switches-menu)
   ("M-m"     . dirvish-mark-menu)
   ("M-t"     . dirvish-layout-toggle)
   ("M-s"     . dirvish-setup-menu)
   ("M-e"     . dirvish-emerge-menu)))

;; -------------------------------- VSCode-Icons -------------------------------

(use-package vscode-icon
  :defer .5
  :commands (vscode-icon-for-file))

;; -------------------------------- Dired Rsync --------------------------------

(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map
              ("r" . dired-rsync)))

;; ---------------------------- Dired Hide Dotfiles ----------------------------

(use-package dired-hide-dotfiles
  :after dired
  :bind
  (:map  dired-mode-map
         ("h" . dired-hide-dotfiles-mode)))

;; ------------------------------- Dired Sidebar -------------------------------

(use-package dired-sidebar
  :after (dired projectile)
  :bind (("<f11>" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands) ; don't allow splitting dired window when it's showing
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)      ; don't allow rotating windows when sidebar is showing
  (setq dired-sidebar-subtree-line-prefix "  ")                    ; two spaces give simple and aesthetic indentation
  (setq dired-sidebar-no-delete-other-windows t)                   ; don't close when calling 'delete other windows'
  (setq dired-sidebar-theme 'vscode)                               ; fancy icons, please
  (setq dired-sidebar-use-custom-font 'nil)                        ; keep the same font as the rest of Emacs
  (setq dired-sidebar-delay-auto-revert-updates 'nil)              ; don't delay auto-reverting
  (setq dired-sidebar-pop-to-sidebar-on-toggle-open 'nil))         ; don't jump to sidebar when it's toggled on

;; ----------------------------- Dired Ediff Files -----------------------------
;; mark two files within dired, then ediff them within Emacs

(defun cj/dired-ediff-files ()
  "Ediff two selected files within Dired."
  (interactive)
  (let ((files (dired-get-marked-files))
		(wnd (current-window-configuration)))
	(if (<= (length files) 2)
		(let ((file1 (car files))
			  (file2 (if (cdr files)
						 (cadr files)
					   (read-file-name
						"file: "
						(dired-dwim-target-directory)))))
		  (if (file-newer-than-file-p file1 file2)
			  (ediff-files file2 file1)
			(ediff-files file1 file2))
		  (add-hook 'ediff-after-quit-hook-internal
					(lambda ()
					  (setq ediff-after-quit-hook-internal nil)
					  (set-window-configuration wnd))))
	  (error "No more than 2 files should be marked"))))

(provide 'dirvish-config)
;;; dirvish-config.el ends here
