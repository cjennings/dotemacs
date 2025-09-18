;;; dirvish-config.el --- Dired/Dirvish Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; DIRVISH NOTES:
;; access the quick access directories by pressing 'g' (for "go")

;;; Code:

(require 'user-constants)

;;; ----------------------------------- Dired -----------------------------------

(use-package dired
  :ensure nil ;; built-in
  :defer t
  :bind
  (:map dired-mode-map
		([remap dired-summary] . which-key-show-major-mode)
		("E" . wdired-change-to-wdired-mode) ;; edit names and properties in buffer
		("e" . cj/dired-ediff-files))        ;; ediff files
  :custom
  (dired-use-ls-dired nil)                             ;; non GNU FreeBSD doesn't support a "--dired" switch
  :config
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-clean-up-buffers-too t)                  ;; offer to kill buffers associated deleted files and dirs
  (setq dired-clean-confirm-killing-deleted-buffers t) ;; don't ask; just kill buffers associated with deleted files
  (setq dired-recursive-copies (quote always))         ;; “always” means no asking
  (setq dired-recursive-deletes (quote top)))          ;; “top” means ask once

;; note: disabled as it prevents marking and moving files to another directory
;; (setq dired-kill-when-opening-new-dired-buffer t)   ;; don't litter by leaving buffers when navigating directories

(add-hook 'dired-mode-hook 'auto-revert-mode)          ;; auto revert dired when files change

;;; --------------------------- Dired Open HTML In EWW --------------------------

(defun cj/dirvish-open-html-in-eww ()
  "Open HTML file at point in dired/dirvish using eww."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
	(if (string-match-p "\\.html?\\'" file)
		(eww-open-file file)
	  (message "Not an HTML file: %s" file))))

;;; -------------------------- Dired Copy Path As Kill --------------------------

(defun cj/dired-copy-path-as-kill ()
  "Copy the full path of file at point in Dired to the clipboard."
  (interactive)
  (let ((filename (dired-get-file-for-visit)))
	(if (and filename (file-exists-p filename))
		(progn
		  (kill-new filename)
		  (message "Copied '%s' to clipboard." filename))
	  (message "No file at point."))))

;;; ------------------------ Dired Mark All Visible Files -----------------------

(defun cj/dired-mark-all-visible-files ()
  "Mark all visible files in Dired mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (looking-at "^. d"))
          (dired-mark 1))
      (forward-line 1))))

;;; ----------------------- Dirvish Open File Manager Here ----------------------

(defun cj/dirvish-open-file-manager-here ()
  "Open system's default file manager in the current dired/dirvish directory.
Always opens the file manager in the directory currently being displayed,
regardless of what file or subdirectory the point is on."
  (interactive)
  (let ((current-dir (dired-current-directory)))
	(if (and current-dir (file-exists-p current-dir))
		(progn
		  (message "Opening file manager in %s..." current-dir)
		  ;; Use shell-command with & to run asynchronously and detached
		  (let ((process-connection-type nil)) ; Use pipe instead of pty
			(cond
			 ;; Linux/Unix with xdg-open
			 ((executable-find "xdg-open")
			  (call-process "xdg-open" nil 0 nil current-dir))
			 ;; macOS
			 ((eq system-type 'darwin)
			  (call-process "open" nil 0 nil current-dir))
			 ;; Windows
			 ((eq system-type 'windows-nt)
			  (call-process "explorer" nil 0 nil current-dir))
			 ;; Fallback to shell-command
			 (t
			  (shell-command (format "xdg-open %s &"
									 (shell-quote-argument current-dir)))))))
	  (message "Could not determine current directory."))))

;;; ---------------------------------- Dirvish ----------------------------------

(use-package dirvish
  :defer 1
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; This MUST be in :custom section, not :config
  (dirvish-quick-access-entries
   `(("h"  "~/"                                             "home")
	 ("cx" ,code-dir                                        "code directory")
	 ("ex" ,user-emacs-directory                            "emacs home")
	 ("es" ,sounds-dir                                      "notification sounds")
	 ("ra" ,video-recordings-dir                            "video recordings")
	 ("rv" ,audio-recordings-dir                            "audio recordings")
	 ("dl" ,dl-dir                                          "downloads")
	 ("dr" ,(concat sync-dir "/drill/")                     "drill files")
	 ("dt" ,(concat dl-dir "/torrents/complete/")           "torrents")
	 ("dx" "~/documents/"                                   "documents")
	 ("lx" "~/lectures/"                                    "lectures")
	 ("mb" "/media/backup/"                                 "backup directory")
	 ("mx" "~/music/"                                       "music")
	 ("pD" "~/projects/documents/"                          "project documents")
	 ("pd" "~/projects/danneel/"                            "project danneel")
	 ("pl" "~/projects/elibrary/"                           "project elibrary")
	 ("pf" "~/projects/finances/"                           "project finances")
	 ("pjr" "~/projects/jr-estate/"                         "project jr-estate")
	 ("ps" ,(concat pix-dir "/screenshots/")                "pictures screenshots")
	 ("pw" ,(concat pix-dir "/wallpaper/")                  "pictures wallpaper")
	 ("px" ,pix-dir                                         "pictures directory")
	 ("rcj" "/sshx:cjennings@cjennings.net:~"               "remote cjennings.net")
	 ("rsb" "/sshx:cjennings@wolf.usbx.me:/home/cjennings/" "remote seedbox")
	 ("sx" ,sync-dir                                        "sync directory")
	 ("so" "~/sync/org"                                     "org directory")
	 ("sv" "~/sync/videos/"                                 "sync/videos directory")
	 ("tg" ,(concat sync-dir "/text.games")                 "text games")
	 ("vr" ,video-recordings-dir                            "video recordings directory")
	 ("vx" ,videos-dir                                      "videos")))
  :config
  ;; Add the extensions directory to load-path
  (let ((extensions-dir (expand-file-name "extensions"
										  (file-name-directory (locate-library "dirvish")))))
	(when (file-directory-p extensions-dir)
	  (add-to-list 'load-path extensions-dir)))

  ;; Load dirvish modules with error checking
  (let ((dirvish-modules '(dirvish-emerge
						   dirvish-subtree
						   dirvish-narrow
						   dirvish-history
						   dirvish-ls
						   dirvish-yank
						   dirvish-quick-access
						   dirvish-collapse
						   dirvish-rsync
						   dirvish-vc
						   dirvish-icons
						   dirvish-side
						   dirvish-peek)))
	(dolist (module dirvish-modules)
	  (condition-case err
		  (require module)
		(error
		 (message "Failed to load %s: %s" module (error-message-string err))))))

  ;; Enable peek mode with error checking
  (condition-case err
	  (dirvish-peek-mode 1)
	(error (message "Failed to enable dirvish-peek-mode: %s" (error-message-string err))))

  ;; Enable side-follow mode with error checking
  (condition-case err
	  (dirvish-side-follow-mode 1)
	(error (message "Failed to enable dirvish-side-follow-mode: %s"
					(error-message-string err))))

  ;; Your other configuration settings
  (setq dirvish-attributes '(nerd-icons file-size))
  (setq dirvish-preview-dispatchers '(image gif video audio epub pdf archive))
  (setq dirvish-use-mode-line nil)
  (setq dirvish-use-header-line nil)
  :bind
  (("C-x d"   . dirvish)
   ("C-x C-d" . dirvish)
   ("C-x D"   . dirvish)
   ("<f11>"   . dirvish-side)
   :map dirvish-mode-map
   ("bg"      . (lambda () (interactive)
				  (shell-command
				   (concat "nitrogen --save --set-zoom-fill "
						   (dired-file-name-at-point) " >>/dev/null 2>&1"))))
   ("/"       . dirvish-narrow)
   ("<left>"  . dired-up-directory)
   ("<right>" . dired-find-file)
   ("C-,"     . dirvish-history-go-backward)
   ("C-       ."     . dirvish-history-go-forward)
   ("F"       . dirvish-file-info-menu)
   ("G"       . revert-buffer)
   ("h"       . cj/dirvish-open-html-in-eww)  ;; it does what it says it does
   ("M"       . cj/dired-mark-all-visible-files)
   ("M-e"     . dirvish-emerge-menu)
   ("M-l"     . dirvish-ls-switches-menu)
   ("M-m"     . dirvish-mark-menu)
   ("M-p"     . dirvish-peek-toggle)
   ("M-s"     . dirvish-setup-menu)
   ("TAB"     . dirvish-subtree-toggle)
   ("f"       . cj/dirvish-open-file-manager-here)
   ("g"       . dirvish-quick-access)
   ("o"       . cj/xdg-open)
   ("O"       . cj/open-file-with-command)  ; Prompts for command to run
   ("r"       . dirvish-rsync)
   ("p"       . cj/dired-create-playlist-from-marked)
   ("s"       . dirvish-quicksort)
   ("v"       . dirvish-vc-menu)
   ("y"       . dirvish-yank-menu)))

;;; -------------------------------- Nerd Icons -------------------------------

(use-package nerd-icons
  :defer .5)

(use-package nerd-icons-dired
  :commands (nerd-icons-dired-mode))

;;; ---------------------------- Dired Hide Dotfiles ----------------------------

(use-package dired-hide-dotfiles
  :after dired
  :hook
  ;; Auto-hide dotfiles when entering dired/dirvish
  ((dired-mode . dired-hide-dotfiles-mode)
   (dirvish-mode . dired-hide-dotfiles-mode))
  :bind
  (:map dired-mode-map
		("." . dired-hide-dotfiles-mode)))

;;; ------------------------------- Dired Sidebar -------------------------------

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
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands) ;; disallow splitting dired window when it's showing
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)      ;; disallow rotating windows when sidebar is showing
  (setq dired-sidebar-subtree-line-prefix "  ")                    ;; two spaces give simple and aesthetic indentation
  (setq dired-sidebar-no-delete-other-windows t)                   ;; don't close when calling 'delete other windows'
  (setq dired-sidebar-theme 'nerd-icons)                           ;; gimme fancy icons, please
  (setq dired-sidebar-use-custom-font 'nil)                        ;; keep the same font as the rest of Emacs
  (setq dired-sidebar-delay-auto-revert-updates 'nil)              ;; don't delay auto-reverting
  (setq dired-sidebar-pop-to-sidebar-on-toggle-open 'nil))         ;; don't jump to sidebar when it's toggled on

;;; ----------------------------- Dired Ediff Files -----------------------------

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

;; ------------------------ Create Playlist From Marked ------------------------

(defvar cj/audio-file-extensions
  '("mp3" "flac" "m4a" "wav" "ogg" "aac" "opus" "aiff" "alac" "wma")
  "List of audio file extensions (lowercase, no dot). Used to filter files for M3U playlists.")

(defun cj/dired-create-playlist-from-marked ()
  "Create an .m3u playlist file from marked files in Dired (or Dirvish).
Filters for audio files, prompts for the playlist name, and saves the resulting
.m3u in the directory specified by =music-dir=. Interactive use only."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
		 (audio-files
		  (cl-remove-if-not
		   (lambda (f)
			 (let ((ext (file-name-extension f)))
			   (and ext
					(member (downcase ext) cj/audio-file-extensions))))
		   marked-files))
		 (count (length audio-files)))
	(if (zerop count)
		(user-error "No audio files marked (extensions: %s)"
					(string-join cj/audio-file-extensions ", "))
	  (let ((base-name nil)
			(playlist-path nil)
			(done nil))
		(while (not done)
		  (setq base-name (read-string
						   (format "Playlist name (without .m3u): ")))
		  ;; Sanitize: strip any trailing .m3u
		  (setq base-name (replace-regexp-in-string "\\.m3u\\'" "" base-name))
		  (setq playlist-path (expand-file-name (concat base-name ".m3u") music-dir))
		  (cond
		   ((not (file-exists-p playlist-path))
			;; Safe to write
			(setq done t))
		   (t
			(let ((choice (read-char-choice
						   (format "Playlist '%s' exists. [o]verwrite, [c]ancel, [r]ename? "
								   (file-name-nondirectory playlist-path))
						   '(?o ?c ?r))))
			  (cl-case choice
				(?o (setq done t))
				(?c (user-error "Cancelled playlist creation"))
				(?r (setq done nil)))))))
		;; Actually write the file
		(with-temp-file playlist-path
		  (dolist (af audio-files)
			(insert af "\n")))
		(message "Wrote playlist %s with %d tracks" (file-name-nondirectory playlist-path) count)))))

(provide 'dirvish-config)
;;; dirvish-config.el ends here.
