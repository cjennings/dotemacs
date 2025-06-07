;;; dirvish-config.el --- Configuration for the Dired and Dirvish File Managers -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

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
        ("E" . wdired-change-to-wdired-mode)           ;; edit names and properties in buffer
        ("e" . cj/dired-ediff-files))                  ;; ediff files
  :custom
  (dired-use-ls-dired nil)                             ;; non GNU FreeBSD doesn't support a "--dired" switch
  :config
  (setq dired-listing-switches "-l --almost-all --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-clean-up-buffers-too t)                  ;; offer to kill buffers associated deleted files and dirs
  (setq dired-clean-confirm-killing-deleted-buffers t) ;; don't ask; just kill buffers associated with deleted files
  (setq dired-kill-when-opening-new-dired-buffer t)    ;; don't litter by leaving buffers when navigating directories
  (setq dired-recursive-copies (quote always))         ;; “always” means no asking
  (setq dired-recursive-deletes (quote top)))          ;; “top” means ask once

(add-hook 'dired-mode-hook 'auto-revert-mode)          ;; auto revert dired when files change

;; ------------------------------ Dired Open With ------------------------------

(defun cj/dired-open-with (command)
  "Open the Dired file at point with a user-specified COMMAND.
This function is meant to be called interactively. It prompts for the command to
open the file with if called without a parameter. The command runs
asynchronously and its output is saved in a buffer, but the buffer is not
automatically displayed."
  (interactive "sCommand to use to open the file: ")
  (let* ((file (dired-get-file-for-visit))
		 (buff (generate-new-buffer (concat "*dired-open-with output: " file "*"))))
	(start-process-shell-command command buff (concat command " \"" file "\""))))


;; -------------------------- Dired Copy Path As Kill --------------------------
;; copies the full path of the file at point to the clipboard

(defun cj/dired-copy-path-as-kill ()
  "Copy the full path of file at point in Dired to the clipboard."
  (interactive)
  (let ((filename (dired-get-file-for-visit)))
	(if (and filename (file-exists-p filename))
		(progn
		  (kill-new filename)
		  (message "Copied '%s' to clipboard." filename))
	  (message "No file at point."))))

;; ------------------------ Dired Convert Image To Jpeg ------------------------
;; converts the image at point to a jpeg by typing "C" within dirvish

(defun cj/dired-convert-image-to-jpeg ()
  "Convert an image to JPEG via ImageMagick on the file at point.
Alert if the file is already a JPEG; notify the user when converstion is done."
  (interactive)
  (let* ((original-file (dired-get-file-for-visit))
         (file-extension (file-name-extension original-file))
         (jpeg-file (concat (file-name-sans-extension original-file) ".jpeg")))
    (if (member file-extension '("png" "bmp" "gif" "tif" "tiff" "svg" "webp"))
        (if (string= file-extension "jpeg")
            (message "File is already in JPEG format.")
          (start-process "convert-to-jpeg" nil "convert" original-file jpeg-file)
          (message "Conversion started for %s" original-file))
      (message (concat "File is not a supported image file type."
                       "Current supported types: "
                       "'png' 'bmp' 'gif' 'tif' 'tiff' 'svg' 'webp'")))))

;; ------------------------ Dired Mark All Visible Files -----------------------
;; convenience function to mark all visible files by typing "M" in dirvish

(defun cj/dired-mark-all-visible-files ()
  "Mark all visible files in Dired mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (looking-at "^. d"))
          (dired-mark 1))
      (forward-line 1))))

;; ---------------------------------- Dirvish ----------------------------------

(use-package dirvish
  :init
  (add-to-list 'load-path (concat user-emacs-directory "elpa/dirvish-2.3.0"))
  (add-to-list 'load-path (concat user-emacs-directory "elpa/dirvish-2.3.0/extensions"))
  (require 'dirvish)
  (require 'dirvish-icons)
  (require 'dirvish-emerge)
  (require 'dirvish-quick-access)
  (require 'dirvish-yank)
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   `(("h"  "~/"                                   "home")
     ("cx" ,code-dir                              "code directory")
     ("dl" ,dl-dir                                "downloads")
     ("dr" ,(concat sync-dir "/drill/")           "drill files")
     ("dt" ,(concat dl-dir "/torrents/complete/") "torrents")
     ("dx" "~/documents/"                         "documents")
     ("mb" "/media/backup/"                       "backup directory")
     ("mx" "~/music/"                             "music")
     ("pd" "~/projects/documents/"                "project documents")
     ("pl" "~/projects/elibrary/"                 "project elibrary")
     ("pf" "~/projects/finances/"                 "project finances")
     ("pj" "~/projects/jr-estate/"                "project jr-estate")
     ("ps" ,(concat pix-dir "/screenshots/")      "pictures screenshots")
     ("pw" ,(concat pix-dir "/wallpaper/")        "pictures wallpaper")
     ("px" ,pix-dir                               "pictures directory")
	 ("rcj" "/sshx:cjennings@cjennings.net:~"     "remote cjennings.net")
	 ("rsb" "/sshx:cjennings@wolf.usbx.me:/home/cjennings/" "remote seedbox")
	 ("sx" ,sync-dir                              "sync directory")
	 ("sv" "~/sync/videos/"                       "sync/videos directory")
	 ("tg" ,(concat sync-dir "/text.games")       "text games")
	 ("vr" ,video-recordings-dir                  "video recordings directory")
	 ("vx" ,videos-dir                            "videos")
     )) ;; end dirvish-quick-access-entries
  ;; (dirvish-attributes '(file-size))
  (dirvish-attributes '(nerd-icons file-size))
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
   ("Z"       . (lambda () (interactive) (cj/dired-open-with "zathura")))
   ("L"       . (lambda () (interactive) (cj/dired-open-with "libreoffice")))
   ("o"       . (lambda () (interactive) (cj/dired-open-with "xdg-open")))
   ("P"       . (lambda () (interactive) (cj/dired-open-with "gimp")))
   ("O"       . (lambda () (interactive) (call-interactively 'cj/dired-open-with)))
   ("<left>"  . dired-up-directory)
   ("<right>" . dired-find-file)
   ("f"       . dirvish-file-info-menu)
   ("p"       . cj/dired-copy-path-as-kill)
   ("C"       . cj/dired-convert-image-to-jpeg)
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
   ;;   ("M-t"     . dirvish-layout-toggle) ;; todo find another key; M-t is contentious
   ("M-s"     . dirvish-setup-menu)
   ("M-e"     . dirvish-emerge-menu)))

;; -------------------------------- Nerd Icons -------------------------------

(use-package nerd-icons
  :defer .5)

(use-package nerd-icons-dired
  :commands (nerd-icons-dired-mode))

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
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands) ;; disallow splitting dired window when it's showing
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)      ;; disallow rotating windows when sidebar is showing
  (setq dired-sidebar-subtree-line-prefix "  ")                    ;; two spaces give simple and aesthetic indentation
  (setq dired-sidebar-no-delete-other-windows t)                   ;; don't close when calling 'delete other windows'
  (setq dired-sidebar-theme 'nerd-icons)                           ;; gimme fancy icons, please
  (setq dired-sidebar-use-custom-font 'nil)                        ;; keep the same font as the rest of Emacs
  (setq dired-sidebar-delay-auto-revert-updates 'nil)              ;; don't delay auto-reverting
  (setq dired-sidebar-pop-to-sidebar-on-toggle-open 'nil))         ;; don't jump to sidebar when it's toggled on

;; ----------------------------- Dired Ediff Files -----------------------------
;; mark two files within dirvish, then ediff them by typing "e" (see above keybinding)

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
