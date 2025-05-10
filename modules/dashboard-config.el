;;; dashboard-config.el --- Dashboard Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Note:
;; Nerd-Icons Cheat Sheet: https://www.nerdfonts.com/cheat-sheet

;;; Code:

;; ------------------------ Dashboard Bookmarks Override -----------------------
;; overrides the bookmark insertion from the dashboard package to provide an
;; option that only shows the bookmark name, avoiding the path. Paths are often
;; too long and the truncation options aren't aesthetically pleasing. Should be
;; accompanied by the setting (setq dashboard-bookmarks-show-path nil) in
;; config.

(defcustom dashboard-bookmarks-item-format "%s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defun dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (dashboard-insert-section
   "Bookmarks:"
   (dashboard-subseq (bookmark-all-names) list-size)
   list-size
   'bookmarks
   (dashboard-get-shortcut 'bookmarks)
   `(lambda (&rest _) (bookmark-jump ,el))
   (if-let* ((filename el)
             (path (bookmark-get-filename el))
             (path-shorten (dashboard-shorten-path path 'bookmarks)))
       (cl-case dashboard-bookmarks-show-path
         (`align
          (unless dashboard--bookmarks-cache-item-format
            (let* ((len-align (dashboard--align-length-by-type 'bookmarks))
                   (new-fmt (dashboard--generate-align-format
                             dashboard-bookmarks-item-format len-align)))
              (setq dashboard--bookmarks-cache-item-format new-fmt)))
          (format dashboard--bookmarks-cache-item-format filename path-shorten))
         (`nil filename)
         (t (format dashboard-bookmarks-item-format filename path-shorten)))
     el)))

;; ----------------------------- Display Dashboard -----------------------------
;; convenience function to redisplay dashboard

(defun cj/display-dashboard ()
  "Display dashboard, create if it doesn't exist."
  (interactive)
  (dired-sidebar-hide-sidebar) ;; hide dired-sidebar if displaying
  (get-buffer-create "*dashboard*")
  (pop-to-buffer "*dashboard*")
  (delete-other-windows))
(global-set-key (kbd "<f4>") 'cj/display-dashboard)

;; --------------------------------- Dashboard ---------------------------------
;; a useful startup screen for Emacs

(use-package dashboard
  :demand t ;; needed to startup quickly
  :custom
  (dashboard-projects-backend 'projectile)

  (dashboard-item-generators
   '((projects . dashboard-insert-projects)
	 (bookmarks . dashboard-insert-bookmarks)))

  (dashboard-items '((projects . 5)
                     (bookmarks . 7)))

  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-banner-title
	 dashboard-insert-newline
	 dashboard-insert-newline
	 dashboard-insert-navigator
	 dashboard-insert-init-info
	 dashboard-insert-newline
	 dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline))
  :config

  ;; == general
  (dashboard-setup-startup-hook)                                 ;; run dashboard post emacs init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; display dashboard on startup
  (setq dashboard-display-icons-p t)                             ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons)                         ;; use `nerd-icons' package
  (setq dashboard-center-content t)                              ;; horizontally center dashboard content
  (setq dashboard-bookmarks-show-path nil)                       ;; don't show paths in bookmarks
  (setq dashboard-set-footer nil)  ;; don't show footer and quotes

  ;; == banner
  (setq dashboard-startup-banner (concat user-emacs-directory "assets/M-x_butterfly.png"))
  (setq dashboard-banner-logo-title "Emacs: The Editor That Saves Your Soul")

  ;; == navigation
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
		`(((,(nerd-icons-faicon  "nf-fa-envelope")
			"Email" "Mu4e Email Client"
			(lambda (&rest _) (mu4e)))

		   (,(nerd-icons-faicon "nf-fae-book_open_o")
			"Ebooks" "Calibre Ebook Reader"
			(lambda (&rest _) (calibredb)))

		   (,(nerd-icons-mdicon "nf-md-school")
			"Flashcards" "Org-Drill"
			(lambda (&rest _) (cj/drill-start)))

		   (,(nerd-icons-faicon "nf-fa-rss_square")
			"Feeds" "Elfeed Feed Reader"
			(lambda (&rest _) (elfeed-dashboard)))

		   (,(nerd-icons-faicon "nf-fa-comments")
			"IRC" "Emacs Relay Chat"
			(lambda (&rest _) (cj/erc-start-or-switch)))

		   ;; (,(nerd-icons-faicon "nf-fae-telegram")
		   ;; 	"Telegram" "Telega Chat Client"
		   ;; 	(lambda (&rest _) (telega)))

		   (,(nerd-icons-faicon "nf-fa-folder_o")
			"Files" "Dirvish File Manager"
			(lambda (&rest _) (dirvish user-home-dir))))))

  ;; == content
  (setq dashboard-show-shortcuts nil) ;; don't show dashboard item abbreviations
  ) ;; end use-package dashboard

(provide 'dashboard-config)
;;; dashboard-config.el ends here.
