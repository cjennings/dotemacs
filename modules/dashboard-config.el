;;; dashboard-config.el --- Dashboard Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Note:
;; Nerd-Icons Cheat Sheet: https://www.nerdfonts.com/cheat-sheet

;;; Code:

(eval-when-compile (require 'undead-buffers))
(declare-function cj/make-buffer-undead "undead-buffers" (string))
(autoload 'cj/make-buffer-undead "undead-buffers" nil t)

;; ------------------------ Dashboard Bookmarks Override -----------------------
;; overrides the bookmark insertion from the dashboard package to provide an
;; option that only shows the bookmark name, avoiding the path. Paths are often
;; too long and the truncation options aren't aesthetically pleasing. Should be
;; accompanied by the setting (setq dashboard-bookmarks-show-path nil) in
;; config.

(defvar dashboard-bookmarks-item-format "%s"
  "Format to use when showing the base of the file name.")

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
;; convenience function to redisplay dashboard and kill all other windows

(defun cj/dashboard-only ()
  "Switch to *dashboard* buffer and kill all other buffers and windows."
  (interactive)
  (if (get-buffer "*dashboard*")
	  (progn
		(switch-to-buffer "*dashboard*")
		(cj/kill-all-other-buffers-and-windows))
	(when (fboundp 'dashboard-open)
	  (dashboard-open))))

;; --------------------------------- Dashboard ---------------------------------
;; a useful startup screen for Emacs

(use-package dashboard
  :demand t
  :hook (emacs-startup . cj/dashboard-only)
  :bind ("<f1>" . cj/dashboard-only)
  :custom
  (dashboard-projects-backend 'projectile)

  (dashboard-item-generators
   '((projects . dashboard-insert-projects)
     (bookmarks . dashboard-insert-bookmarks)))

  (dashboard-items '((projects . 5)
                     (bookmarks . 10)))

  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-newline
     dashboard-insert-navigator
     ;; dashboard-insert-init-info  ; Disabled: package count and startup time
     dashboard-insert-newline
     dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline))
  :config

  ;; == general
  (dashboard-setup-startup-hook)                                      ;; run dashboard post emacs init
  (cj/make-buffer-undead "*dashboard*")                               ;; make this buffer unkillable

  (if (< (length command-line-args) 2)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))) ;; don't display dashboard if opening a file
  (setq dashboard-display-icons-p t)                                  ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons)                              ;; use `nerd-icons' package
  (setq dashboard-center-content t)                                   ;; horizontally center dashboard content
  (setq dashboard-bookmarks-show-path nil)                            ;; don't show paths in bookmarks
  (setq dashboard-set-footer nil)                                     ;; don't show footer and quotes

  ;; == banner
  (setq dashboard-startup-banner (concat user-emacs-directory "assets/M-x_butterfly.png"))
  (setq dashboard-banner-logo-title "Emacs: The Editor That Saves Your Soul")

  ;; == navigation
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(;; Row 1
          ((,(nerd-icons-faicon "nf-fa-code")
            "Code" "Switch Project"
            (lambda (&rest _) (projectile-switch-project))
            nil " " "")

           (,(nerd-icons-faicon  "nf-fa-envelope")
            "Email" "Mu4e Email Client"
            (lambda (&rest _) (mu4e))
            nil " " "")

           (,(nerd-icons-mdicon "nf-md-calendar")
            "Agenda" "Main Org Agenda"
            (lambda (&rest _) (cj/main-agenda-display))
            nil " " "")

           (,(nerd-icons-mdicon "nf-md-school")
            "Flashcards" "Org-Drill"
            (lambda (&rest _) (cj/drill-start))
            nil " " "")

           (,(nerd-icons-faicon "nf-fae-book_open_o")
            "Books" "Calibre Ebook Reader"
            (lambda (&rest _) (calibredb))
            nil " " ""))

          ;; Row 2
          ((,(nerd-icons-faicon "nf-fa-rss_square")
            "RSS/Feeds" "Elfeed Feed Reader"
			(lambda (&rest _) (cj/elfeed-open))
            nil " " "")

           (,(nerd-icons-faicon "nf-fa-comments")
            "IRC" "Emacs Relay Chat"
            (lambda (&rest _) (cj/erc-switch-to-buffer-with-completion))
            nil " " "")

           (,(nerd-icons-devicon "nf-dev-terminal")
            "Terminal" "Launch VTerm"
            (lambda (&rest _) (vterm))
            nil " " "")

           ;; (,(nerd-icons-faicon "nf-fae-telegram")
           ;;     "Telegram" "Telega Chat Client"
           ;;     (lambda (&rest _) (telega))
           ;;     nil " " "")

           (,(nerd-icons-faicon "nf-fa-folder_o")
            "Directory/Files" "Dirvish File Manager"
            (lambda (&rest _) (dirvish user-home-dir))
            nil " " ""))))

  ;; == content
  (setq dashboard-show-shortcuts nil) ;; don't show dashboard item abbreviations
  ) ;; end use-package dashboard

;; ------------------------ Dashboard Keybindings ------------------------------

(with-eval-after-load 'dashboard
  ;; Disable 'q' to quit dashboard
  (define-key dashboard-mode-map (kbd "q") nil)

  ;; Dashboard launcher keybindings
  (define-key dashboard-mode-map (kbd "e") (lambda () (interactive) (mu4e)))
  (define-key dashboard-mode-map (kbd "c") (lambda () (interactive) (projectile-switch-project)))
  (define-key dashboard-mode-map (kbd "a") (lambda () (interactive) (cj/main-agenda-display)))
  (define-key dashboard-mode-map (kbd "b") (lambda () (interactive) (calibredb)))
  (define-key dashboard-mode-map (kbd "f") (lambda () (interactive) (cj/drill-start)))
  (define-key dashboard-mode-map (kbd "r") (lambda () (interactive) (cj/elfeed-open)))
  (define-key dashboard-mode-map (kbd "i") (lambda () (interactive) (cj/erc-switch-to-buffer-with-completion)))
  (define-key dashboard-mode-map (kbd "t") (lambda () (interactive) (vterm)))
  (define-key dashboard-mode-map (kbd "d") (lambda () (interactive) (dirvish user-home-dir))))

(provide 'dashboard-config)
;;; dashboard-config.el ends here.
