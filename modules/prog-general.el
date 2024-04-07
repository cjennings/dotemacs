;;; prog-general --- General Programming Functionality and Settings -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; This module provides general programming functionality not related to a
;; specific programming language, such as code-folding, project management,
;; highlighting symbols, snippets, and whitespace management.

;;; Code:

;; ---------------------- General Prog Settings ----------------------
;; keybindings, minor-modes, and prog-mode settings

(defun cj/general-prog-settings ()
  "Keybindings, minor modes, and settings for programming mode."
  (interactive)
  (display-line-numbers-mode)                   ;; show line numbers
  (setq display-line-numbers-type 'relative)    ;; display numbers relative to 'the point'
  (setq-default display-line-numbers-width 3)   ;; 3 characters reserved for line numbers
  (turn-on-visual-line-mode)                    ;; word-wrapping
  (auto-fill-mode)                              ;; auto wrap at the fill column set
  (local-set-key (kbd "M-;") 'comment-dwim))    ;; comment/uncomment region as appropriate

(add-hook 'prog-mode-hook #'cj/general-prog-settings)
(add-hook 'html-mode-hook #'cj/general-prog-settings)
(add-hook 'yaml-mode-hook #'cj/general-prog-settings)
(add-hook 'toml-mode-hook #'cj/general-prog-settings)

;; --------------------------- Header Folding --------------------------

;; dependencies for bicycle
(use-package outline-minor-mode
  :ensure nil ;; built-in part of org mode
  :hook prog-mode)

(use-package hs-minor-mode
  :ensure nil ;; built-in
  :hook prog-mode)

;; BICYCLE
;; cycle visibility of outline sections and code blocks.
;; additionally it can make use of the hideshow package.
(use-package bicycle
  :after outline
  :defer 1
  :bind (:map outline-minor-mode-map
              ("C-<tab>" . bicycle-cycle)
              ;; backtab is shift-tab
              ("<backtab>" . bicycle-cycle-global)))

;; --------------------------- Project Switch Actions --------------------------
;; if there's a todo or readme file in the project, display it when switching

(defun cj/project-switch-actions ()
  "Opens TODO or README file on projectile switch project.
If none exists, it opens magit-status."
  ;; (dired-sidebar-hide-sidebar)
  ;; (dired-sidebar-show-sidebar)
  (let ((todo-file
         (cond
          ((file-exists-p (concat (projectile-project-root) "TODO.org")) "TODO.org")
          ((file-exists-p (concat (projectile-project-root) "todo.org")) "todo.org")
          ((file-exists-p (concat (projectile-project-root) "TODO.md")) "TODO.md")
		  ((file-exists-p (concat (projectile-project-root) "TODO.txt")) "TODO.txt"))))
    (if todo-file
        (find-file (concat (projectile-project-root) todo-file))
      (magit-status))))

;; --------------------------------- Projectile --------------------------------
;; project support

;; notify user when scanning for projects
(defun cj/projectile-identify-projects ()
  (message "No project cache found. Identifying projects....")
  (projectile-discover-projects-in-search-path))

;; only run discover projects when there's no bookmarks file
(defun cj/projectile-schedule-project-discovery ()
  (let ((bookmark-file (concat user-emacs-directory "/projectile-bookmarks.eld")))
    (unless (file-exists-p bookmark-file)
      (run-at-time "3" nil 'cj/projectile-identify-projects))))

(use-package projectile
  :defer .5
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
        ("r" . projectile-replace-regexp))
  :custom
  (projectile-auto-discover nil)
  (projectile-project-search-path '("~/code"
                                    "~/projects"))
  :config
  (cj/projectile-schedule-project-discovery)
  ;; don't reuse comp buffers between projects
  (setq projectile-per-project-compilation-buffer t)
  (projectile-mode)
  (setq projectile-switch-project-action #'cj/project-switch-actions))

;; groups ibuffer by projects
(use-package ibuffer-projectile
  :defer .5
  :after projectile
  :hook (ibuffer-mode . ibuffer-projectile-set-filter-groups))

;; list all errors project-wide
(use-package flycheck-projectile
  :defer .5
  :after projectile
  :bind
  (:map projectile-command-map
        ("X" . flycheck-projectile-list-errors)))

;; ---------------------------------- Ripgrep ----------------------------------
;; allows fast searching for text anywhere in the project with C-c p G (grep)

(use-package ripgrep
  :defer .5
  :after projectile
  :bind
  (:map projectile-command-map
        ("G" . projectile-ripgrep)))

;; ---------------------------------- Snippets ---------------------------------
;; reusable code and text.

(use-package yasnippet
  :defer 1
  :bind
  ("C-c s n" . yas-new-snippet)
  ("C-c s e" . yas-visit-snippet-file)
  :config
  (setq yas-snippet-dirs '(snippets-dir))
  (yas-global-mode 1))

(use-package ivy-yasnippet
  :after yasnippet
  :bind
  ("C-c s i" . ivy-yasnippet))

;; --------------------- Display Color On Color Declaration --------------------
;; display the actual color as highlight to color hex code

(use-package rainbow-mode
  :defer .5
  :hook (prog-mode . rainbow-mode))

;; ---------------------------- Symbol Overlay Mode ----------------------------
;; Highlight symbols with keymap-enabled overlays
;; replaces highlight-symbol-mode

(use-package symbol-overlay
  :defer .5
  :bind-keymap
  ("C-c C-s" . symbol-overlay-map)
  :hook
  (prog-mode . symbol-overlay-mode))

;; ------------------------------ Highlight TODOs ------------------------------
;; Highlights todo keywords in code for easy spotting
(use-package hl-todo
  :defer 1
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("FIXME"  . "#FF0000")
          ("BUG"    . "#FF0000")
          ("HACK"   . "#FF0000")
          ("ISSUE"  . "#DAA520")
          ("TASK"   . "#DAA520")
          ("NOTE"   . "#2C780E")
          ("WIP"   .  "#1E90FF"))))

;; --------------------------- Whitespace Management ---------------------------
;; when saving your file, trims unneeded whitespace only from lines you've modified

(use-package ws-butler
  :defer .5
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode)
  :config
  (setq ws-butler-convert-leading-tabs-or-spaces t))

;; ----------------- Auto-Close Successful Compilation Windows -----------------
;; from 'enberg' on #emacs

(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (if (null (string-match ".*exited abnormally.*" str))
                ;;no errors, make the compilation window go away in a few seconds
                (progn
                  (run-at-time
                   "1.5 sec" nil 'delete-windows-on
                   (get-buffer-create "*compilation*"))))))


(provide 'prog-general)
;;; prog-general.el ends here
