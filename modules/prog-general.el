;;; prog-general --- General Programming Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; This module provides general programming functionality not related to a
;; specific programming language, such as code-folding, project management,
;; highlighting symbols, snippets, and whitespace management.
;;
;; Keybinding Scheme:
;; ------------------
;; Unified keybindings across all programming languages using Projectile
;; for project-aware operations with language-specific overrides.
;;
;; Global Keybindings (all prog-mode buffers):
;;   F4     - projectile-compile-project (smart compilation)
;;   S-F4   - recompile (repeat last compile)
;;   F5     - projectile-test-project (run tests)
;;   S-F5   - Language-specific static analysis
;;   F6     - projectile-run-project (run/execute)
;;   S-F6   - Language-specific debugger
;;   C-; f  - Language-specific formatter
;;
;; Quick Reference Table:
;; | Key   | Global   | C             | Go          | Python      | Shell       |
;; |-------|----------|---------------|-------------|-------------|-------------|
;; | F4    | compile  | compile       | compile     | compile     | compile     |
;; | S-F4  | recompile| recompile     | (projectile)| (projectile)| (projectile)|
;; | F5    | test     | test          | test        | test        | test        |
;; | S-F5  | (none)   | disabled      | staticcheck | mypy        | shellcheck  |
;; | F6    | run      | run           | run         | run         | run         |
;; | S-F6  | (none)   | gdb           | dlv         | pdb         | disabled    |
;; | C-; f | format   | clang-format  | gofmt       | blacken     | shfmt       |

;;; Code:

(eval-when-compile (defvar code-dir))
(eval-when-compile (defvar projects-dir))
(eval-when-compile (defvar snippets-dir))

(defvar display-line-numbers-type)
(defvar outline-minor-mode-map)
(defvar projectile-per-project-compilation-buffer)
(defvar projectile-switch-project-action)
(defvar projectile-command-map)
(defvar dired-mode-map)
(defvar yas-snippet-dirs)
(defvar highlight-indent-guides-auto-enabled)
(defvar hl-todo-keyword-faces)
(defvar ws-butler-convert-leading-tabs-or-spaces)

(declare-function projectile-project-root "projectile")
(declare-function projectile-mode "projectile")
(declare-function magit-status "magit")
(declare-function dired-get-filename "dired")
(declare-function global-treesit-auto-mode "treesit-auto")
(declare-function treesit-auto-add-to-auto-mode-alist "treesit-auto")
(declare-function treesit-auto-recipe-lang "treesit-auto")
(declare-function highlight-indent-guides-mode "highlight-indent-guides")

;; Forward declarations for treesit-auto variables
(defvar treesit-auto-recipe-list)

;; Forward declarations for functions defined later in this file
(declare-function cj/find-project-root-file "prog-general")
(declare-function cj/project-switch-actions "prog-general")
(declare-function cj/deadgrep--initial-term "prog-general")
(declare-function cj/highlight-indent-guides-disable-in-non-prog-modes "prog-general")

;; --------------------- General Programming Mode Settings ---------------------
;; keybindings, minor-modes, and prog-mode settings

(defun cj/general-prog-settings ()
  "Keybindings, minor modes, and settings for programming mode."
  (interactive)
  (display-line-numbers-mode)                   ;; show line numbers
  (setq display-line-numbers-type 'relative)    ;; display numbers relative to 'the point'
  (setq-default display-line-numbers-width 3)   ;; 3 characters reserved for line numbers
  (turn-on-visual-line-mode)                    ;; word-wrapping
  (auto-fill-mode)                              ;; auto wrap at the fill column set
  (local-set-key (kbd "M-;") 'comment-dwim)     ;; comment/uncomment region as appropriate

  ;; Project-wide commands (can be overridden by language-specific modes)
  (local-set-key (kbd "<f4>") 'projectile-compile-project)   ;; compile project
  (local-set-key (kbd "S-<f4>") 'recompile)                  ;; recompile (repeat last)
  (local-set-key (kbd "<f5>") 'projectile-test-project)      ;; run tests
  (local-set-key (kbd "<f6>") 'projectile-run-project))      ;; run project

(add-hook 'prog-mode-hook #'cj/general-prog-settings)
(add-hook 'html-mode-hook #'cj/general-prog-settings)
(add-hook 'yaml-mode-hook #'cj/general-prog-settings)
(add-hook 'toml-mode-hook #'cj/general-prog-settings)


;; --------------------------------- Treesitter --------------------------------
;; incremental language syntax parser

(use-package tree-sitter)

;; installs tree-sitter grammars if they're absent
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  ;;  (treesit-auto-install 'prompt) ;; optional prompt instead of auto-install
  :config
  (require 'cl-lib)
  ;; Pin Go grammar to v0.19.1 for compatibility with Emacs 30.2 font-lock queries
  (let* ((go-idx (cl-position-if (lambda (recipe)
                                    (eq (treesit-auto-recipe-lang recipe) 'go))
                                  treesit-auto-recipe-list))
         (go-recipe (and go-idx (nth go-idx treesit-auto-recipe-list))))
    (when go-recipe
      ;; Directly modify the slot value using aset (struct fields are vectors internally)
      (aset go-recipe 6 "v0.19.1")))  ; slot 6 is :revision

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; -------------------------------- Code Folding -------------------------------

;; BICYCLE
;; cycle visibility of outline sections and code blocks.
(use-package bicycle
  :after outline
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
			  ("C-<tab>" . bicycle-cycle)
			  ;; backtab is shift-tab
			  ("<backtab>" . bicycle-cycle-global)))

;; --------------------------------- Projectile --------------------------------
;; project support

;; only discover projects when there's no bookmarks file
(defun cj/projectile-schedule-project-discovery ()
  (let ((projectile-bookmark-file (concat user-emacs-directory "/projectile-bookmarks.eld")))
	(unless (file-exists-p projectile-bookmark-file)
	  (run-at-time "3" nil 'projectile-discover-projects-in-search-path))))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
		("r" . projectile-replace-regexp)
		("t" . cj/open-project-root-todo))
  :custom
  (projectile-auto-discover nil)
  (projectile-project-search-path `(,code-dir ,projects-dir))
  :config
  (require 'seq)

  (defun cj/find-project-root-file (regexp)
	"Return first file in the current Projectile project root matching REGEXP.

Match is done against (downcase file) for case-insensitivity.
REGEXP must be a string or an rx form."
	(when-let ((root (projectile-project-root)))
	  (seq-find (lambda (file)
				  (string-match-p (if (stringp regexp)
									  regexp
									(rx-to-string regexp))
								  (downcase file)))
				(directory-files root))))

  (defun cj/open-project-root-todo ()
	"Open todo.org in the current Projectile project root.

If no such file exists there, display a message."
	(interactive)
	(if-let ((root (projectile-project-root)))
		(let ((file (cj/find-project-root-file "^todo\\.org$")))
		  (if file
			  (find-file (expand-file-name file root))
			(message "No todo.org in project root: %s" root)))
	  (message "Not in a Projectile project")))

  (defun cj/project-switch-actions ()
	"On project switch, open TODO.{org,md,txt} or fall back to Magit."
	(let ((file (cj/find-project-root-file
				 (rx bos "todo." (or "org" "md" "txt") eos))))
	  (if file
		  (find-file (expand-file-name file (projectile-project-root)))
		(magit-status (projectile-project-root)))))

  ;; scan for projects if none are defined
  (cj/projectile-schedule-project-discovery)

  ;; don't reuse comp buffers between projects
  (setq projectile-per-project-compilation-buffer t)
  (projectile-mode)
  (setq projectile-switch-project-action #'cj/project-switch-actions))

;; groups ibuffer by projects
(use-package ibuffer-projectile
  :after projectile
  :hook (ibuffer-mode . ibuffer-projectile-set-filter-groups))

;; list all errors project-wide
(use-package flycheck-projectile
  :after projectile
  :commands flycheck-projectile-list-errors
  :bind
  (:map projectile-command-map
		("x" . flycheck-projectile-list-errors)))

;; ---------------------------------- Ripgrep ----------------------------------

(use-package deadgrep
  :after projectile
  :bind
  (:map projectile-command-map
		("G" . deadgrep)                 ;; project-wide search
		("g" . cj/deadgrep-here)     ;; search in context directory
		("d" . cj/deadgrep-in-dir))  ;; prompt for directory

  :config
  (require 'thingatpt)

  (defun cj/deadgrep--initial-term ()
	(cond
	 ((use-region-p)
	  (buffer-substring-no-properties (region-beginning) (region-end)))
	 (t (thing-at-point 'symbol t))))

  (defun cj/deadgrep-here (&optional term)
	"Search with Deadgrep in the most relevant directory at point."
	(interactive)
	(let* ((root
			(cond
			 ((derived-mode-p 'dired-mode)
			  (let ((path (dired-get-filename nil t)))
				(cond
				 ;; If point is on a directory entry, search within that directory.
				 ((and path (file-directory-p path)) path)
				 ;; If point is on a file, search in its containing directory.
				 ((and path (file-regular-p path)) (file-name-directory path))
				 (t default-directory))))
			 (buffer-file-name
			  (file-name-directory (file-truename buffer-file-name)))
			 (t default-directory)))
		   (root (file-name-as-directory (expand-file-name root)))
		   (term (or term (read-from-minibuffer "Search: " (cj/deadgrep--initial-term)))))
	  (deadgrep term root)))

  (defun cj/deadgrep-in-dir (&optional dir term)
    "Prompt for a directory, then search there with Deadgrep."
    (interactive)
	(let* ((dir (or dir (read-directory-name "Search in directory: " default-directory nil t)))
		   (dir (file-name-as-directory (expand-file-name dir)))
		   (term (or term (read-from-minibuffer "Search: " (cj/deadgrep--initial-term)))))
	  (deadgrep term dir))))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "d" #'cj/deadgrep-here))


;; ---------------------------------- Snippets ---------------------------------
;; reusable code and text

(use-package yasnippet
  :commands (yas-new-snippet yas-visit-snippet-file yas-global-mode)
  :hook (prog-mode . yas-minor-mode)
  :bind
  ("C-c s n" . yas-new-snippet)
  ("C-c s e" . yas-visit-snippet-file)
  :config
  (setq yas-snippet-dirs '(snippets-dir)))

(use-package ivy-yasnippet
  :after yasnippet
  :bind
  ("C-c s i" . ivy-yasnippet))

;; --------------------- Display Color On Color Declaration --------------------
;; display the actual color as highlight to color hex code

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; ---------------------------- Symbol Overlay Mode ----------------------------
;; Highlight symbols with keymap-enabled overlays
;; replaces highlight-symbol-mode

(use-package symbol-overlay
  :bind-keymap
  ("C-c C-s" . symbol-overlay-map)
  :hook
  (prog-mode . symbol-overlay-mode))


;; --------------------------- Highlight Indentation ---------------------------

(use-package highlight-indent-guides
  :hook (prog-mode . cj/highlight-indent-guides-enable)
  :config
  ;; Disable auto face coloring to use explicit faces for better visibility across themes
  (setq highlight-indent-guides-auto-enabled nil)

  ;; Set explicit face backgrounds and foreground for the indentation guides
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "darkgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")

  (defun cj/highlight-indent-guides-enable ()
	"Enable highlight-indent-guides with preferred settings for programming modes."
	(setq-local highlight-indent-guides-method 'bitmap)
	(setq-local highlight-indent-guides-responsive nil)
	(highlight-indent-guides-mode 1))

  ;; Disable in non-prog-mode buffers
  (defun cj/highlight-indent-guides-disable-in-non-prog-modes ()
	"Disable highlight-indent-guides-mode outside programming modes."
	(unless (derived-mode-p 'prog-mode)
	  (highlight-indent-guides-mode -1)))

  (add-hook 'after-change-major-mode-hook
			#'cj/highlight-indent-guides-disable-in-non-prog-modes))

;; ------------------------------ Highlight TODOs ------------------------------
;; Highlights todo keywords in code for easy spotting.

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
;; trims trailing whitespace only from lines you've modified when saving buffer

(use-package ws-butler
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  ;; no org and text mode as org branches occasionally move up a line and become invalid
  :config
  (setq ws-butler-convert-leading-tabs-or-spaces t))

;; ------------------------------------ LSP ------------------------------------
;; Language Server Protocol for intelligent code completion and navigation
;; Works with multiple languages: C, Python, Go, Rust, JavaScript, etc.

;; Forward declarations for LSP variables
(defvar lsp-idle-delay)
(defvar lsp-log-io)
(defvar lsp-enable-folding)
(defvar lsp-enable-snippet)
(defvar lsp-headerline-breadcrumb-enable)
(defvar lsp-completion-provider)
(defvar lsp-completion-show-detail)
(defvar lsp-completion-show-kind)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")  ;; LSP commands under C-c l prefix
  :config
  ;; Performance optimizations
  (setq lsp-idle-delay 0.1)
  (setq lsp-log-io nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet t)
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Improve completion
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t))

;; ----------------- Auto-Close Successful Compilation Windows -----------------
;; close compilation windows when successful. from 'enberg' on #emacs

(add-hook 'compilation-finish-functions
		  (lambda (_buf str)
			(if (null (string-match ".*exited abnormally.*" str))
				;;no errors, make the compilation window go away in a few seconds
				(progn
				  (run-at-time
				   "1.5 sec" nil 'delete-windows-on
				   (get-buffer-create "*compilation*"))))))


(provide 'prog-general)
;;; prog-general.el ends here
