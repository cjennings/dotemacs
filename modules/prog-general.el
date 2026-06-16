;;; prog-general --- General Programming Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P/S.
;; Load shape: eager.
;; Eager reason: generic programming defaults, projectile, and the tree-sitter /
;;   LSP policy shared across every language mode.
;; Top-level side effects: four add-hook, package configuration via use-package.
;; Runtime requires: user-constants (code-dir, projects-dir, snippets-dir, used
;;   in the projectile and yasnippet configs).
;; Direct test load: yes.
;;
;; This module provides general programming functionality not related to a
;; specific programming language, such as code-folding, project management,
;; highlighting symbols, snippets, and whitespace management.
;;
;; Keybinding Scheme:
;; ------------------
;; The F4–F7 dev block is owned by dev-fkeys.el (global bindings). Per-
;; language modules only set S-F5 / S-F6 overrides for static analysis
;; and debugging. F5 is reserved for the debug ticket.
;;
;; Global (dev-fkeys.el):
;;   F4 / C-F4 / M-F4   compile + run dispatcher / compile only / clean + rebuild
;;   S-F4               recompile (repeat last)
;;   F6                 project tests (Phase 1 stopgap; Phase 2 = polyglot dispatcher)
;;   F7                 coverage report (coverage-core.el)
;;   C-; f              language-specific formatter
;;
;; Per-language S-modifier overrides:
;; | Key  | C        | Go          | Python | Shell      |
;; |------|----------|-------------|--------|------------|
;; | S-F5 | disabled | staticcheck | mypy   | shellcheck |
;; | S-F6 | gdb      | dlv         | pdb    | disabled   |

;;; Code:

(require 'user-constants)  ;; code-dir, projects-dir, snippets-dir

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

;; Set the line-number type and width before any prog buffer enables
;; display-line-numbers-mode. Setting them inside the hook ran after the mode
;; turned on, so the first prog buffer of a session got absolute numbers.
(setq display-line-numbers-type 'relative)      ;; numbers relative to point
(setq-default display-line-numbers-width 3)     ;; 3 chars reserved for numbers

(defun cj/general-prog-settings ()
  "Keybindings, minor modes, and settings for programming mode."
  (interactive)
  (display-line-numbers-mode)                   ;; show line numbers
  (turn-on-visual-line-mode)                    ;; word-wrapping
  (auto-fill-mode)                              ;; auto wrap at the fill column set
  (local-set-key (kbd "M-;") 'comment-dwim)     ;; comment/uncomment region as appropriate

  ;; F4–F6 are global, owned by dev-fkeys.el. F5 is reserved for the
  ;; debug ticket (separate work).
  )

(add-hook 'prog-mode-hook #'cj/general-prog-settings)
(add-hook 'html-mode-hook #'cj/general-prog-settings)
(add-hook 'yaml-mode-hook #'cj/general-prog-settings)
(add-hook 'toml-mode-hook #'cj/general-prog-settings)


;; --------------------------------- Treesitter --------------------------------
;; incremental language syntax parser
;; Using Emacs 29+ built-in treesit with treesit-auto for grammar management

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

(defun cj/--find-file-respecting-split (file)
  "Open FILE in another window when the frame is split, else in this window.
\"Split\" means the selected window is not the only one in its frame, so
opening the file lands it in the other pane rather than replacing the
buffer the user is looking at."
  (if (one-window-p)
	  (find-file file)
	(find-file-other-window file)))

(defun cj/open-project-daily-prep ()
  "Open the current Projectile project's daily prep, respecting the split.
The prep file is daily-prep.org under the project root (a stable symlink
to the dated prep doc in daily-prep/).  Project-scoped: a project without
one gets a message.  Opens in the other window when the frame is split and
reuses the current window otherwise, matching `cj/open-project-root-todo'."
  (interactive)
  (if-let ((root (projectile-project-root)))
      (let ((file (expand-file-name "daily-prep.org" root)))
        (if (file-exists-p file)
            (cj/--find-file-respecting-split file)
          (message "No daily-prep.org in project: %s" root)))
    (message "Not in a Projectile project")))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
		("r" . projectile-replace-regexp)
		("t" . cj/open-project-root-todo)
		("d" . cj/open-project-daily-prep))
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
			  (cj/--find-file-respecting-split (expand-file-name file root))
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
		("G" . cj/deadgrep-in-dir)   ;; prompt for any directory
		("g" . cj/deadgrep-here))    ;; search in context directory

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
  (keymap-set dired-mode-map "G" #'cj/deadgrep-here))


;; ---------------------------------- Snippets ---------------------------------
;; reusable code and text

(defun cj/--yas-activate-fundamental-extras ()
  "Activate `fundamental-mode' as an extra yasnippet mode in this buffer.
Hooked onto `yas-minor-mode-hook' so every buffer also consults
`snippets/fundamental-mode/' regardless of the buffer's own major mode.
This is what makes universal snippets like =<cj= work in any buffer."
  (yas-activate-extra-mode 'fundamental-mode))

(use-package yasnippet
  :demand t
  :bind
  ("C-c s n" . yas-new-snippet)
  ("C-c s e" . yas-visit-snippet-file)
  :hook (yas-minor-mode . cj/--yas-activate-fundamental-extras)
  :config
  (setq yas-snippet-dirs (list snippets-dir))
  (yas-reload-all)
  (yas-global-mode 1))

;; Most of the snippet keys start with "<" (=<cj=, =<for=, =<main=…), mirroring
;; org-tempo.  But `electric-pair-mode' pairs "<" into "<>" wherever the mode's
;; syntax table gives "<" paren syntax (org, and the prog modes that enable
;; pairing), so typing "<cj" lands as "<cj>"; expanding the "<cj" key then
;; strands the ">" after the snippet — the cj-comment fence comes out as
;; "#+end_src>", which breaks the cj-scan fence parser.  Inhibit pairing for the
;; open angle bracket globally; defer to the default for every other character.
(defun cj/--electric-pair-inhibit-angle (char)
  "Return non-nil to stop `electric-pair-mode' from pairing the angle CHAR.
Inhibit the open angle bracket so \"<\"-prefixed yasnippet keys expand cleanly;
defer to `electric-pair-default-inhibit' for any other CHAR."
  (or (eq char ?<)
      (electric-pair-default-inhibit char)))

(setq electric-pair-inhibit-predicate #'cj/--electric-pair-inhibit-angle)

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
  ;; Disable auto face coloring; the guide faces are left to the theme
  (setq highlight-indent-guides-auto-enabled nil)

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
;; Disabled: when test results land in *compilation* (F6 / projectile-test-
;; project), I want to read the output afterward, not have the window close
;; under me 1.5 s later. To re-enable globally, uncomment the block below.
;; A prog-mode-only variant would need to capture the originating buffer's
;; major-mode at compile-start (via advice on `compile') and gate the hook
;; on it — left out for now per the simpler comment-out path.
;;
;; (add-hook 'compilation-finish-functions
;; 		  (lambda (_buf str)
;; 			(if (null (string-match ".*exited abnormally.*" str))
;; 				;;no errors, make the compilation window go away in a few seconds
;; 				(progn
;; 				  (run-at-time
;; 				   "1.5 sec" nil 'delete-windows-on
;; 				   (get-buffer-create "*compilation*"))))))

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c s" "snippets menu"
    "C-c s n" "new snippet"
    "C-c s e" "edit snippet"
    "C-c s i" "insert snippet"
    "C-c p" "projectile menu"
    "C-c C-s" "symbol overlay"))

(provide 'prog-general)
;;; prog-general.el ends here
