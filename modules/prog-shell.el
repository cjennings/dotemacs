;;; prog-shell --- Shell Programming Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Modern shell scripting environment with LSP, tree-sitter, linting, and formatting.
;;
;; Installation:
;;   sudo pacman -S shellcheck shfmt   # Linter and formatter
;;   npm install -g bash-language-server
;;
;; Features:
;;   - LSP: Intelligent completion, hover docs, jump to definition
;;   - ShellCheck: Industry-standard linting (catches common bugs)
;;   - shfmt: Google's shell formatter (consistent style)
;;   - Tree-sitter: Better syntax highlighting via bash-ts-mode
;;   - Auto-executable: Scripts with shebangs auto-get execute permission
;;
;; Workflow:
;;   1. Open .sh file → LSP auto-starts, ShellCheck runs
;;   2. <f6> → Format with shfmt
;;   3. C-c ! l → Show all ShellCheck diagnostics
;;   4. Save → Auto-set executable bit if script has shebang

;;; Code:

(defvar bash-ts-mode-map)
(defvar sh-mode-map)

;; Forward declarations for LSP
(declare-function lsp-deferred "lsp-mode")
(defvar lsp-bash-explainshell-endpoint)
(defvar lsp-bash-highlight-parsing-errors)

;; Forward declarations for sh-script
(defvar sh-learn-basic-offset)

;; Forward declarations for flycheck
(defvar flycheck-shellcheck-follow-sources)
(defvar flycheck-shellcheck-excluded-warnings)
(defvar flycheck-checkers)

;; Forward declarations for system utilities
(declare-function cj/disabled "system-defaults")

(defvar bash-language-server-path "bash-language-server"
  "Path to bash-language-server executable.
Install with: npm install -g bash-language-server")

(defvar shfmt-path "shfmt"
  "Path to shfmt executable.
Install with: sudo pacman -S shfmt")

(defvar shellcheck-path "shellcheck"
  "Path to shellcheck executable.
Install with: sudo pacman -S shellcheck")

;; ------------------------------- Shell Script Setup ------------------------------
;; preferences for shell scripting

(defun cj/shell-script-setup ()
  "Settings for shell script editing (bash, sh, zsh)."
  (setq-local indent-tabs-mode nil)     ;; use spaces, not tabs
  (setq-local sh-basic-offset 2)        ;; 2 spaces (common shell convention)
  (setq-local tab-width 2)              ;; tab displays as 2 spaces
  (setq-local fill-column 80)           ;; wrap at 80 columns
  (electric-pair-mode t)                ;; automatic quote/bracket pairing

  ;; Enable LSP if available
  (when (and (fboundp 'lsp-deferred)
             (executable-find bash-language-server-path))
    (lsp-deferred)))

(defun cj/shell-run-shellcheck ()
  "Run shellcheck on the current shell script."
  (interactive)
  (if (executable-find shellcheck-path)
      (if buffer-file-name
          (compile (format "%s %s" shellcheck-path (shell-quote-argument buffer-file-name)))
        (message "No file associated with this buffer"))
    (message "shellcheck not found. Install with: sudo pacman -S shellcheck")))

(defun cj/shell-mode-keybindings ()
  "Set up keybindings for shell script editing.
Overrides default prog-mode keybindings with shell-specific commands."
  ;; S-f5: Run shellcheck (static analysis)
  (local-set-key (kbd "S-<f5>") #'cj/shell-run-shellcheck)

  ;; S-f6: Disabled (shell scripts don't have interactive debugging like gdb/pdb)
  (local-set-key (kbd "S-<f6>") #'cj/disabled))

;; Apply to both legacy sh-mode and modern bash-ts-mode
(add-hook 'sh-mode-hook 'cj/shell-script-setup)
(add-hook 'bash-ts-mode-hook 'cj/shell-script-setup)
(add-hook 'sh-mode-hook 'cj/shell-mode-keybindings)
(add-hook 'bash-ts-mode-hook 'cj/shell-mode-keybindings)

;; -------------------------------- Shell Scripts ----------------------------------
;; built-in shell script mode configuration

(use-package sh-script
  :ensure nil  ;; built-in
  :mode (("\\.sh\\'" . bash-ts-mode)        ;; .sh files use bash-ts-mode
         ("\\.bash\\'" . bash-ts-mode)      ;; .bash files
         ("\\.zsh\\'" . sh-mode)            ;; zsh doesn't have ts-mode yet
         ("/PKGBUILD\\'" . bash-ts-mode))   ;; Arch Linux PKGBUILDs
  :config
  ;; Set default shell type
  (setq sh-shell-file "/bin/bash")

  ;; Improve shell script detection
  (setq sh-learn-basic-offset t))

;; -------------------------------- LSP for Shell ----------------------------------
;; Shell script LSP configuration using bash-language-server
;; Core LSP setup is in prog-general.el

(use-package lsp-mode
  :hook ((sh-mode bash-ts-mode) . lsp-deferred)
  :config
  ;; Configure bash-language-server
  (setq lsp-bash-explainshell-endpoint nil)  ;; Disable external API calls
  (setq lsp-bash-highlight-parsing-errors t))

;; --------------------------------- ShellCheck ------------------------------------
;; Industry-standard shell script linter

(use-package flycheck
  :if (executable-find shellcheck-path)
  :hook ((sh-mode bash-ts-mode) . flycheck-mode)
  :config
  ;; Prefer ShellCheck over basic sh linter
  (setq flycheck-shellcheck-follow-sources t)
  (setq flycheck-shellcheck-excluded-warnings '("SC2086"))  ;; Customize as needed

  ;; Use ShellCheck for shell scripts
  (add-to-list 'flycheck-checkers 'sh-shellcheck))

;; -------------------------------- Formatting -------------------------------------
;; Format shell scripts with shfmt

(use-package shfmt
  :if (executable-find shfmt-path)
  :hook ((sh-mode bash-ts-mode) . shfmt-on-save-mode)
  :bind ((:map sh-mode-map
               ("<f6>" . shfmt-buffer)
               ("C-; f" . shfmt-buffer))
         (:map bash-ts-mode-map
               ("<f6>" . shfmt-buffer)
               ("C-; f" . shfmt-buffer)))
  :custom
  (shfmt-arguments '("-i" "2"      ;; indent with 2 spaces
                    "-ci"          ;; indent switch cases
                    "-bn")))       ;; binary ops like && and | may start a line

;; ---------------------------- Auto-Executable Scripts ----------------------------
;; Automatically set execute permission on shell scripts with shebangs

(defun cj/make-script-executable ()
  "Make the current file executable if it has a shebang."
  (when (and buffer-file-name
             (not (file-executable-p buffer-file-name))
             (save-excursion
               (goto-char (point-min))
               (looking-at "^#!")))
    (set-file-modes buffer-file-name
                   (logior (file-modes buffer-file-name) #o111))
    (message "Made %s executable" (file-name-nondirectory buffer-file-name))))

(add-hook 'after-save-hook 'cj/make-script-executable)

(provide 'prog-shell)
;;; prog-shell.el ends here
