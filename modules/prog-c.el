;;; prog-c --- C Programming Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Modern C programming environment with LSP, tree-sitter, debugging, and formatting.
;;
;; Installation:
;;   sudo pacman -S clang          # Provides clangd and clang-format
;;
;; LSP will provide:
;;   - Intelligent code completion
;;   - Jump to definition (M-.)
;;   - Find references
;;   - On-the-fly error checking
;;   - Documentation on hover
;;
;; Workflow Example:
;;   1. Open a .c file → LSP auto-starts, provides completions
;;   2. S-<f2> → Compile (auto-detects Makefile/CMake/single file)
;;   3. <f5> → Quick recompile
;;   4. S-<f3> → Start GDB with multi-window layout
;;   5. <f6> or C-c f → Format code with clang-format
;;   6. M-. → Jump to function definition
;;   7. C-c l → Access LSP commands (rename, find references, etc.)

;;; Code:

(defvar c-ts-mode-map)
(defvar c-mode-base-map)
(defvar c-default-style)

;; Forward declarations for LSP
(declare-function lsp-deferred "lsp-mode")
(defvar lsp-idle-delay)
(defvar lsp-log-io)
(defvar lsp-enable-folding)
(defvar lsp-enable-snippet)
(defvar lsp-headerline-breadcrumb-enable)

;; Forward declarations for compile
(declare-function recompile "compile")

(defvar clangd-path "clangd"
  "Path to clangd language server executable.")

(defvar clang-format-path "clang-format"
  "Path to clang-format executable.")

;; -------------------------------- C Mode Setup -------------------------------
;; preferences for C programming following common conventions

(defun cj/c-mode-settings ()
  "Settings for C programming (works with both c-mode and c-ts-mode)."
  (setq-local indent-tabs-mode nil)         ;; use spaces, not tabs
  (setq-local c-basic-offset 4)             ;; 4 spaces per indent level
  (setq-local tab-width 4)                  ;; tab displays as 4 spaces
  (setq-local fill-column 80)               ;; wrap at 80 columns
  (setq-local comment-auto-fill-only-comments t) ;; only auto-fill inside comments
  (auto-fill-mode)                          ;; auto-fill multiline comments
  (electric-pair-mode)                      ;; automatic parenthesis pairing

  ;; Enable LSP if available
  (when (and (fboundp 'lsp-deferred)
             (executable-find clangd-path))
    (lsp-deferred)))

;; Apply to both legacy c-mode and modern c-ts-mode
(add-hook 'c-mode-hook 'cj/c-mode-settings)
(add-hook 'c-ts-mode-hook 'cj/c-mode-settings)

;; Set default C style globally (before modes load)
(setq c-default-style '((c-mode . "stroustrup")
                        (c-ts-mode . "stroustrup")
                        (other . "gnu")))

;; -------------------------------- LSP for C ----------------------------------
;; C-specific LSP configuration using clangd
;; Core LSP setup is in prog-general.el

(use-package lsp-mode
  :hook ((c-mode c-ts-mode) . lsp-deferred)
  :custom
  (lsp-clients-clangd-executable clangd-path)
  (lsp-clients-clangd-args '("--header-insertion-decorators=0"
                            "--clang-tidy"
                            "--completion-style=detailed"
                            "--background-index")))

;; ----------------------------- Code Formatting -------------------------------
;; Format C code using clang-format

(use-package clang-format
  :if (executable-find clang-format-path)
  :bind (:map c-mode-base-map
              ("<f6>" . clang-format-buffer)
              ("C-c f" . clang-format-buffer)))

;; -------------------------------- Compilation --------------------------------
;; Smart compilation with project detection

(defun cj/c-compile-command ()
  "Set buffer-local compile command based on project structure."
  (let* ((makefile (locate-dominating-file default-directory "Makefile"))
         (cmakefile (locate-dominating-file default-directory "CMakeLists.txt")))
    (cond
     (makefile
      (setq-local compile-command
                  (format "cd %s && make -k " (shell-quote-argument makefile))))
     (cmakefile
      (setq-local compile-command
                  (format "cd %s && cmake --build build " (shell-quote-argument cmakefile))))
     (t
      ;; Single file compilation
      (setq-local compile-command
                  (format "gcc -Wall -Wextra -g -o %s %s"
                          (file-name-sans-extension (buffer-name))
                          (buffer-name)))))))

(add-hook 'c-mode-hook 'cj/c-compile-command)
(add-hook 'c-ts-mode-hook 'cj/c-compile-command)

;; -------------------------------- Debugging ----------------------------------
;; Enhanced GDB integration

(use-package gdb-mi
  :ensure nil  ;; built-in
  :custom
  (gdb-many-windows t)          ;; Show multiple windows (source, locals, stack, etc.)
  (gdb-show-main t)             ;; Show main source window
  (gdb-display-io-natively t))  ;; Display program I/O in separate window

;; -------------------------------- Keybindings --------------------------------

(defun cj/c-mode-keybindings ()
  "Set up keybindings for C programming."
  (local-set-key (kbd "S-<f2>") #'compile)
  (local-set-key (kbd "S-<f3>") #'gdb)
  (local-set-key (kbd "<f5>") #'recompile))

(add-hook 'c-mode-hook 'cj/c-mode-keybindings)
(add-hook 'c-ts-mode-hook 'cj/c-mode-keybindings)

(provide 'prog-c)
;;; prog-c.el ends here
