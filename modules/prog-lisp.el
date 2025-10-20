;;; prog-lisp --- Lisp Specific Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Configuration for Emacs Lisp, Common Lisp (SLIME), and Scheme (Guile).

;; ==== Common Lisp Setup (SLIME + Quicklisp) ====
;;
;; Install SBCL:
;;   sudo pacman -S sbcl       # arch
;;   doas pkg install sbcl     # bsd
;;   sudo apt-get install sbcl # debian
;;
;; Install Quicklisp (Common Lisp package manager):
;;   curl -O http://beta.quicklisp.org/quicklisp.lisp
;;   sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
;;   sbcl --load ~/quicklisp/setup.lisp --eval "(ql:add-to-init-file)" --quit
;;
;; Install SLIME via Quicklisp:
;;   sbcl --eval "(ql:quickload :quicklisp-slime-helper)" --quit
;;
;; Readline support for SBCL REPL (outside Emacs):
;;   rlwrap sbcl

;;; Code:

(defvar common-lisp-program "/usr/bin/sbcl"
  "Path to Common Lisp implementation (SBCL, CCL, etc.).")

(defvar quicklisp-helper-path (expand-file-name "~/quicklisp/slime-helper.el")
  "Path to Quicklisp's SLIME helper file.")

(defvar guile-program "/usr/bin/guile"
  "Path to Guile Scheme implementation.")

;; Forward declarations for SLIME variables
(defvar inferior-lisp-program)
(defvar slime-contribs)
(defvar slime-repl-history-size)
(defvar slime-repl-history-file)

;; Forward declarations for Geiser variables
(defvar geiser-guile-binary)

;; Forward declarations for flycheck-package
(declare-function flycheck-package-setup "flycheck-package")

;; -------------------------------- Elisp Setup --------------------------------
;; preferences for Emacs Lisp editing

(defun cj/elisp-setup ()
  "Default code preferences for Emacs Lisp."
  (setq-local tab-width 4)              ;; set the tab width to 4 spaces
  (setq-local indent-tabs-mode nil)     ;; disable tab characters (use spaces)
  (setq-local fill-column 120)          ;; wrap code at this column
  (display-fill-column-indicator-mode)) ;; show where the fill-column is
(add-hook 'emacs-lisp-mode-hook 'cj/elisp-setup)

;; ---------------------------- Common Lisp Setup ------------------------------
;; preferences for Common Lisp editing

(defun cj/common-lisp-setup ()
  "Default code preferences for Common Lisp."
  (setq-local tab-width 2)              ;; Common Lisp standard is 2 spaces
  (setq-local indent-tabs-mode nil)     ;; use spaces, not tabs
  (setq-local fill-column 100))         ;; wrap at 100 columns
(add-hook 'lisp-mode-hook 'cj/common-lisp-setup)

;; ------------------------------ Emacs Lisp REPL ------------------------------

(use-package ielm
  :ensure nil ;; built-in
  :hook (ielm-mode . eldoc-mode)
  :config (setq ielm-prompt "elisp> "))

;; ----------------------------------- Eldoc -----------------------------------

(use-package eldoc
  :ensure nil ;; built-in
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-display-truncation-message nil))

;; -------------------------- ERT + Testing Libraries --------------------------
;; unit/regression testing framework
;; basic introduction: https://nullprogram.com/blog/2012/08/15/
;; https://www.gnu.org/software/emacs/manual/html_node/ert/
;; or: [[info:ert#User Input]]

(use-package ert
  :ensure nil) ;; built-into emacs

;; ---------------------------------- El-Mock ----------------------------------

(use-package el-mock
  :commands (with-mock mocklet mocklet-function)) ;; mock/stub framework

;; --------------------------------- Elisp Lint --------------------------------

(use-package elisp-lint
  :commands (elisp-lint-file elisp-lint-directory))

;; ------------------------------ Package Tooling ------------------------------

(use-package package-lint
  :commands (package-lint-current-buffer package-lint-batch-and-exit))

(use-package flycheck-package
  :after (flycheck package-lint)
  :config
  (flycheck-package-setup))

(use-package package-build
  :commands (package-build-archive package-build-current-recipe))

;; ----------------------------- Rainbow Delimiters ----------------------------

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode lisp-mode scheme-mode) . rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")     ;; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")     ;; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")     ;; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")     ;; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")     ;; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")     ;; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")     ;; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")     ;; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666"))    ;; dark gray

;; ----------------------------------- SLIME -----------------------------------
;; Superior Lisp Interaction Mode for Emacs (Common Lisp REPL/debugger)

(use-package slime
  :commands (slime slime-mode)
  :bind ("C-c L" . slime)
  :config
  ;; Load Quicklisp's SLIME helper if it exists
  (when (file-exists-p quicklisp-helper-path)
    (load quicklisp-helper-path))

  ;; Set Common Lisp implementation
  (setq inferior-lisp-program common-lisp-program)

  ;; Enable commonly-used SLIME contribs
  (setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf))

  ;; Better REPL history
  (setq slime-repl-history-size 1000)
  (setq slime-repl-history-file (expand-file-name "~/.emacs.d/slime-history.eld")))

;; -------------------------------- Geiser Guile -------------------------------
;; Scheme support in Emacs (Guile implementation)

(use-package geiser-guile
  :commands (geiser-guile)
  :bind ("C-c G" . geiser-guile)
  :config
  (setq geiser-guile-binary guile-program))

(provide 'prog-lisp)
;;; prog-lisp.el ends here
