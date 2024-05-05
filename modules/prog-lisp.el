;;; prog-lisp --- Lisp Specific Settings and Functionality -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; ==== Setting up Quicklisp ====
;; Quicklisp is a library manager for Common Lisp. It works with your existing Common Lisp
;; implementation to download, install, and load any of over 1,500 libraries with a few
;; simple commands.
;; https://www.quicklisp.org/beta/

;; mostly from: https://gist.github.com/jteneycke/7947353
;; * Install SBCL
;; sudo pacman -S sbcl       # arch
;; doas pkg install sbcl     # bsd
;; sudo apt-get install sbcl # debian

;; * Install QuickLisp
;; curl -O http://beta.quicklisp.org/quicklisp.lisp
;; sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
;; sbcl --load ~/quicklisp/setup.lisp --eval "(ql:add-to-init-file)" --quit

;; * Emacs Config
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; ;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "/usr/bin/sbcl")

;; to get readline support in SBCL's REPL, install rlwrap and run it
;; before sbcl like so:

;; $ rlwrap sbcl

;;; Code:
(require 'ert)

;; -------------------------------- Elisp Setup --------------------------------
;; run this on editing an elisp file

(defun cj/elisp-setup ()
  "My default code preferences for emacs-lisp."
  (setq-default tab-width 4)            ;; set the tab width to 4 spaces
  (setq-default indent-tabs-mode -1)    ;; disable tab characters
  (setq-default fill-column 80)         ;; default column for gnu projects
  (display-fill-column-indicator-mode)) ;; show where the 80th column is
(add-hook 'emacs-lisp-mode-hook 'cj/elisp-setup)

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
  :ensure nil ;; built-into emacs
  :defer 1)

;; mocking/stub framework
;; note: Find the documentation at M-x describe-function RET with-mock RET
;; and M-x describe-function RET mocklet RET.testing
(with-eval-after-load 'ert
  (defun ert-all-tests ()
	"Run all ert tests and display the results in a buffer."
	(interactive)
	(ert t))

  (keymap-global-unset "C-r" t)
  (keymap-global-unset "C-R" t)
  (define-key emacs-lisp-mode-map (kbd "C-r") 'ert-all-tests)
  (define-key emacs-lisp-mode-map (kbd "C-R") 'ert-run-tests-interactively)
  (define-key lisp-interaction-mode-map (kbd "C-r") 'ert-all-tests)
  (define-key lisp-interaction-mode-map (kbd "C-R") 'ert-run-tests-interactively))

(use-package el-mock) ;; mock/stub framework


(defun cj/eval-and-run-all-tests-in-buffer ()
  "Delete any loaded tests, evaluate current buffer, and run loaded ERT tests."
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert 't))

;; ------------------------------ Package Tooling ------------------------------

(use-package package-lint
  :defer 1)

(use-package flycheck-package
  :defer 1
  :after (flycheck package-lint)
  :config
  (flycheck-package-setup))

(use-package package-build
  :defer 1)

;; ----------------------------- Rainbow Delimiters ----------------------------

(use-package rainbow-delimiters
  :defer .5
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
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

;; -------------------------------- Geiser Guile -------------------------------
;; Guile support in Emacs

(use-package geiser-guile
  :defer 1
  :commands (geiser-guile)
  :bind ("C-c G" . geiser-guile)
  :config
  (setq geiser-guile-binary "/usr/bin/guile"))

(provide 'prog-lisp)
;;; prog-lisp.el ends here
