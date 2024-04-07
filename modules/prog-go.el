;;; prog-go --- Golang Specific Settings and Functionality -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:


(defun cj/go-setup ()
  "My default code preferences for Golang."
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (tree-sitter-hl-mode)
  (hs-minor-mode)
  (company-mode)
  (setq-default tab-width 4)            ;; set the tab width to 4 spaces
  (setq-default standard-indent 4)      ;; indent 4 spaces
  (setq-default indent-tabs-mode nil)   ;; disable tab characters
  (electric-pair-mode t))               ;; match delimiters automatically
(add-hook 'go-mode-hook 'cj/go-setup)

;;;; ---------------------------- Go Mode ----------------------------

(use-package go-mode
  :bind (:map go-mode-map
			  ("<f6>"   . gofmt)
			  ("C-c 6"  . gofmt)
			  ("<f4>"   . golint)
			  ("C-c 4"  . golint))
  :config
  (add-to-list 'exec-path "~/go/bin")
  ;; allow adding/removing fmt lines; install with:
  ;; go install golang.org/x/tools/cmd/goimports@latest
  (setq gofmt-command "goimports"))

;; (use-package go-mode
;;   :config
;;   (general-define-key
;;    :keymaps 'go-mode-map
;;    :states '(normal)
;;    "K" #'godoc-at-point
;;    "C-]" #'godef-jump)

;;   (general-define-key
;;    :keymaps 'go-mode-map
;;    :states '(normal)
;;    :prefix mpereira/leader
;;    "tt" #'go-test-current-test
;;    "tT" #'go-test-current-file
;;    "pt" #'go-test-current-project))

;; ------------- Configure Emacs To Find Go Project Root -------------

;; Note: This appears to interfere with tramp. Before re-enabling, this
;; should have a toggle and turned off when working in tramp.

;; (require 'project)

;; (defun project-find-go-module (dir)
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;  (cons 'go-module root)))

;; (cl-defmethod project-root ((project (head go-module)))
;;   (cdr project))

;; (add-hook 'project-find-functions #'project-find-go-module)

;; -------------------- Enable Eglot Integrations --------------------

;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
;; (defun eglot-format-buffer-on-save ()
;;   (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
;; (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;; -------------------- Configure Gopls Via Eglot --------------------

;; (setq-default eglot-workspace-configuration
;; 			  '((:gopls .
;; 						((staticcheck . t)
;; 						 (matcher . "CaseSensitive")))))

(provide 'prog-go)
;;; prog-go.el ends here
