;;; org-babel-config.el --- Org Babel/Tempo Config -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; All Org-Babel and Org-Tempo Packages, Settings, and Languages.

;;; Code:

;; ------------------------------- Org Babel Core ------------------------------
;; general org babel settings

(use-package ob-core
  :ensure nil ;; built-in
  :after org
  :defer .5
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images)   ;; for seeing inline dotgraph images
  :config
  (setq org-src-fontify-natively t)                                     ;; fontify the code in blocks
  (setq org-src-tab-acts-natively t)                                    ;; tabs act like in language major mode buffer
  (setq org-src-window-setup 'current-window)                           ;; don't split window when source editing wih C-c '
  (setq org-confirm-babel-evaluate nil)                                 ;; just evaluate the source code
  (setq org-babel-default-header-args
        (cons '(:tangle . "yes")
              (assq-delete-all :tangle org-babel-default-header-args)))) ;; default header args for babel


;; ------------------- Babel Execution Confirmation Toggle -------------------
;; org-babel verifies before each execution

(defun babel-confirm (flag)
  "Report the setting of org-confirm-babel-evaluate.
   If invoked with C-u, toggle the setting."
  (interactive "P")
  (if (equal flag '(4))
      (setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate)))
  (message "Babel evaluation confirmation is %s"
           (if org-confirm-babel-evaluate "on" "off")))


;; ---------------------------- Org Babel Languages ----------------------------
;; create executable code blocks in a language within org-mode

(use-package ob-awk
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:awk
   org-babel-expand-body:awk))

(use-package ob-dot
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:dot
   org-babel-expand-body:dot)
  :config
  ;; https://stackoverflow.com/questions/16770868/org-babel-doesnt-load-graphviz-editing-mode-for-dot-sources
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot))))

(use-package ob-emacs-lisp
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:emacs-lisp
   org-babel-expand-body:emacs-lisp))

(use-package ob-latex
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:latex
   org-babel-expand-body:latex))

(use-package ob-python
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:python
   org-babel-variable-assignments:python
   org-babel-load-session:python
   org-babel-prep-session:python))

(use-package ob-scheme
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:scheme
   org-babel-expand-body:scheme))

;; allows for shell, bash, and fish
(use-package ob-shell
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands
  (org-babel-execute:shell
   org-babel-prep-session:shell
   org-babel-load-session:shell
   org-babel-variable-assignments:shell))

(use-package ob-sed
  :ensure nil ;; built-in
  :defer .5
  :after ob-core
  :commands (org-babel-execute:sed))

;; --------------------------------- Org-Tempo ---------------------------------
;; expands snippets to babel code blocks using templates

(use-package org-tempo
  :defer .5
  :ensure nil ;; built-in
  :after ob-core
  :config
  (add-to-list 'org-structure-template-alist '("awk"    . "src awk"))
  (add-to-list 'org-structure-template-alist '("sed"    . "src sed"))
  (add-to-list 'org-structure-template-alist '("bash"   . "src bash"))
  (add-to-list 'org-structure-template-alist '("dot"    . "src dot :file temp.png :cmdline -Kdot -Tpng"))
  (add-to-list 'org-structure-template-alist '("el"     . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("json"   . "src json"))
  (add-to-list 'org-structure-template-alist '("latex"  . "src latex"))
  (add-to-list 'org-structure-template-alist '("py"     . "src python"))
  (add-to-list 'org-structure-template-alist '("scheme" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("shell"  . "src shell"))
  (add-to-list 'org-structure-template-alist '("yaml"   . "src yaml")))

;; requires ob-racket, not yet in repositories
;;   (add-to-list 'org-structure-template-alist '("sicp"   . "src racket :lang sicp"))

;; drop Org’s default footnote list at the end
(setq org-html-footnote-separator "")

(provide 'org-babel-config)
;;; org-babel-config.el ends here.
