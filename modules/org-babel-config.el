;;; org-babel-config.el --- Org Babel/Tempo Config -*- lexical-binding: t; -*-

;;; Commentary:
;; All Org-Babel and Org-Tempo Packages, Settings, and Languages.

;;; Code:

(with-eval-after-load 'org
  (require 'ob-core)

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

  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((shell . t)))

  (use-package ob-python
	:ensure nil ;; built-in
	:defer .5
	:after org-contrib
	:commands (org-babel-execute:python))

  (use-package ob-emacs-lisp
	:ensure nil ;; built-in
	:defer .5
	:commands
	(org-babel-execute:emacs-lisp
	 org-babel-expand-body:emacs-lisp))

  (use-package ob-dot
	:ensure nil ;; built-in
	:defer .5
	:commands
	(org-babel-execute:dot
	 org-babel-expand-body:dot)
	:config
	;; https://stackoverflow.com/questions/16770868/org-babel-doesnt-load-graphviz-editing-mode-for-dot-sources
	(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot))))

  ;; --------------------------------- Org-Tempo ---------------------------------
  ;; expands snippets to babel code blocks using templates

  (use-package org-tempo
	:defer .5
	:ensure nil ;; built-in
	:after org
	:config
	(add-to-list 'org-structure-template-alist '("awk"   . "src awk"))
	(add-to-list 'org-structure-template-alist '("bash"  . "src bash"))
	(add-to-list 'org-structure-template-alist '("dot"   . "src dot :file temp.png :cmdline -Kdot -Tpng"))
	(add-to-list 'org-structure-template-alist '("el"    . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("json"  . "src json"))
	(add-to-list 'org-structure-template-alist '("latex" . "src latex"))
	(add-to-list 'org-structure-template-alist '("sicp"  . "src racket :lang sicp"))
	(add-to-list 'org-structure-template-alist '("py"    . "src python"))
	(add-to-list 'org-structure-template-alist '("sh"    . "src sh"))
	(add-to-list 'org-structure-template-alist '("yaml"  . "src yaml")))

  ;; ---------------------------- Org Babel Settings ---------------------------
  ;; general org babel settings

  (setq org-src-fontify-natively t)                                     ;; fontify the code in blocks
  (setq org-src-tab-acts-natively t)                                    ;; tabs act like in language major mode buffer
  (setq org-src-window-setup 'current-window)                           ;; don't split window when source editing wih C-c '
  (setq org-confirm-babel-evaluate nil)                                 ;; just evaluate the source code
  (setq org-babel-default-header-args
		(cons '(:tangle . "yes")
			  (assq-delete-all :tangle org-babel-default-header-args))) ;; default header args for babel
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images) ;; for seeing inline dotgraph images

  ) ;; end with-eval-after-load


(provide 'org-babel-config)
;;; org-babel-config.el ends here.
