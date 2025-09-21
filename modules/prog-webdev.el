;;; prog-webdev.el --- Web Development Packages and Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Open a project file and Emacs selects the right helper:
;; - *.json buffers drop into json-mode for quick structural edits.
;; - *.js buffers jump into js2-mode for linty feedback.
;; - Mixed HTML templates land in web-mode which chains Tide and CSS Eldoc.
;;
;; Workflow:
;; - Hit C-RET in web-mode to ask for completions; the command routes to Tide, company-css, or dabbrev based on the language at point.
;; - Eldoc messages come from `cj/eldoc-web-mode`, so keeping point over JS, CSS, or markup swaps the doc source automatically.
;; - New web buffers call `cj/setup-web-mode-mixed`, enabling Tide so goto-definition and rename are ready without extra setup.

;;; Code:

;; --------------------------------- JSON Mode ---------------------------------
;; mode for editing JavaScript Object Notation (JSON) data files

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :defer .5)

;; ---------------------------------- JS2 Mode ---------------------------------
;; javascript editing mode

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :defer .5)

;; --------------------------------- CSS Eldoc ---------------------------------
;; CSS info in the echo area

(use-package css-eldoc
  :defer .5)

;; ------------------------------------ Tide -----------------------------------
;; typescript interactive development environment

(use-package tide
  :defer .5)

(defun cj/activate-tide ()
  (interactive)
  (tide-setup)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1))

;; ---------------------------------- Web Mode ---------------------------------
;; major mode for editing web templates

(use-package web-mode
  :defer .5
  :after (tide css-eldoc)
  :custom
  (web-mode-enable-current-element-highlight t)
  :bind
  ([(control return)] . cj/complete-web-mode)
  :mode
  (("\\.html?$" . cj/setup-web-mode-mixed)))

(defun cj/complete-web-mode ()
  (interactive)
  (let ((current-scope (web-mode-language-at-pos (point))))
	(cond ((string-equal "javascript" current-scope)
		   (company-tide 'interactive))
		  ((string-equal "css" current-scope)
		   (company-css 'interactive))
		  (t
		   (company-dabbrev-code 'interactive)))))

(defun cj/eldoc-web-mode ()
  (let ((current-scope (web-mode-language-at-pos (point))))
	(cond ((string-equal "javascript" current-scope)
		   (tide-eldoc-function))
		  ((string-equal "css" current-scope)
		   (css-eldoc-function))
		  (t
		   nil))))

(defun cj/setup-web-mode-mixed ()
  (web-mode)
  (cj/activate-tide)
  (setq-local eldoc-documentation-function #'cj/eldoc-web-mode))


(provide 'prog-webdev)
;;; prog-webdev.el ends here.
