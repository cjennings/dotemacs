;;; prog-webdev.el --- Web Development Packages and Settings -*- lexical-binding: t; coding: utf-8; -*-
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Open a project file and Emacs selects the right helper:
;; - *.json buffers drop into json-mode for quick structural edits.
;; - *.js buffers jump into js2-mode for linty feedback.
;; - Mixed HTML templates land in web-mode which chains Tide and CSS Eldoc.
;;
;; Workflow:
;; - Hit C-RET in web-mode to ask for completions; the command routes to Tide,
;;   company-css, or dabbrev based on the language at point.
;; - Eldoc messages come from `cj/eldoc-web-mode`, so keeping point over JS, CSS,
;;   or markup swaps the doc source automatically.
;; - New web buffers call `cj/setup-web-mode-mixed`, enabling Tide so goto-definition
;;   and rename are ready without extra setup.

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
  "Activate Tide mode for TypeScript development.
Calls Tide's setup, enables `eldoc-mode, and activates identifier highlighting."
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
  "Provide context-aware completion in `web-mode' buffers.
Determines the language at point (JavaScript, CSS, or markup)
and invokes the appropriate completion backend:
- JavaScript: uses `company-tide'
- CSS: uses `company-css'
- Other markup: uses `company-dabbrev-code'

This function is typically bound to \\[cj/complete-web-mode] in
`web-mode' buffers to provide intelligent completions based on
the current context."
  (interactive)
  (let ((current-scope (web-mode-language-at-pos (point))))
	(cond ((string-equal "javascript" current-scope)
		   (company-tide 'interactive))
		  ((string-equal "css" current-scope)
		   (company-css 'interactive))
		  (t
		   (company-dabbrev-code 'interactive)))))

(defun cj/eldoc-web-mode ()
  "Provide context-aware eldoc documentation in `web-mode' buffers.
Return appropriate documentation based on the language at point:
- JavaScript: uses `tide-eldoc-function' for TypeScript/JavaScript docs
- CSS: uses `css-eldoc-function' for CSS property documentation
- Other markup: returns nil (no documentation)

This function is designed to be used as the buffer-local value
of `eldoc-documentation-function' in `web-mode' buffers with
mixed content."
  (let ((current-scope (web-mode-language-at-pos (point))))
	(cond ((string-equal "javascript" current-scope)
		   (tide-eldoc-function))
		  ((string-equal "css" current-scope)
		   (css-eldoc-function))
		  (t
		   nil))))

(defun cj/setup-web-mode-mixed ()
  "Set up `web-mode' with Tide and context-aware eldoc support.
Enable `web-mode' for the current buffer and configure it for
mixed HTML/JavaScript/CSS content.  Activate Tide for JavaScript
support and configure eldoc to use `cj/eldoc-web-mode' for
context-aware documentation.

This function is typically used as an auto-mode entry point for
HTML files that contain embedded JavaScript and CSS."
  (web-mode)
  (cj/activate-tide)
  (setq-local eldoc-documentation-function #'cj/eldoc-web-mode))


(provide 'prog-webdev)
;;; prog-webdev.el ends here.
