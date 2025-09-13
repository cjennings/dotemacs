;;; keybindings --- General Keyboard Shortcuts -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; TLDR: "C-c ?" Should bring up a helpful menu from wherever you are.

;; I've created a general menu that contains commonly used applications and
;; utilities.

;; There are also helpful menus that describe common functionality from within
;; another package, e.g., ledger. You access these with the same "C-c ? keyboard
;; shortcut"

;; Also, commonly used files should be easy to jump to. The "jump-to" keymap
;; refers to files defined in user-constants.el.

;; "Hostile Keybindings" are those that are close to keybindings I use commonly
;; so they're easy to  hit by accident, but they have painful results. I'd
;; rather avoid the pain by unsetting they keybindings and view an error '<key>
;; is undefined' message. Finally, I'm providing messages to train me to use
;; faster keybindings and provide feedback when evaluating buffers.

;;; Code:


(require 'user-constants)

;; make org-store-link binding global
(global-set-key (kbd "C-c l") 'org-store-link)

;; remap Shift Backspace to Delete
(global-set-key (kbd "S-<backspace>") 'delete-forward-char)

;; ------------------------------ Jump To Commands -----------------------------
;; quick access for commonly used files

(defvar jump-to-keymap (make-sparse-keymap)
  "Jump-to commonly used files/directories/commands.")
(global-set-key (kbd "C-c j") jump-to-keymap)

(define-key jump-to-keymap (kbd "r")
			#'(lambda () (interactive) (find-file reference-file)))
(define-key jump-to-keymap (kbd "s")
			#'(lambda () (interactive) (find-file schedule-file)))
(define-key jump-to-keymap (kbd "i")
			#'(lambda () (interactive) (find-file inbox-file)))
(define-key jump-to-keymap (kbd "c")
			#'(lambda () (interactive) (find-file contacts-file)))
(define-key jump-to-keymap (kbd "a")
			#'(lambda () (interactive) (find-file article-file)))
(define-key jump-to-keymap (kbd "A")
			#'(lambda () (interactive) (find-file article-archive)))
(define-key jump-to-keymap (kbd "$")
			#'(lambda () (interactive) (find-file ledger-file)))
(define-key jump-to-keymap (kbd "m")
			#'(lambda () (interactive) (find-file macros-file)))
(define-key jump-to-keymap (kbd "n")
			#'(lambda () (interactive) (find-file reading-notes-file)))
(define-key jump-to-keymap (kbd "w")
			#'(lambda () (interactive) (find-file webclipped-file)))
(define-key jump-to-keymap (kbd "I")
			#'(lambda () (interactive) (find-file emacs-init-file)))


;; ---------------------------- Keybinding Discovery ---------------------------

(use-package free-keys
  :defer 1
  :bind ("C-h C-k" . free-keys))

(use-package which-key
  :defer 1
  :config
  (setq which-key-idle-delay 3.0
		which-key-popup-type 'side-window)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode 1))

;; ---------------------------- General Keybindings ----------------------------

;; Avoid hostile bindings
(global-unset-key (kbd "C-x C-f"))   ;; find-file-read-only
(global-unset-key (kbd "C-z"))       ;; suspend-frame is accidentally hit often
(global-unset-key (kbd "M-o"))       ;; facemenu-mode

;; Add commonly-used general keybindings
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-c f")   'link-hint-open-link-at-point)
(global-set-key (kbd "M-*")     'calculator)

;; Normally bound to ESC ESC ESC, hit ESC once to get out of unpleasant situations.
(global-set-key (kbd "<escape>")  'keyboard-escape-quit)

;; remap C-x \ to sort-lines (from remap activate-transient-input-method)
(global-unset-key (kbd "C-x \\"))
(global-set-key (kbd "C-x \\") 'sort-lines)

;; training myself to use C-/ for undo (bound internally) as it's faster.
(if (display-graphic-p)
	(progn
	  (global-unset-key (kbd "C-x u"))
	  (define-key global-map (kbd "C-x u")
				  #'(lambda () (interactive)
					  (message (concat "Seriously, " user-name
									   "? Use 'C-/'. It's faster."))))))

;; evaluating a buffer should give confirmation or error.
(defun cj/eval-buffer-with-confirmation-or-error-message ()
  "Evaluate the buffer and display a message."
  (interactive)
  (let ((result (eval-buffer)))
    (if (not (eq result 'error))
        (message "Buffer evaluated.")
      (message "error occurred during evaluation: %s" result))))
(global-set-key (kbd "C-c b")   'cj/eval-buffer-with-confirmation-or-error-message)

(provide 'keybindings)
;;; keybindings.el ends here
