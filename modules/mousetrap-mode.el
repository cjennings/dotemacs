;;; mousetrap-mode.el ---  -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;; Mouse Trap Mode is a minor mode for Emacs that globally disables most mouse and
;; trackpad events to prevent accidental text modifications. Hitting the trackpad and
;; finding my text is being inserted in an unintended place is quite annoying,
;; especially when you're overcaffeinated.
;;
;; The mode  unbinds almost every mouse event, including clicks, drags, and wheel
;; movements, with various modifiers like Control, Meta, and Shift.
;;
;; Inspired by this blog post from Malabarba
;; https://endlessparentheses.com/disable-mouse-only-inside-emacs.html
;;
;;; Code:

;; ------------------------------ Mouse Trap Mode ------------------------------

(defvar mouse-trap-mode-map
  (let* ((prefixes '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-")) ; modifiers
		 (buttons  (number-sequence 1 5))                             ; mouse-1..5
		 (types    '("mouse" "down-mouse" "drag-mouse"
					 "double-mouse" "triple-mouse"))
		 (wheel    '("wheel-up" "wheel-down" "wheel-left" "wheel-right"))
		 (map (make-sparse-keymap)))
	;; clicks, drags, double, triple
	(dolist (type types)
	  (dolist (pref prefixes)
		(dolist (n buttons)
		  (define-key map (kbd (format "<%s%s-%d>" pref type n)) #'ignore))))
	;; wheel
	(dolist (evt wheel)
	  (dolist (pref prefixes)
		(define-key map (kbd (format "<%s%s>" pref evt)) #'ignore)))
	map)
  "Keymap for `mouse-trap-mode'. Unbinds almost every mouse event.

Disabling mouse prevents accidental mouse moves modifying text.")

(define-minor-mode mouse-trap-mode
  "Globally disable most mouse and trackpad events.

When active, <mouse-*>, <down-mouse-*>, <drag-mouse-*>,
<double-mouse-*>, <triple-mouse-*>, and wheel events are bound to `ignore',
with or without C-, M-, S- modifiers."
  :global t
  :lighter " 🐭"
  :keymap mouse-trap-mode-map)
(global-set-key (kbd "C-c M") #'mouse-trap-mode)
(mouse-trap-mode 1)

(provide 'mousetrap-mode)
;;; mousetrap-mode.el ends here.
