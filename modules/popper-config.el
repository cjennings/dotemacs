;;; popper-config.el --- secondary buffers to popup -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Configuration for popper.el, which manages secondary buffers as popup windows.
;; Popup buffers (like *Messages*, help, and compilation output) are displayed in
;; a dedicated bottom window and can be easily toggled, cycled, or promoted to
;; regular windows.
;;
;; Keybindings:
;;   C-`   - Toggle popup window visibility
;;   M-`   - Cycle through open popup buffers
;;   C-M-` - Promote popup to regular window or demote back to popup
;;
;;; Code:

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-display-control-nil)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Async-native-compile-log\\*"
          help-mode
          compilation-mode))
  (add-to-list 'display-buffer-alist
               '(popper-display-control-p  ; Predicate to match popper buffers
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.5)))  ; Half the frame height
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'popper-config)
;;; popper-config.el ends here.
