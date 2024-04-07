;;; ui-config --- User Interface Preferences -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ----------------------------- System UI Settings ----------------------------

(add-to-list 'initial-frame-alist '(fullscreen . maximized))	;; start the initial frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))	;; start every frame maximized
(setq pixel-scroll-precision-mode nil)							;; smooth scroll past images - enabled if nil!

(setq-default frame-inhibit-implied-resize t)					;; don't resize frames when setting ui-elements
(setq frame-title-format '("Emacs " emacs-version " - %b"))		;; the title is just the app name and version

(setq use-file-dialog nil)										;; no file dialog
(setq use-dialog-box nil)										;; no dialog boxes either
(column-number-mode 1)											;; show column number in the modeline
(setq switch-to-buffer-obey-display-actions t)                  ;; manual buffer switching obeys display action rules

;; -------------------------------- Transparency -------------------------------

(set-frame-parameter (selected-frame) 'alpha '(84 84))
(add-to-list 'default-frame-alist '(alpha 84 84))

;; ----------------------------------- Cursor ----------------------------------
;; set cursor color according to mode
;;
;;    #f06a3f indicates a read-only document
;;    #c48702 indicates overwrite mode
;;    #64aa0f indicates insert and read/write mode


(defvar cj/set-cursor-color-color "")
(defvar cj/set-cursor-color-buffer "")

(defun cj/set-cursor-color-according-to-mode ()
  "Change the cursor color based on selected minor modes.
Cursor becomes a red hue when in a read-only buffer,
turns goldenrod when in overwrite mode, and green otherwise."
  ;; set-cursor-color is somewhat costly, so only call it when needed:
  (let ((color
         (if buffer-read-only "#f06a3f"
           (if overwrite-mode "#c48702"
             "#64aa0f"))))
    (unless (and
             (string= color cj/set-cursor-color-color)
             (string= (buffer-name) cj/set-cursor-color-buffer))
      (set-cursor-color (setq cj/set-cursor-color-color color))
      (setq cj/set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'cj/set-cursor-color-according-to-mode)
(setq cursor-in-non-selected-windows 'nil) ;; don't show cursor in unselected windows

;; SET-CURSOR-TYPE
(defun cj/set-cursor-type (new-cursor-type)
  "Set the cursor type of the selected frame to NEW-CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'."
  (interactive
   (list (intern (completing-read
                  "Cursor type: "
                  (mapcar 'list '("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters (selected-frame)
						   (list (cons 'cursor-type new-cursor-type))))
(cj/set-cursor-type 'box) ;; start with the box cursor

;; --------------------------------- Nerd Icons --------------------------------
;; use icons from nerd fonts in the Emacs UI

(use-package nerd-icons
  :demand t)


(provide 'ui-config)
;;; ui-config.el ends here
