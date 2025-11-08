;;; ui-config --- User Interface Preferences -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; This file centralizes user interface preferences, including:

;; • Frame and window behavior
;;   – Start all frames maximized
;;   – Disable file‐ and dialog‐boxes
;;   – Pixel scroll precision
;;   – Show column numbers in the mode-line

;; • Transparency controls
;;   – Customizable variables 'cj/enable-transparency' and 'cj/transparency-level'
;;   – Interactive 'cj/toggle-transparency' command

;; • Cursor appearance
;;   – Cursor color changes on the buffer's write and insertion state
;;     (i.e., read-only, overwrite, normal)
;;   – Option to customize cursor shape with 'cj/set-cursor-type'

;; • Icons
;;   – Load and enable 'nerd-icons' for UI glyphs

;; Customize the transparency and cursor color options at the top of this file.

;;; Code:

;; -------------------------------- UI Constants -------------------------------

(defvar cj/enable-transparency nil
  "Non-nil means use `cj/transparency-level' for frame transparency.")

(defvar cj/transparency-level 84
  "Opacity level for Emacs frames when `cj/enable-transparency' is non-nil.
100 = fully opaque, 0 = fully transparent.")

;; Use buffer status colors from user-constants
(require 'user-constants)

;; ----------------------------- System UI Settings ----------------------------

(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; start the initial frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start every frame maximized
(setq pixel-scroll-precision-mode nil)						 ;; disabled for performance

(setq-default frame-inhibit-implied-resize t)				 ;; don't resize frames when setting ui-elements
(setq frame-title-format '("Emacs " emacs-version " : %b"))	 ;; the title is emacs with version and buffer name

(setq use-file-dialog nil)									 ;; no file dialog
(setq use-dialog-box nil)									 ;; no dialog boxes either
(line-number-mode 1)                                         ;; show line number in the modeline (cached)
(column-number-mode 1)										 ;; show column number in the modeline (cached)
(setq switch-to-buffer-obey-display-actions t)               ;; manual buffer switching obeys display action rules

;; -------------------------------- Transparency -------------------------------

(defun cj/apply-transparency ()
  "Apply `cj/transparency-level' to the selected frame and future frames.

When `cj/enable-transparency' is nil, reset alpha to fully opaque."
  (let ((alpha (if cj/enable-transparency
				   (cons cj/transparency-level cj/transparency-level)
				 '(100 . 100))))
	;; apply to current frame (skip if terminal frame)
	(when (display-graphic-p)
	  (condition-case err
		  (set-frame-parameter nil 'alpha alpha)
		(error (message "Failed to set transparency: %s" (error-message-string err)))))
	;; update default for new frames
	(setq default-frame-alist
		  (assq-delete-all 'alpha default-frame-alist))
	(add-to-list 'default-frame-alist `(alpha . ,alpha))))

;; apply once at startup
(cj/apply-transparency)

(defun cj/toggle-transparency ()
  "Toggle `cj/enable-transparency' and re-apply."
  (interactive)
  (setq cj/enable-transparency (not cj/enable-transparency))
  (cj/apply-transparency)
  (message "Transparency %s"
		   (if cj/enable-transparency "enabled" "disabled")))

;; ----------------------------------- Cursor ----------------------------------
;; set cursor color according to mode
;;
;;    #f06a3f indicates a read-only document
;;    #c48702 indicates overwrite mode
;;    #64aa0f indicates insert and read/write mode

(defvar cj/-cursor-last-color nil
  "Last color applied by `cj/set-cursor-color-according-to-mode'.")
(defvar cj/-cursor-last-buffer nil
  "Last buffer name where cursor color was applied.")

(defun cj/set-cursor-color-according-to-mode ()
  "Change cursor color according to \\='buffer-read-only or \\='overwrite state."
  (let* ((state (cond
				 (buffer-read-only 'read-only)
				 (overwrite-mode   'overwrite)
				 (t                'normal)))
		 (color (alist-get state cj/buffer-status-colors)))
	(unless (and (string= color cj/-cursor-last-color)
				 (string= (buffer-name) cj/-cursor-last-buffer))
	  (set-cursor-color color)
	  (setq cj/-cursor-last-color color
			cj/-cursor-last-buffer (buffer-name)))))

;; Use more efficient hooks instead of post-command-hook for better performance
(add-hook 'window-buffer-change-functions
		  (lambda (_window) (cj/set-cursor-color-according-to-mode)))
(add-hook 'read-only-mode-hook #'cj/set-cursor-color-according-to-mode)
(add-hook 'overwrite-mode-hook #'cj/set-cursor-color-according-to-mode)

;; Don’t show a cursor in non-selected windows:
(setq cursor-in-non-selected-windows nil)

;; Initialize to box cursor (or any type you prefer)
(defun cj/set-cursor-type (new-cursor-type)
  "Set the cursor type of the selected frame to NEW-CURSOR-TYPE."
  (interactive
   (list (intern (completing-read
				  "Cursor type: "
				  (mapcar #'list '("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters nil `((cursor-type . ,new-cursor-type))))

(cj/set-cursor-type 'box)

;; --------------------------------- Nerd Icons --------------------------------
;; use icons from nerd fonts in the Emacs UI

(use-package nerd-icons
  :defer t)

(provide 'ui-config)
;;; ui-config.el ends here
