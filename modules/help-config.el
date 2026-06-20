;;; help-config --- Help Functionality Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P.
;; Load shape: eager.
;; Eager reason: help/info/man configuration and its keybindings; eager only by
;;   init order, a deferral candidate.
;; Top-level side effects: two global keys, package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; This module enhances Emacs' built-in help system and documentation features.
;; It configures:
;;
;; 1. Helpful - A better help buffer that provides context, examples, and source code
;; 2. Man - Man page viewing integration
;; 3. Info - Enhanced Info mode with custom keybindings and directory configuration
;;
;; The configuration prioritizes discoverability and improves the experience of
;; reading documentation within Emacs. Custom keybindings maintain the C-h prefix
;; convention for help-related commands.

;;; Code:


(setq help-window-select t) ;; Always select the help buffer in a separate window

(keymap-global-set "C-h P" #'list-packages) ;; bring up the package menu

;; ---------------------------------- Helpful ----------------------------------

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)
  ("C-h ." . helpful-at-point)
  ("C-h o" . helpful-symbol)) ;; overrides 'describe-symbol' keybinding

;; ------------------------------------ Man ------------------------------------

(use-package man
  :ensure nil ;; built-in
  :bind ("C-h M" . man))

;; ------------------------------------ Info -----------------------------------

(defun cj/--info-open-plan (modified-p save-confirmed-p)
  "Decide how to open a buffer in Info given its MODIFIED-P state.
SAVE-CONFIRMED-P is the answer to the save prompt, meaningful only when
MODIFIED-P.  Returns `open', `save-then-open', or `cancel'."
  (cond ((not modified-p) 'open)
        (save-confirmed-p 'save-then-open)
        (t 'cancel)))

(defun cj/open-with-info-mode ()
  "Open the current buffer's file in Info mode if it's a valid info file.

Preserves any unsaved changes and checks if the file exists."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (if (and (file-exists-p file-name)
               (string-match-p "\\.info\\'" file-name))
          (let ((modified (buffer-modified-p)))
            (pcase (cj/--info-open-plan
                    modified
                    (and modified
                         (y-or-n-p "Buffer has unsaved changes. Save before opening in Info? ")))
              ('cancel (message "Operation canceled"))
              (plan
               (when (eq plan 'save-then-open) (save-buffer))
               (kill-buffer (current-buffer))
               (info file-name))))
        (message "Not a valid info file: %s" file-name)))))

(defun cj/browse-info-files ()
  "Browse and open .info or .info.gz files from user-emacs-directory."
  (interactive)
  (let* ((info-files (directory-files-recursively
					  user-emacs-directory
					  "\\.info\\(\\.gz\\)?$"))
		 (files-alist (mapcar (lambda (f)
								(cons (file-name-nondirectory f) f))
							  info-files))
		 (chosen-name (completing-read
					   "Select Info file: "
					   (mapcar #'car files-alist)
					   nil t))
		 (chosen-file (cdr (assoc chosen-name files-alist))))
	(when chosen-file
	  (info chosen-file))))

(global-unset-key (kbd "C-h i"))
(keymap-global-set "C-h i" #'cj/browse-info-files)


(use-package info
  :ensure nil ;; built-in
  :bind
  (:map Info-mode-map
		("m" . bookmark-set) ;; Rebind 'm' from Info-menu to bookmark-set
		("M" . Info-menu)))   ;; Move Info-menu to 'M' instead

(provide 'help-config)
;;; help-config.el ends here.
