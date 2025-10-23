;;; keyboard-macros.el --- Keyboard Macro Management  -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; This library provides a simple, end-user–focused interface for
;; creating, naming, saving, and replaying keyboard macros in Emacs.
;; All commands are built on top of the built-in =kmacro= machinery, but
;; add a lightweight workflow and persistence across sessions.
;;
;; Workflow:
;;
;; 1. Start recording with C-F3 (or M-x cj/kbd-macro-start-or-end)
;;    This toggles macro recording on.
;;    Now you can perform all the edits you want recorded in the macro.
;;
;; 2. Stop recording with C-F3 (or M-x cj/kbd-macro-start-or-end)
;;    This stops recording and the macro becomes the "last keyboard macro."
;;
;; 3. Replay your macro <f3> (or M-x call-last-kbd-macro)
;;
;; 4. Name your macro with M-<F3>
;;    You will be prompted for a short name (e.g. =align-comments=,
;;    =cleanup-trail-spaces=).  This name is how you'll refer to it later.
;;
;; 5. Recall that macro later with M-x [the name you gave the macro]
;;
;; 6. View all your saved macros with s-<f3> (super-f3)
;;
;; 7. All macros are lazy-loaded when needed.
;;    When you first use keyboard macro functionality or open the macros file,
;;    all your previously saved macros are loaded into the current session.
;;
;;; Code:

(require 'subr-x) ;; for string-trim
(eval-when-compile (require 'user-constants))

(defvar cj/macros-loaded nil
  "Whether saved keyboard macros have been loaded from file.")

(defvar cj/macros-loading nil
  "Lock to prevent concurrent macro loading.")

(defun cj/ensure-macros-loaded ()
  "Load keyboard macros from file if not already loaded.
This function is idempotent and fast when macros are already loaded."
  (when (and (not cj/macros-loaded)
             (not cj/macros-loading)
             (file-exists-p macros-file))
    (setq cj/macros-loading t)
    (condition-case err
        (progn
          (load macros-file)
          (setq cj/macros-loaded t))
      (error
	   (message "Error loading keyboard macros file: %s"
                (error-message-string err))))
    (setq cj/macros-loading nil)))

(defun ensure-macros-file (file)
  "Ensure FILE exists and its first line enables `lexical-binding'."
  (unless (file-exists-p file)
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t -*-\n"))))

(defun cj/kbd-macro-start-or-end ()
  "Toggle start/end of keyboard macro definition."
  (interactive)
  ;; Lazy load macros on first use
  (cj/ensure-macros-loaded)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

(defun cj/save-maybe-edit-macro (name)
  "Save last macro as NAME in `macros-file'.
With prefix arg, open the macros file for editing after saving."
  (interactive "sName of macro: ")
  ;; Validate macro name
  (when (string-empty-p (string-trim name))
    (user-error "Macro name cannot be empty"))
  (unless (string-match-p "^[a-zA-Z][a-zA-Z0-9-]*$" name)
    (user-error "Macro name must start with a letter and contain only letters, numbers, and hyphens"))
  ;; Check if there's a macro to save
  (unless last-kbd-macro
    (user-error "No keyboard macro defined"))
  (kmacro-name-last-macro (intern name))
  (ensure-macros-file macros-file)
  (let ((original-buffer (current-buffer))
        (macros-buffer (find-file-noselect macros-file)))
    (condition-case err
        (progn
          (with-current-buffer macros-buffer
            (goto-char (point-max))
            (newline)
            (insert-kbd-macro (intern name))
            (newline)
            (save-buffer))
          (if current-prefix-arg
              (switch-to-buffer macros-buffer)
            (when (not (eq original-buffer (get-file-buffer macros-file)))
              (switch-to-buffer original-buffer)))
          (message "Macro '%s' saved to %s" name macros-file))
      (error
       (message "Error saving macro: %s" (error-message-string err))
       (signal (car err) (cdr err)))))
  name)

(defun cj/open-macros-file ()
  "Open the keyboard macros file."
  (interactive)
  ;; Ensure macros are loaded before opening the file
  (cj/ensure-macros-loaded)
  (ensure-macros-file macros-file)
  (find-file macros-file))

;; Set up key bindings and hooks
(defun keyboard-macros-setup ()
  "Set up keyboard macro key bindings and hooks."
  (keymap-global-set "C-<f3>" #'cj/kbd-macro-start-or-end)
  (keymap-global-set "<f3>"   #'call-last-kbd-macro)
  (keymap-global-set "M-<f3>" #'cj/save-maybe-edit-macro)
  (keymap-global-set "s-<f3>" #'cj/open-macros-file)
  (add-hook 'kill-emacs-hook #'cj/save-last-kbd-macro-on-exit))

;; Add hook to save any unnamed macros on exit if desired
(defun cj/save-last-kbd-macro-on-exit ()
  "Save the last keyboard macro before exiting Emacs if it's not saved."
  (when last-kbd-macro
    (when (y-or-n-p "Save last keyboard macro before exiting? ")
      (call-interactively #'cj/save-maybe-edit-macro))))

;; Auto-call setup after init
(if after-init-time
    ;; Init already completed, run setup now
    (keyboard-macros-setup)
  ;; Init not yet complete, defer until after init
  (add-hook 'after-init-hook #'keyboard-macros-setup))

(provide 'keyboard-macros)
;;; keyboard-macros.el ends here
