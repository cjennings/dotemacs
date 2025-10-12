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
;; 7. All macros reload at startup automatically.
;;    When this library is loaded, it will look for the save file and
;;    re-establish all your named macros in your current session.
;;
;;; Code:

(require 'user-constants) ;; definitions of sync-dir and macros-file

(defun ensure-macros-file (file)
  "Ensure FILE exists and its first line enables lexical-binding."
  (unless (file-exists-p file)
	(with-temp-file file
	  (insert ";;; -*- lexical-binding: t -*-\n"))))

(defun cj/kbd-macro-start-or-end ()
  "Toggle start/end of keyboard macro definition."
  (interactive)
  (if defining-kbd-macro
	  (end-kbd-macro)
	(start-kbd-macro nil)))

(defun cj/save-maybe-edit-macro (name)
  "Save last macro as NAME in `macros-file'; edit if prefix arg."
  (interactive "SName of macro: ")
  (kmacro-name-last-macro name)
  (ensure-macros-file macros-file)
  (find-file macros-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (save-buffer)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  (when current-prefix-arg
	(find-file macros-file)
	(goto-char (point-max)))
  name)

(defun cj/open-macros-file ()
  "Open the keyboard macros file."
  (interactive)
  (ensure-macros-file macros-file)
  (find-file macros-file))

;; Set up key bindings
(global-set-key (kbd "C-<f3>") #'cj/kbd-macro-start-or-end)
(global-set-key (kbd "<f3>")   #'call-last-kbd-macro)
(global-set-key (kbd "M-<f3>") #'cj/save-maybe-edit-macro)
(global-set-key (kbd "s-<f3>") #'cj/open-macros-file)

;; Add hook to save any unnamed macros on exit if desired
(defun cj/save-last-kbd-macro-on-exit ()
  "Save the last keyboard macro before exiting Emacs if it's not saved."
  (when (and last-kbd-macro (not (kmacro-name-last-macro)))
	(when (y-or-n-p "Save last keyboard macro before exiting? ")
	  (call-interactively #'cj/save-maybe-edit-macro))))

(add-hook 'kill-emacs-hook #'cj/save-last-kbd-macro-on-exit)

;; Load existing macros file with proper error handling
(when (file-exists-p macros-file)
  (condition-case err
	  (load macros-file)
	(error
	 (message "Error loading keyboard macros file: %s" (error-message-string err)))))

(provide 'keyboard-macros)
;;; keyboard-macros.el ends here
