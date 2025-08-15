;;; keyboard-macros.el --- Keyboard Macro Management  -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; This library provides a simple, end-user–focused interface for
;; creating, naming, saving, and replaying keyboard macros in Emacs.
;; All commands are built on top of the built-in =kmacro= machinery, but
;; add a lightweight workflow and persistence across sessions.
;;
;; User Workflow:
;;
;; 1. Start recording with C-F3 (or M-x cj/kbd-macro-start-or-end)
;;    This toggles macro recording on.
;;    Now you can perform all the edits you want recorded in the macro.
;;
;; 2. Stop recording with C-F3 (or M-x cj/kbd-macro-start-or-end)
;;    This stops recording and the macro becomes the “last keyboard macro.”
;;
;; 3. Replay your macro <f3> (or M-x call-last-kbd-macro)
;;
;; 4. Name your macro with M-<F3>
;;    You will be prompted for a short name (e.g. =align-comments=,
;;    =cleanup-trail-spaces=).  This name is how you’ll refer to it later.
;;
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

(require 'user-constants) ;; definition of sync-dir constant is here.

(defvar macros-file     (concat sync-dir "macros.el")
  "The location of the macros file for recorded saved macros via M-f3.")

(defun ensure-macros-file (file)
  "Ensure FILE exists and its first line enables lexical-binding."
  (unless (file-exists-p file)
	(with-temp-file file
	  (insert ";;; -*- lexical-binding: t -*-\n"))))

(when (file-exists-p macros-file)
  (load macros-file))

(defun cj/kbd-macro-start-or-end ()
  "Toggle start/end of keyboard macro definition."
  (interactive)
  (if defining-kbd-macro
	  (end-kbd-macro)
	(start-kbd-macro nil)))
(global-set-key (kbd "C-<f3>") #'cj/kbd-macro-start-or-end)
(global-set-key (kbd "<f3>")     #'call-last-kbd-macro)

(defun cj/save-maybe-edit-macro (name)
  "Save last macro as NAME in `macros-file'; edit if prefix arg."
  (interactive "SName of macro: ")
  (kmacro-name-last-macro name)
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
(global-set-key (kbd "M-<f3>") #'cj/save-maybe-edit-macro)

(global-set-key (kbd "s-<f3>") (lambda () (interactive) (find-file macros-file)))

(defun ensure-macros-file (file)
  "Ensure FILE exists and its first line enables lexical-binding."
  (unless (file-exists-p file)
	(with-temp-file file
	  (insert ";;; -*- lexical-binding: t -*-\n"))))

(when (file-exists-p macros-file)
  (load macros-file))

(provide 'keyboard-macros)
;;; keyboard-macros.el ends here.


;; --------------------------------- ERT Tests ---------------------------------
;; Run these tests with M-x ert RET t RET

(require 'ert)

(ert-deftest keyboard-macros/ensure-macros-file-creates-header ()
  "ensure-macros-file creates FILE with the right header."
  (let ((file (make-temp-file "macros-test" nil ".el")))
	(unwind-protect
		(progn
		  (delete-file file)
		  (should-not (file-exists-p file))
		  (ensure-macros-file file)
		  (should (file-exists-p file))
		  (let ((contents (with-temp-buffer
							(insert-file-contents file)
							(buffer-string))))
			(should (string= contents ";;; -*- lexical-binding: t -*-\n"))))
	  (when (file-exists-p file)
		(delete-file file)))))
