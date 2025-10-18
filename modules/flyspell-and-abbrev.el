;;; flyspell-and-abbrev.el --- Spell Check Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; WORKFLOW:
;; C-' is now my main interface for all spell checking.
;;
;; The workflow is that it finds the nearest misspelled word above where the
;; cursor is, allows for saving or correcting, then stops. You may proceed to
;; the next misspelling by selecting C-' again.
;;
;; Use M-o to get to 'other options', like saving to your personal dictionary.
;;
;; Flyspell will automatically run in a mode appropriate for the buffer type
;; - if it's a programming mode, it will only check comments
;; - if in text mode, it will check everything
;; - otherwise it will turn off.
;; This check happens on every mode switch.
;;
;; If you want flyspell on in another mode (say fundamental mode), or you want
;; to turn it off, you can toggle flyspell's state with 'C-c f'
;;
;; The nicest thing is that each spell correction creates an abbrev. This
;; essentially is a shortcut that expands that same misspelling to the correct
;; spelling the next time it's typed. That idea comes courtesy Artur Malabarba,
;; and it's increased my overall typing speed.
;;
;; Original idea here:
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;;
;; The code below is my refactoring of Artur Malabarba's code, and using
;; flyspell rather than ispell.
;;
;; NOTES:
;;
;; FYI, the keybinding typically taken for the flyspell-mode-map "C-;" has
;; been deliberately hijacked in custom-functions.el for my personal-keymap.
;; This is the code run there:

;; (eval-after-load "flyspell"
;;   '(define-key flyspell-mode-map (kbd "C-;") nil))

;;; Code:

;; ----------------------------------- Abbrev ----------------------------------

(use-package abbrev-mode
  :ensure nil
  :defer 0.5
  :custom
  (abbrev-file-name (concat user-emacs-directory "assets/abbrev_defs"))
  :config
  (abbrev-mode 1))

;; ---------------------------- Ispell And Flyspell ----------------------------

(use-package ispell
  :defer .5
  :ensure nil ;; built-in
  :config
  ;; (setopt ispell-alternate-dictionary
  ;;        (concat user-emacs-directory "assets/english-words.txt"))
  (setopt text-mode-ispell-word-completion nil)
  (setopt ispell-alternate-dictionary nil)

  (setq ispell-dictionary "american") ; better for aspell
  ;; use aspell rather than ispell
  (setq ispell-program-name "aspell")
  ;; aspell is in /usr/local/ on BSD
  (cond ((eq system-type 'berkeley-unix)
		 (setq ispell-program-name "/usr/local/bin/aspell")))

  ;; in aspell "-l" means --list, not --lang
  (setq ispell-list-command "--list")
  (setq ispell-extra-args '("--sug-mode=ultra" "-W" "3" "--lang=en_US"))
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
		'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
		   t ;; Many other characters
		   ("-d" "en_US") nil utf-8)))
  ;; personal directory goes with sync'd files
  (setq ispell-personal-dictionary
		(concat org-dir "aspell-personal-dictionary"))
  ;; skip code blocks in org mode
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(use-package flyspell
  :after (ispell abbrev)
  :ensure nil ;; built-in
  :config
  ;; unset keybinding as we're using it for cj/custom keymap
  (keymap-unset flyspell-mode-map "C-;")
  ;; don't print message for every word when checking
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :after flyspell
  :defer .5)

;; ------------------------------ Flyspell Toggle ------------------------------
;; easy toggling flyspell and also leverage the 'for-buffer-type' functionality.

;; (defun flyspell-toggle ()
;;   "Turn Flyspell on if it is off, or off if it is on.

;; When turning on,it uses `flyspell-on-for-buffer-type' so code-vs-text is
;; handled appropriately."
;;   (interactive)
;;   (if (symbol-value flyspell-mode)
;;       (progn ; flyspell is on, turn it off
;;         (message "Flyspell off")
;;         (flyspell-mode -1))
;;     ;; else - flyspell is off, turn it on
;;     (progn
;;       (flyspell-on-for-buffer-type)
;;       (message "Flyspell on"))))
;; (define-key global-map (kbd "C-c f") 'flyspell-toggle )

;; ------------------------ Flyspell On For Buffer Type ------------------------
;; check strings and comments in prog mode; check everything in text mode

;; (defun flyspell-on-for-buffer-type ()
;;   "Enable Flyspell for the major mode and check the current buffer.

;; If flyspell is already enabled, do nothing.  If the mode is derived from
;; `prog-mode', enable `flyspell-prog-mode' so only strings and comments get
;; checked.  If the buffer is text based `flyspell-mode' is enabled to check
;; all text."
;;   (interactive)
;;   (unless flyspell-mode ; if not already on
;;  (cond
;;   ((derived-mode-p 'prog-mode)
;;    (flyspell-prog-mode)
;;    (flyspell-buffer)
;;    ((derived-mode-p 'text-mode)
;;     (flyspell-mode 1)
;;     (flyspell-buffer))))))

;; (add-hook 'after-change-major-mode-hook 'flyspell-on-for-buffer-type)
;; (add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

;; ---------------------------- Flyspell Then Abbrev ---------------------------
;; Spell check the buffer and create abbrevs to avoid future misspellings.

(setq-default abbrev-mode t)

(defun cj/find-previous-flyspell-overlay (position)
  "Locate the Flyspell overlay immediately previous to a given POSITION."
  ;; sort the overlays into position order
  (let ((overlay-list (sort (overlays-in (point-min) position)
							(lambda (a b)
							  (> (overlay-start a) (overlay-start b))))))
    ;; search for previous flyspell overlay
	(while (and overlay-list
				(or (not (flyspell-overlay-p (car overlay-list)))
					;; check if its face has changed
					(not (eq (get-char-property
							  (overlay-start (car overlay-list)) 'face)
							 'flyspell-incorrect))))
	  (setq overlay-list (cdr overlay-list)))
    ;; if no previous overlay exists, return nil
    (when overlay-list
      ;; otherwise, return the overlay start position
      (overlay-start (car overlay-list)))))


(defun cj/flyspell-goto-previous-misspelling (position)
  "Go to the first misspelled word before the given POSITION.
Return the misspelled word if found or nil if not. Leave the point at the
beginning of the misspelled word. Setting the hook on pre-command ensures that
any started Flyspell corrections complete before running other commands in the
buffer."
  (interactive "d")
  (add-hook 'pre-command-hook
            (function flyspell-auto-correct-previous-hook) t t)
  (let* ((overlay-position (cj/find-previous-flyspell-overlay position))
         (misspelled-word (when overlay-position
                            (goto-char overlay-position)
                            (thing-at-point 'word))))
    (if misspelled-word
        (downcase misspelled-word)
      nil)))

(defun cj/flyspell-then-abbrev (p)
  "Call \='flyspell-correct-at-point\=' and create abbrev for future corrections.
The abbrev is created in the local dictionary unless the prefix P
argument is provided, when it's created in the global dictionary."
  (interactive "P")
  (unless (featurep 'files)
	(require 'files))
  (setq save-abbrevs 'silently)
  (flyspell-buffer)
  (save-excursion
	(let (misspelled-word corrected-word)
	  (while (setq misspelled-word
				   (cj/flyspell-goto-previous-misspelling (point)))
		(call-interactively 'flyspell-correct-at-point)
		(setq corrected-word (downcase (or (thing-at-point 'word) "")))
		(when (and misspelled-word corrected-word
				   (not (string= corrected-word misspelled-word)))
		  (message "\"%s\" now expands to \"%s\" %sally"
				   misspelled-word corrected-word (if p "loc" "glob"))
		  (define-abbrev
			(if p local-abbrev-table global-abbrev-table)
			misspelled-word corrected-word))
		(goto-char (point-min))))
	(message "Spell check complete.")))

(define-key global-map (kbd "C-'") 'cj/flyspell-then-abbrev)

(provide 'flyspell-and-abbrev)
;;; flyspell-and-abbrev.el ends here.
