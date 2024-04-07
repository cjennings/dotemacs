;;; flyspell-config.el --- Spell Check Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Spell-Checking: Flyspell
;; C-' is now my main interface for all spell checking. It checks one word, then
;; you may proceed to your next misspelling by selecting 'C-' again.

;; Flyspell will automatically run in a mode that's appropriate for the buffer type
;; - if it's a programming mode, it will only check comments
;; - if in text mode, it will check everything
;; - otherwise it will turn off.
;; This check happens on every mode switch.

;; If you want flyspell on in another mode (say fundamental mode), or you'd like it off
;; to keep it from distracting, you can toggle flyspell's state with 'C-c f'

;; The nicest thing is that each spell correction creates an abbrev. This essentially is a shortcut
;; that expands that same misspelling to the correct spelling the next time it's typed. That
;; idea comes courtesy Artur Malabarba, and it's increased my overall typing speed.
;;
;; Use M-o to get to 'other options', including saving to your personal dictionary.

;;; Code:

;; ------------------------ Reclaim Flyspell Keybinding ------------------------
;; remove one of flyspell's many greedy keyboard bindings
;; this is now used for my personal-keymap

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))

;; --------------------------------- Flyspell --------------------------------
;; there's some ispell settings here as well.

(use-package flyspell
  :defer .5
  :ensure nil ;; built-in
  :config
  (setq ispell-alternate-dictionary (concat user-emacs-directory "assets/english-words.txt"))
  (setq ispell-dictionary "american") ; better for aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "-W" "3" "--lang=en_US"))
  (setq ispell-list-command "--list")                                       ;; in aspell "-l" means --list, not --lang
  (setq ispell-local-dictionary "en_US")
  (setq ispell-program-name "aspell")                                       ;; use aspell instead of ispell
  (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
										 t ; Many other characters
										 ("-d" "en_US") nil utf-8)))

  (setq ispell-personal-dictionary
		(concat sync-dir "aspell-personal-dictionary"))                     ;; personal directory goes with sync'd files
  (setq flyspell-issue-message-flag nil)                                    ;; don't print message for every word when checking
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))) ;; skip code blocks in org mode


(use-package flyspell-correct
  :defer 1
  :after flyspell
  :bind
  (:map flyspell-mode-map
		("C-'" . cj/flyspell-then-abbrev)))                                 ;; disallow other entries to spelling corrections


(use-package flyspell-correct-ivy
  :defer 1
  :after (ivy flyspell-correct)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))                  ;; use ivy as the flyspell interface


;; ------------------------ Flyspell On For Buffer Type ------------------------
;; check strings and comments in prog mode; everything in text mode

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode and check the current buffer.
If flyspell is already enabled, do nothing.  If the mode is derived from
`prog-mode', enable `flyspell-prog-mode' so only strings and comments get
checked.  If the buffer is text based `flyspell-mode' is enabled to check
all text.  Otherwise, flyspell is turned off."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        (if (derived-mode-p 'prog-mode)
            (progn
              (flyspell-prog-mode)
              (flyspell-buffer)))
        (if (derived-mode-p 'text-mode)
            (progn
              (flyspell-mode 1)
              (flyspell-buffer))
          ;; else
          (progn
            (flyspell-mode 0))))))
(add-hook 'after-change-major-mode-hook 'flyspell-on-for-buffer-type)
(add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

;; ---------------------------- Flyspell Then Abbrev ---------------------------
;; A stroke of genius from Artur Malabarba.
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;; Spell checks backwards (after you've noticed a typo). Corrects, then automatically
;; creates an abbrev to autocorrect the same misspelling.
;; Bound to C-; above.

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(defun cj/flyspell-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (save-excursion
    (if (flyspell-goto-previous-word (point))
        (let ((bef (downcase (or (thing-at-point 'word)
                                 "")))
              aft)
          (call-interactively 'flyspell-correct-at-point)
          (setq aft (downcase
                     (or (thing-at-point 'word) "")))
          (unless (or (string= aft bef)
                      (string= aft "")
                      (string= bef ""))
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob"))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)))
      (message "Cannot find a misspelled word"))))

(defun flyspell-goto-previous-word (position)
  "Go to the first misspelled word that occurs before point.
But don't look beyond what's visible on the screen."
  (interactive "d")
  (let ((top (window-start))
        (bot (window-end)))
    (save-restriction
      (narrow-to-region top bot)
      (overlay-recenter (point))
      (add-hook 'pre-command-hook
                (function flyspell-auto-correct-previous-hook) t t)
      (unless flyspell-auto-correct-previous-pos
        ;; only reset if a new overlay exists
        (setq flyspell-auto-correct-previous-pos nil)
        (let ((overlay-list (overlays-in (point-min) position))
              (new-overlay 'dummy-value))
          ;; search for previous (new) flyspell overlay
          (while (and new-overlay
                      (or (not (flyspell-overlay-p new-overlay))
                          ;; check if its face has changed
                          (not (eq (get-char-property
                                    (overlay-start new-overlay) 'face)
                                   'flyspell-incorrect))))
            (setq new-overlay (car-safe overlay-list))
            (setq overlay-list (cdr-safe overlay-list)))
          ;; if nothing new exits new-overlay should be nil
          (if new-overlay ;; the length of the word may change so go to the start
              (setq flyspell-auto-correct-previous-pos
                    (overlay-start new-overlay)))))
      (if (not flyspell-auto-correct-previous-pos)
          nil
        (goto-char flyspell-auto-correct-previous-pos)
        t))))

;; ------------------------------ Flyspell Toggle ------------------------------
;; easy toggling flyspell which also leverages the 'for-buffer-type' functionality.

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.
When turning on,it uses `flyspell-on-for-buffer-type' so code-vs-text is
handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
    ;; else - flyspell is off, turn it on
    (progn
      (flyspell-on-for-buffer-type)
      (message "Flyspell on"))))
(global-set-key (kbd "C-c f") 'flyspell-toggle )


;; -------------------------------- Ispell STFU --------------------------------
;; tell ispell where to send it's spurious error messages

(advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
			  (shut-up (apply orig args))))

(provide 'flyspell-config)
;;; flyspell-config.el ends here.
