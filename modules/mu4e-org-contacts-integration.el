;;; mu4e-org-contacts-integration.el --- Integrate org-contacts with mu4e completion -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; This module provides seamless integration between org-contacts and mu4e's
;; email composition, enabling automatic contact completion in email fields.

;;; Code:

(require 'mu4e)
(require 'org-contacts)

;; ---------------------- Completion at Point Function -------------------------

(defun cj/org-contacts-completion-at-point ()
  "Provide completion-at-point function for org-contacts emails.
This function is designed to work with mu4e's compose buffers."
  (when (and (derived-mode-p 'mu4e-compose-mode 'org-msg-edit-mode)
             (mail-abbrev-in-expansion-header-p))
    (let* ((end (point))
           (start (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*" nil t)
                    (goto-char (match-end 0))
                    (point)))
           (initial (buffer-substring-no-properties start end))
           (contacts (cj/get-all-contact-emails)))
      (when contacts
        (list start end
              (completion-table-dynamic
               (lambda (_)
                 contacts))
              :exclusive 'no  ; Allow other completion sources
              :annotation-function
              (lambda (s)
                (when-let* ((email-match (string-match "<\\([^>]+\\)>" s))
                            (email (match-string 1 s)))
                  (propertize (format " [%s]" (file-name-nondirectory contacts-file))
                              'face 'completions-annotations))))))))

;; ---------------------- Smart TAB Completion -------------------------

(defun cj/mu4e-org-contacts-tab-complete ()
  "Smart TAB completion for mu4e compose buffers.
In email header fields (To, Cc, Bcc), complete using org-contacts.
Elsewhere, perform the default TAB action."
  (interactive)
  (cond
   ;; In email header fields, use completion-at-point
   ((mail-abbrev-in-expansion-header-p)
    (if (and (boundp 'completion-in-region-mode) completion-in-region-mode)
        ;; If we're already in completion mode, cycle through candidates
        (completion-at-point)
      ;; Start new completion
      (completion-at-point)))
   ;; In org-msg-edit-mode body, use org-cycle
   ((and (eq major-mode 'org-msg-edit-mode)
         (not (mail-abbrev-in-expansion-header-p)))
    (org-cycle))
   ;; Default: indent
   (t (indent-for-tab-command))))

;; ---------------------- Comma-triggered Completion -------------------------

(defun cj/mu4e-org-contacts-comma-complete ()
  "Insert comma and optionally trigger contact completion.
In email header fields, insert comma with space and offer completion.
Elsewhere, just insert comma."
  (interactive)
  (if (mail-abbrev-in-expansion-header-p)
      (progn
        (insert ", ")  ; Insert comma with space
        ;; Trigger completion immediately if there's no text after the comma
        (when (looking-at-p "\\s-*$")
          (completion-at-point)))
    (insert ",")))

;; ---------------------- Direct Insertion (Alternative) -------------------------

(defun cj/mu4e-org-contacts-insert-email ()
  "Directly insert a contact email using completing-read.
This bypasses the completion-at-point system for direct selection."
  (interactive)
  (when (mail-abbrev-in-expansion-header-p)
    (let* ((contacts (cj/get-all-contact-emails))
           (selected (completing-read "Contact: " contacts nil t)))
      ;; If we're not at the beginning of a field, check if we need a comma
      (when (and (not (save-excursion
                        (skip-chars-backward " \t")
                        (or (bolp) (looking-back "[:,]" 1))))
                 (not (looking-at-p "\\s-*$")))
        (insert ", "))
      (insert selected))))

;; ---------------------- Setup Functions -------------------------

(defun cj/mu4e-org-contacts-setup-completion ()
  "Setup org-contacts completion for mu4e compose buffers."
  ;; Add our completion function with high priority
  (add-hook 'completion-at-point-functions
            #'cj/org-contacts-completion-at-point
            -10 t)  ; High priority, buffer-local
  
  ;; Setup completion behavior
  (setq-local completion-ignore-case t)
  (setq-local completion-cycle-threshold 7)
  
  ;; Add substring matching if not already present
  (unless (member 'substring completion-styles)
    (setq-local completion-styles
                (append '(substring) completion-styles))))

(defun cj/mu4e-org-contacts-setup-keybindings ()
  "Setup keybindings for org-contacts completion in compose buffers."
  ;; TAB for smart completion
  (local-set-key (kbd "TAB") #'cj/mu4e-org-contacts-tab-complete)
  (local-set-key (kbd "<tab>") #'cj/mu4e-org-contacts-tab-complete)
  
  ;; Comma for comma-triggered completion
  (local-set-key (kbd ",") #'cj/mu4e-org-contacts-comma-complete)
  
  ;; Optional: Direct insertion binding
  (local-set-key (kbd "C-c e") #'cj/mu4e-org-contacts-insert-email))

;; ---------------------- Mode Hooks -------------------------

(defun cj/mu4e-org-contacts-compose-setup ()
  "Setup function to be called in mu4e compose mode hooks."
  (cj/mu4e-org-contacts-setup-completion)
  (cj/mu4e-org-contacts-setup-keybindings))

;; ---------------------- Activation -------------------------

(defun cj/activate-mu4e-org-contacts-integration ()
  "Activate org-contacts integration with mu4e email composition."
  (interactive)
  
  ;; Ensure mu4e's built-in completion is disabled
  (setq mu4e-compose-complete-addresses nil)
  
  ;; Setup hooks for mu4e-compose-mode
  (add-hook 'mu4e-compose-mode-hook #'cj/mu4e-org-contacts-compose-setup)
  
  ;; Setup hooks for org-msg-edit-mode (HTML email composition)
  (with-eval-after-load 'org-msg
    (add-hook 'org-msg-edit-mode-hook #'cj/mu4e-org-contacts-compose-setup))
  
  ;; Remove any existing mu4e completion setup
  (remove-hook 'mu4e-compose-mode-hook #'mu4e--compose-setup-completion)
  
  (message "mu4e org-contacts integration activated"))

(defun cj/deactivate-mu4e-org-contacts-integration ()
  "Deactivate org-contacts integration with mu4e email composition."
  (interactive)
  
  ;; Remove our hooks
  (remove-hook 'mu4e-compose-mode-hook #'cj/mu4e-org-contacts-compose-setup)
  (remove-hook 'org-msg-edit-mode-hook #'cj/mu4e-org-contacts-compose-setup)
  
  ;; Re-enable mu4e's built-in completion if desired
  (setq mu4e-compose-complete-addresses t)
  (add-hook 'mu4e-compose-mode-hook #'mu4e--compose-setup-completion)
  
  (message "mu4e org-contacts integration deactivated"))

(provide 'mu4e-org-contacts-integration)
;;; mu4e-org-contacts-integration.el ends here