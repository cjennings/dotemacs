;;; wip.el --- test code -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; This is where to put config code you're working on before it's tested and stable.
;; Include this at the very end of your init.el. This way, if something does break,
;; and it will, most of your Emacs config is loaded.

;; Once you've tested (and time-tested) the code here, graduate it into the proper
;; section of your config above.

;;; Code:

(require 'user-constants)

;; ----------------------------------- Efrit -----------------------------------
;; not working as of Wednesday, September 03, 2025 at 12:44:09 AM CDT

;; (add-to-list 'load-path "~/code/efrit/lisp")
;; (require 'efrit)

;; ------------------------------ Buffer Same Mode -----------------------------

(defun cj/buffer-same-mode (&rest modes)
  "Pop to a buffer with a mode among MODES, or the current one if not given."
  (interactive)
  (let* ((modes (or modes (list major-mode)))
         (pred (lambda (b)
                 (let ((b (get-buffer (if (consp b) (car b) b))))
                   (member (buffer-local-value 'major-mode b) modes)))))
    (pop-to-buffer (read-buffer "Buffer: " nil t pred))))
(global-set-key (kbd "C-x B") 'cj/buffer-same-mode)

;; --------------------------------- Org Noter ---------------------------------

(use-package djvu
  :defer 0.5)

(use-package org-noter
  :after (:any org pdf-view djvu)
  :commands org-noter
  :bind ("<f6>" . org-noter)
  :config
  (setq org-noter-always-create-frame nil)
  (setq org-noter-notes-window-location 'vertical-split)
  (setq org-noter-notes-window-behavior 'scroll)
  (setq org-noter-doc-split-fraction '(0.75 . 0.75))
  (setq org-noter-notes-search-path (concat sync-dir "/org-noter/"))
  (setq org-noter-default-notes-file-names '("reading-notes.org"))
  (setq org-noter-separate-notes-from-heading t)
  (org-noter-enable-org-roam-integration))

(use-package org-pdftools
  :after (org pdf-view)
  :hook (org-mode . org-pdftools-setup-link))

;; ;; --------------------------------- Easy Hugo ---------------------------------

;; (use-package easy-hugo
;;   :defer .5
;;   :init
;;   (setq easy-hugo-basedir "~/code/cjennings-net/")
;;   (setq easy-hugo-url "https://cjennings.net")
;;   (setq easy-hugo-sshdomain "cjennings.net")
;;   (setq easy-hugo-root "/var/www/cjennings/")
;;   (setq easy-hugo-previewtime "300")
;;   (setq easy-hugo-postdir "content")
;;   (setq easy-hugo-server-flags "-D --noHTTPCache --disableFastRender")
;;   (setq easy-hugo-default-ext ".md")
;;   :bind ("C-c H" . easy-hugo)
;;   :config
;;   (easy-hugo-enable-menu))

;; ------------------------------------ Pomm -----------------------------------

(use-package pomm
  :defer .5
  :bind ("M-p" . pomm)
  :commands (pomm pomm-third-time))

;; ------------------------ Org-Branch To Org-Roam-Node ------------------------

(defun cj/move-org-branch-to-roam ()
  (interactive)
  (when (eq (org-element-type (org-element-at-point)) 'headline)
    (let* ((headline-components (org-heading-components))
           (title (nth 4 headline-components)))
      (setq cj/point (point))
      (org-cut-subtree)
      ;; Switch to org-roam buffer, fill in new node's title.
      (org-roam-node-insert
       :immediate-finish t
       :no-edit t
       ;; Pass the current headline title as the default value.
       :region (cons (point)
                     (save-excursion
                       (insert title)
                       (point))))
      (newline)
      ;; Paste the 'cut' subtree and save buffer.
      (org-yank)
      (save-buffer)
      ;; Go back to the initial buffer and position.
      (switch-to-buffer (other-buffer))
      (goto-char cj/point)
      ;; Kill the org-roam link leftover.
      (kill-whole-line))))

;; ----------------------------------- Mpdel -----------------------------------

;; (use-package mpdel
;;   :defer .5
;;   :config
;;   (setq mpdel-prefix-key (kbd "M-p"))
;;   (mpdel-mode))

;; -------------------------------- Google This --------------------------------

;; not working as-is
;; (use-package google-this
;;   :load-path "~/code/emacs-google-this/"
;;   :defer 1
;;   :bind
;;   ("C-h g" . google-this-search)
;;   :config
;;   (google-this-mode 1)
;;   (setq google-this-browse-url-function 'eww-browse-url))

;; ------------------------------ Mouse Trap Mode ------------------------------

(defvar mouse-trap-mode-map
  (let* ((prefixes '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-")) ; modifiers
         (buttons  (number-sequence 1 5))                             ; mouse-1..5
         (types    '("mouse" "down-mouse" "drag-mouse"
                     "double-mouse" "triple-mouse"))
         (wheel    '("wheel-up" "wheel-down" "wheel-left" "wheel-right"))
         (map (make-sparse-keymap)))
    ;; clicks, drags, double, triple
    (dolist (type types)
      (dolist (pref prefixes)
        (dolist (n buttons)
          (define-key map (kbd (format "<%s%s-%d>" pref type n)) #'ignore))))
    ;; wheel
    (dolist (evt wheel)
      (dolist (pref prefixes)
        (define-key map (kbd (format "<%s%s>" pref evt)) #'ignore)))
    map)
  "Keymap for `mouse-trap-mode'. Unbinds almost every mouse event.
Disabling mouse prevents accidental mouse moves modifying text.")

(define-minor-mode mouse-trap-mode
  "Globally disable most mouse and trackpad events.
When active, <mouse-*>, <down-mouse-*>, <drag-mouse-*>,
<double-mouse-*>, <triple-mouse-*>, and wheel events are bound to `ignore',
with or without C-, M-, S- modifiers."
  :global t
  :lighter " 🐭"
  :keymap mouse-trap-mode-map)
(global-set-key (kbd "C-c M") #'mouse-trap-mode)
(mouse-trap-mode 1)

;; --------------------- Debug Code For Package Signatures ---------------------
;; from https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure


;; Set package-check-signature to nil, e.g., M-: (setq package-check-signature nil) RET.
;; Download the package gnu-elpa-keyring-update and run the function with the same name, e.g., M-x package-install RET gnu-elpa-keyring-update RET.
;; Reset package-check-signature to the default value allow-unsigned, e.g., M-: (setq package-check-signature 'allow-unsigned) RET.

;; (setq package-check-signature nil)
;; (setq package-check-signature 'allow-unsigned)

;; ----------------------------- Reset-Auth-Sources ----------------------------

(defun cj/reset-auth-cache ()
  "Clear Emacs auth-source cache."
  (interactive)
  (auth-source-forget-all-cached)
  (message "Emacs auth-source cache cleared."))

(provide 'wip)
;;; wip.el ends here.
