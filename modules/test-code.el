;;; test-code.el --- test code -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; This is where you should put config code you want to test.
;; I recommend calling this file from the end of your init.el
;; if something breaks, you have most of your Emacs config loaded.

;;; Code:

;; ------------------------ Org-Branch To Org-Roam-Node ------------------------

;; (defun cj/move-org-branch-to-roam ()
;;   (interactive)
;;   (when (eq (org-element-type (org-element-at-point)) 'headline)
;; 	(let* ((headline-components (org-heading-components))
;; 		   (title (nth 4 headline-components)))
;; 	  (setq cj/point (point))
;; 	  (org-cut-subtree)
;; 	  ;; Switch to org-roam buffer, fill in new node's title
;; 	  ;; Pass the current headline title as the default value
;; 	  (org-roam-node-insert
;; 	   :immediate-finish t
;; 	   :no-edit t
;; 	   :region (cons (point)
;; 					 (save-excursion
;; 					   (insert title)
;; 					   (point))))
;; 	  (newline)
;; 	  ;; Paste the 'cut' subtree and save buffer
;; 	  (org-yank)
;; 	  (save-buffer)
;; 	  ;; Go back to the initial buffer and position
;; 	  (switch-to-buffer (other-buffer))
;; 	  (goto-char cj/point))))

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

;; (defun cj/move-org-branch-to-roam ()
;;   (interactive)
;;   (when (eq (org-element-type (org-element-at-point)) 'headline)
;; 	;; Save position to come back after branch cut
;; 	(setq cj/point (point))
;; 	;; Cut the whole subtree
;; 	(org-cut-subtree)
;; 	;; Switch to org-roam buffer, prompt for new node's title
;; 	(org-roam-node-insert)
;; 	(newline)
;; 	;; Paste the 'cut' subtree and save buffer
;; 	(org-yank)
;; 	(save-buffer)
;; 	;; Go back to the initial buffer and position
;; 	(switch-to-buffer (other-buffer))
;; 	(goto-char cj/point)))

;; --------------------------------- Org Noter ---------------------------------

(use-package org-noter
  :after (:any org pdf-view)
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

;; ------------------------------------ Pomm -----------------------------------

(use-package pomm
  :defer .5
  :bind ("M-p" . pomm)
  :commands (pomm pomm-third-time))

;; ----------------------------------- Mpdel -----------------------------------

;; (use-package mpdel
;;   :defer .5
;;   :config
;;   (setq mpdel-prefix-key (kbd "M-p"))
;;   (mpdel-mode))

;; --------------------------------- Easy Hugo ---------------------------------

(use-package easy-hugo
  :defer .5
  :init
  (setq easy-hugo-basedir "~/code/cjennings-net/")
  (setq easy-hugo-url "https://cjennings.net")
  (setq easy-hugo-sshdomain "cjennings.net")
  (setq easy-hugo-root "/var/www/cjennings/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-postdir "content")
  (setq easy-hugo-server-flags "-D --noHTTPCache --disableFastRender")
  (setq easy-hugo-default-ext ".md")
  :bind ("C-c H" . easy-hugo)
  :config
  (easy-hugo-enable-menu))

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

;; --------------------------------- Ob-Racket ---------------------------------

;; (use-package ob-racket
;;   :load-path "~/code/ob-racket"
;;   :defer .5
;;   :after racket-mode
;;   :commands (org-babel-execute:racket)
;;   :quelpa (ob-racket
;;       :fetcher github
;;       :repo "hasu/emacs-ob-racket"
;;       :files ("*.el" "*.rkt")))

(provide 'test-code)
;;; test-code.el ends here.
