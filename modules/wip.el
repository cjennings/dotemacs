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


;; --------------------------- Org Upcoming Modeline ---------------------------

;; (use-package org-upcoming-modeline
;;   :after org
;;   :load-path "~/code/org-upcoming-modeline/org-upcoming-modeline.el"
;;   :config
;;   (setq org-upcoming-modeline-keep-late 300)
;;   (setq org-upcoming-modeline-ignored-keywords '("DONE" "CANCELLED" "FAILED"))
;;   (setq org-upcoming-modeline-trim 30)
;;   (setq org-upcoming-modeline-days-ahead 5)
;;   (setq org-upcoming-modeline-format (lambda (ms mh) (format "📅 %s %s" ms mh)))
;;   (org-upcoming-modeline-mode))

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

;; --------------------- Debug Code For Package Signatures ---------------------
;; from https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure


;; Set package-check-signature to nil, e.g., M-: (setq package-check-signature nil) RET.
;; Download the package gnu-elpa-keyring-update and run the function with the same name, e.g., M-x package-install RET gnu-elpa-keyring-update RET.
;; Reset package-check-signature to the default value allow-unsigned, e.g., M-: (setq package-check-signature 'allow-unsigned) RET.

;; (setq package-check-signature nil)
;; (setq package-check-signature 'allow-unsigned)


(provide 'wip)
;;; wip.el ends here.
