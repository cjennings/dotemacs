;;; wip.el --- test code -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Work-in-progress and experimental code testing area. This file contains
;; code that is being actively developed or tested before being promoted
;; to stable configuration modules. Functions here may be incomplete,
;; buggy, or subject to significant changes. Include this file at the end
;; of init.el so that if something breaks, most of the Emacs config has
;; already loaded. Once code has been tested and proven stable, graduate
;; it into the appropriate configuration module. Do not rely on this code
;; for production use.
;;
;;; Code:

;; ----------------------------------- Efrit -----------------------------------
;; not working as of Wednesday, September 03, 2025 at 12:44:09 AM CDT

;; (add-to-list 'load-path "~/code/efrit/lisp")
;; (require 'efrit)

;; ------------------------------ Buffer Same Mode -----------------------------

;; (defun cj/buffer-same-mode (&rest modes)
;;   "Pop to a buffer with a mode among MODES, or the current one if not given."
;;   (interactive)
;;   (let* ((modes (or modes (list major-mode)))
;;          (pred (lambda (b)
;;                  (let ((b (get-buffer (if (consp b) (car b) b))))
;;                    (member (buffer-local-value 'major-mode b) modes)))))
;;     (pop-to-buffer (read-buffer "Buffer: " nil t pred))))
;; (keymap-global-set "C-x B" #'cj/buffer-same-mode)

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

(provide 'wip)
;;; wip.el ends here.
