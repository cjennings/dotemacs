;;; pdf-config --- PDF Viewer Setup -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; --------------------------------- PDF Tools ---------------------------------

(use-package pdf-tools
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :init
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-midnight-colors '( "#F1D5AC" . "#0F0E06")) ;; fg . bg
  :config
  (pdf-tools-install :no-query) ;; automatically compile on first launch
  :bind
  (:map pdf-view-mode-map
		("M" . pdf-view-midnight-minor-mode)
		("m" . bookmark-set)
		("C-=" . pdf-view-enlarge)
		("C--" . pdf-view-shrink)
        ("z" . (lambda () (interactive) (cj/open-file-with-command "zathura")))
		("e" . (lambda () (interactive) (cj/open-file-with-command "evince")))
		("C-c l" . org-store-link)
        ("j" . pdf-view-next-line-or-next-page)
		("k" . pdf-view-previous-line-or-previous-page)))

(use-package pdf-view
  :ensure nil ;; built-in
  :after pdf-tools
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  ;; Avoid searching for unicodes to speed up pdf-tools.
  ;; ... and yes, 'ligther' is not a typo
  (pdf-view-use-unicode-ligther nil)
  ;; Enable HiDPI support, at the cost of memory.
  (pdf-view-use-scaling t))

;; ------------------------------ PDF View Restore -----------------------------

;; restores the last known position on opening a pdf file.
(use-package pdf-view-restore
  :after pdf-tools
  :defer 1
  :hook
  (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename (concat user-emacs-directory "/.pdf-view-restore")))

;; --------------------------- PDF Continuous Scroll ---------------------------

;; provides continuous scrolling of PDF documents in PDF View
(use-package pdf-continuous-scroll-mode
  :ensure nil ;; in custom folder
  :after pdf-tools
  :load-path "custom/pdf-continuous-scroll-mode-latest.el"
  :hook (pdf-view-mode . pdf-continuous-scroll-mode))

(provide 'pdf-config)
;;; pdf-config.el ends here
