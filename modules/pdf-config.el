;;; pdf-config --- PDF Viewer Setup -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; --------------------------------- PDF Tools ---------------------------------

(use-package pdf-tools
  :defer 1
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :init
  (setq pdf-view-display-size 'fit-page)
  (setq pdf-view-midnight-colors '( "#F1D5AC" . "#0F0E06")) ;; fg . bg
  :config
  (pdf-tools-install :no-query) ;; automatically compile on first launch
  :bind
  (:map pdf-view-mode-map
        ("M" . pdf-view-midnight-minor-mode)
        ("m" . cj/bookmark-set-and-save)
        ("z" . (lambda () (interactive) (cj/open-file-with-command "zathura")))
        ("e" . (lambda () (interactive) (cj/open-file-with-command "evince")))
        ("j" . pdf-view-next-line-or-next-page)
		("k" . pdf-view-previous-line-or-previous-page)))

(use-package pdf-view
  :ensure nil ;; built-in
  :after pdf-tools
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

(provide 'pdf-config)
;;; pdf-config.el ends here
