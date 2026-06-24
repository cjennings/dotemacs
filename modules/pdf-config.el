;;; pdf-config --- PDF Viewer Setup -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; heavy PDF packages should load on PDF open, a file/mode
;;   deferral candidate.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;;; Code:

;; ------------------------------- Declarations --------------------------------

(declare-function pdf-tools-install "pdf-tools")
(declare-function pdf-view-midnight-minor-mode "pdf-view")
(declare-function pdf-view-enlarge "pdf-view")
(declare-function pdf-view-shrink "pdf-view")
(declare-function pdf-view-next-page "pdf-view")
(declare-function pdf-view-previous-page "pdf-view")
(declare-function image-next-line "image-mode")
(declare-function image-previous-line "image-mode")
(declare-function image-bob "image-mode")
(declare-function image-eob "image-mode")
(declare-function org-store-link "ol")
(declare-function cj/open-file-with-command "system-utils")
(declare-function cj/org-noter-insert-note-dwim "org-noter-config")

;; --------------------------------- PDF Tools ---------------------------------

(use-package pdf-tools
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  (pdf-view-midnight-colors '("#F1D5AC" . "#0F0E06")) ;; fg . bg
  ;; Avoid searching for unicodes to speed up pdf-tools.
  ;; ... and yes, 'ligther' is not a typo
  (pdf-view-use-unicode-ligther nil)
  ;; Enable HiDPI support, at the cost of memory.
  (pdf-view-use-scaling t)
  )

;; Keybindings via eval-after-load on 'pdf-view (not 'pdf-tools), because
;; opening a PDF loads pdf-view.el which provides 'pdf-view — it never
;; loads pdf-tools.el, so use-package :config for pdf-tools won't run.
;; pdf-tools-install must run when pdf-view loads (not in use-package :config
;; for pdf-tools, which never triggers — see comment above).  It starts the
;; epdfinfo rendering server and is a no-op when already set up.
(with-eval-after-load 'pdf-view
  (pdf-tools-install :no-query)
  ;; Revert any PDF buffers that opened before the server was ready,
  ;; so they re-render instead of showing raw binary.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'pdf-view-mode)
        (revert-buffer nil t))))
  (define-key pdf-view-mode-map "M" #'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map "m" #'bookmark-set)
  (define-key pdf-view-mode-map (kbd "C-=") #'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "C--") #'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "C-c l") #'org-store-link)
  (define-key pdf-view-mode-map "z" (lambda () (interactive) (cj/open-file-with-command "zathura")))
  ;; Arrow keys / j,k: scroll within page only (no page change)
  (define-key pdf-view-mode-map "j" #'image-next-line)
  (define-key pdf-view-mode-map "k" #'image-previous-line)
  (define-key pdf-view-mode-map (kbd "<down>") #'image-next-line)
  (define-key pdf-view-mode-map (kbd "<up>") #'image-previous-line)
  ;; Org-noter: start session if needed, then insert note
  (define-key pdf-view-mode-map "i" #'cj/org-noter-insert-note-dwim)
  ;; Page change: C-up/C-down go to top of prev/next page
  (define-key pdf-view-mode-map (kbd "C-<down>")
              (lambda () (interactive) (pdf-view-next-page) (image-bob)))
  (define-key pdf-view-mode-map (kbd "C-<up>")
              (lambda () (interactive) (pdf-view-previous-page) (image-eob))))

;; ------------------------------ PDF View Restore -----------------------------

;; restores the last known position on opening a pdf file.
(use-package pdf-view-restore
  :after pdf-tools
  :defer 1
  :hook
  (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename (expand-file-name "persist/pdf-view-restore" user-emacs-directory)))

(provide 'pdf-config)
;;; pdf-config.el ends here.
