;;; pdf-config.el --- PDF Viewer Setup -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; heavy PDF packages should load on PDF open, a file/mode
;;   deferral candidate.
;; Top-level side effects: a defgroup, defcustoms, a defvar-local, and package
;;   configuration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; A reading-view palette layer sits on top of pdf-tools: a fresh PDF opens in
;; the "dark" tint, and `c' cycles dark -> sepia -> light -> none, mirroring the
;; nov-mode reading view (see nov-reading.el).  Each palette is a (FG . BG) cons
;; driven through `pdf-view-midnight-minor-mode', which recolors the rendered
;; page as a duotone -- so "sepia" and "light" are the same mechanism as the
;; built-in midnight mode, just with warmer / lighter color pairs.
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

;; `pdf-view-midnight-colors' lives in pdf-view.el, which loads lazily on the
;; first PDF open.  Declare it special so the `setq-local' below compiles as a
;; dynamic binding rather than a lexical no-op.
(defvar pdf-view-midnight-colors)

;; ------------------------------ Reading palettes -----------------------------
;; pdf-view has no sepia/light of its own -- only a binary midnight toggle.  This
;; layer generalizes that toggle into a named palette cycle: `pdf-view-midnight-
;; minor-mode' recolors the page from a (FG . BG) pair, so any tint is just a
;; different pair.  Colors mirror the nov-reading palettes for a consistent read
;; across EPUBs and PDFs; tune them there and here together.

(defgroup cj/pdf-reading nil
  "Reading-view theming for pdf-view PDFs."
  :group 'cj)

(defcustom cj/pdf-reading-palettes
  '(("dark"  . ("#cfc8b8" . "#15140f"))
    ("sepia" . ("#c9b187" . "#1f1b16"))
    ("light" . ("#2a2622" . "#ece3cf")))
  "Alist of reading-palette NAME -> (FOREGROUND . BACKGROUND) for pdf-view.
Each value is the color pair `pdf-view-midnight-minor-mode' renders the page
with (white maps to BACKGROUND, black to FOREGROUND).  The selector and cycle
commands choose among these names; add an entry to add a palette."
  :type '(alist :key-type string
                :value-type (cons (string :tag "Foreground")
                                  (string :tag "Background")))
  :group 'cj/pdf-reading)

(defcustom cj/pdf-reading-default-palette "dark"
  "Reading palette applied to a freshly opened PDF.
A key in `cj/pdf-reading-palettes', or nil for the PDF's native colors."
  :type '(choice (const :tag "None (native colors)" nil) string)
  :group 'cj/pdf-reading)

(defvar-local cj/pdf--reading-palette nil
  "Name of the reading palette active in this pdf buffer, or nil for none.")

(defun cj/pdf--reading-palette-colors (name)
  "Return the (FOREGROUND . BACKGROUND) cons for palette NAME, or nil when unknown.
NAME nil (the no-palette state) and unknown names both yield nil."
  (cdr (assoc name cj/pdf-reading-palettes)))

(defun cj/pdf--next-reading-palette (current names)
  "Return the palette after CURRENT in the cycle NAMES then nil, wrapping.
CURRENT nil is the no-palette state, and a returned nil means no palette.  An
unknown CURRENT falls back to the first palette."
  (let* ((cycle (append names (list nil)))
         (tail (cdr (member current cycle))))
    (car (or tail cycle))))

(defun cj/pdf--apply-reading-palette (name)
  "Apply reading palette NAME to this pdf buffer; NAME nil removes any palette.
Drives `pdf-view-midnight-minor-mode' with the palette's (FG . BG) colors so the
page renders as a duotone in the reading tint.  An unknown or nil NAME turns
midnight mode off, restoring the PDF's native colors."
  (let ((colors (cj/pdf--reading-palette-colors name)))
    (if colors
        (progn
          (setq-local pdf-view-midnight-colors colors)
          (pdf-view-midnight-minor-mode 1)
          (setq cj/pdf--reading-palette name))
      (pdf-view-midnight-minor-mode -1)
      (setq cj/pdf--reading-palette nil))))

(defun cj/pdf-set-reading-palette (name)
  "Choose reading palette NAME for this pdf buffer; \"none\" clears it.
Interactively prompts among `cj/pdf-reading-palettes' plus \"none\"."
  (interactive
   (list (completing-read "Reading palette: "
                          (cons "none" (mapcar #'car cj/pdf-reading-palettes))
                          nil t)))
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a pdf-view-mode buffer"))
  (cj/pdf--apply-reading-palette (unless (equal name "none") name))
  (message "Reading palette: %s" (or cj/pdf--reading-palette "none")))

(defun cj/pdf-cycle-reading-palette ()
  "Cycle to the next reading palette, then the no-palette state, wrapping."
  (interactive)
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a pdf-view-mode buffer"))
  (let ((next (cj/pdf--next-reading-palette
               cj/pdf--reading-palette
               (mapcar #'car cj/pdf-reading-palettes))))
    (cj/pdf--apply-reading-palette next)
    (message "Reading palette: %s" (or next "none"))))

(defun cj/pdf-reading-setup ()
  "Apply the default reading palette to a freshly opened PDF.
Called from the `pdf-view-mode' launch hook."
  (when cj/pdf-reading-default-palette
    (cj/pdf--apply-reading-palette cj/pdf-reading-default-palette)))

;; --------------------------------- PDF Tools ---------------------------------

(use-package pdf-tools
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook
  (pdf-view-mode . cj/pdf-reading-setup)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
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
  ;; Reading palette: c cycles dark -> sepia -> light -> none, C selects by name.
  ;; M keeps the old midnight key working, now as the cycle.
  (define-key pdf-view-mode-map "c" #'cj/pdf-cycle-reading-palette)
  (define-key pdf-view-mode-map "C" #'cj/pdf-set-reading-palette)
  (define-key pdf-view-mode-map "M" #'cj/pdf-cycle-reading-palette)
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
