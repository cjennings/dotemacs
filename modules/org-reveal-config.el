;;; org-reveal-config.el --- Reveal.js Presentation Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; presentation export is a command-loaded deferral
;;   candidate for Phase 4.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; Integrates ox-reveal for creating reveal.js presentations from Org files.
;;
;; Fully offline workflow using a local reveal.js clone (managed by
;; scripts/setup-reveal.sh) and self-contained HTML export.
;;
;; Keybindings (C-; p prefix):
;; - C-; p SPC : Ensure headers, export, and open in browser
;; - C-; p e : Export to self-contained HTML and open in browser
;; - C-; p p : Start live preview (re-exports on save)
;; - C-; p s : Stop live preview
;; - C-; p h : Insert #+REVEAL_ header block at top of current buffer
;; - C-; p H : Remove reveal.js headers from current buffer
;; - C-; p n : Create new presentation file (prompts for title and location)

;;; Code:

;; Forward declarations for byte-compiler (ox-reveal loaded via use-package)
(defvar org-reveal-root)
(defvar org-reveal-single-file)
(defvar org-reveal-plugins)
(defvar org-reveal-highlight-css)
(defvar org-reveal-init-options)
(declare-function org-reveal-export-to-html "ox-reveal")

;; --------------------------------- Constants ---------------------------------

(defconst cj/reveal-root
  (expand-file-name "reveal.js" user-emacs-directory)
  "Local reveal.js installation directory.")

(defconst cj/reveal-default-theme "black"
  "Default reveal.js theme for new presentations.")

(defconst cj/reveal-default-transition "none"
  "Default reveal.js slide transition for new presentations.")

;; --------------------------------- ox-reveal ---------------------------------

(use-package ox-reveal
  :after ox
  :config
  (setq org-reveal-root (concat "file://" cj/reveal-root))
  (setq org-reveal-single-file t)
  (setq org-reveal-plugins '(highlight notes search zoom))
  (setq org-reveal-highlight-css "%r/plugin/highlight/monokai.css")
  (setq org-reveal-init-options (format "slideNumber:true, hash:true, transition:'%s'"
                                         cj/reveal-default-transition))
  (setq org-reveal-head-preamble
        "<style>.reveal h1,.reveal h2,.reveal h3,.reveal p{text-transform:none;}</style>"))

;; ----------------------------- Private Helpers -------------------------------

(defun cj/--reveal-header-template (title)
  "Return the reveal.js header block string for TITLE."
  (unless (stringp title)
    (user-error "Title must be a string"))
  (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: %s
#+REVEAL_ROOT: %s
#+REVEAL_THEME: %s
#+REVEAL_INIT_OPTIONS: slideNumber:true, hash:true, transition:'%s'
#+REVEAL_PLUGINS: (highlight notes search zoom)
#+REVEAL_HIGHLIGHT_CSS: %%r/plugin/highlight/monokai.css
#+OPTIONS: toc:nil num:nil date:nil timestamp:nil author:t

" title (user-full-name) (format-time-string "%Y-%m-%d")
    (concat "file://" cj/reveal-root)
    cj/reveal-default-theme
    cj/reveal-default-transition))

(defun cj/--reveal-title-to-filename (title)
  "Convert TITLE to a slug-based .org filename.
Downcases TITLE, replaces whitespace runs with hyphens, appends .org."
  (concat (replace-regexp-in-string "[[:space:]]+" "-" (downcase title))
          ".org"))

(defconst cj/--reveal-header-keywords
  '("TITLE"
    "AUTHOR"
    "DATE"
    "REVEAL_ROOT"
    "REVEAL_THEME"
    "REVEAL_INIT_OPTIONS"
    "REVEAL_PLUGINS"
    "REVEAL_HIGHLIGHT_CSS"
    "OPTIONS")
  "Org keywords inserted by `cj/--reveal-header-template'.")

(defun cj/--reveal-keyword-regexp (keyword)
  "Return a regexp matching an Org metadata line for KEYWORD."
  (format "^#\\+%s:[^\n]*\n?" (regexp-quote keyword)))

(defun cj/--reveal-has-header-p ()
  "Return non-nil when the current buffer already has reveal.js headers."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#\\+REVEAL_\\|^#\\+REVEAL_ROOT:" nil t)))

(defun cj/--reveal-remove-headers ()
  "Remove reveal.js header lines inserted by this module.
Returns the number of lines removed."
  (let ((removed 0))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (seq-some
                   (lambda (keyword)
                     (looking-at-p (cj/--reveal-keyword-regexp keyword)))
                   cj/--reveal-header-keywords))
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (setq removed (1+ removed)))
      (while (looking-at-p "\n")
        (delete-char 1)))
    removed))

(defun cj/--reveal-ensure-root ()
  "Signal a `user-error' when the local reveal.js checkout is missing.
Export needs the clone at `cj/reveal-root', installed by
scripts/setup-reveal.sh.  Without it the exporter produces a broken
presentation, so fail early with an actionable message."
  (unless (file-directory-p cj/reveal-root)
    (user-error "Local reveal.js not found at %s — run scripts/setup-reveal.sh"
                cj/reveal-root)))

(defun cj/--reveal-preview-export-on-save ()
  "Export current org buffer to reveal.js HTML silently.
Intended for use as a buffer-local `after-save-hook'."
  (when (derived-mode-p 'org-mode)
    (let ((inhibit-message t))
      (org-reveal-export-to-html))))

(defun cj/--reveal-ensure-header ()
  "Insert reveal.js headers when the current Org buffer does not have them."
  (unless (cj/--reveal-has-header-p)
    (let ((title (read-from-minibuffer "Presentation title: ")))
      (save-excursion
        (goto-char (point-min))
        (insert (cj/--reveal-header-template title)))
      t)))

;; ----------------------------- Public Functions ------------------------------

(defun cj/reveal-export ()
  "Export current Org buffer to self-contained reveal.js HTML and open in browser."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cj/--reveal-ensure-root)
  (let ((html-file (org-reveal-export-to-html)))
    (when html-file
      (browse-url-of-file html-file)
      (message "Opened presentation: %s" html-file))))

(defun cj/reveal-preview-start ()
  "Start live preview: re-export to HTML on every save.
Opens the presentation in a browser on first call.  Subsequent saves
re-export silently; refresh the browser to see changes."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cj/--reveal-ensure-root)
  (add-hook 'after-save-hook #'cj/--reveal-preview-export-on-save nil t)
  (let ((html-file (org-reveal-export-to-html)))
    (when html-file
      (browse-url-of-file html-file)))
  (message "Live preview started — save to re-export, refresh browser to update"))

(defun cj/reveal-preview-stop ()
  "Stop live preview by removing the after-save-hook."
  (interactive)
  (remove-hook 'after-save-hook #'cj/--reveal-preview-export-on-save t)
  (message "Live preview stopped"))

(defun cj/reveal-present ()
  "Ensure reveal headers, export current Org buffer, and open it in browser."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cj/--reveal-ensure-header)
  (when (buffer-modified-p)
    (if buffer-file-name
        (save-buffer)
      (call-interactively #'write-file)))
  (cj/reveal-export))

(defun cj/reveal-insert-header ()
  "Insert a #+REVEAL_ header block at the top of the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (when (cj/--reveal-has-header-p)
    (user-error "Reveal headers already present"))
  (cj/--reveal-ensure-header)
  (message "Inserted reveal.js headers"))

(defun cj/reveal-remove-headers ()
  "Remove reveal.js headers inserted by this module from the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((removed (cj/--reveal-remove-headers)))
    (message "Removed %d reveal.js header line%s"
             removed
             (if (= removed 1) "" "s"))))

(defun cj/reveal-new ()
  "Create a new reveal.js presentation file.
Prompts for a title and save location, then opens the file with
reveal.js headers pre-filled."
  (interactive)
  (let* ((title (read-from-minibuffer "Presentation title: "))
         (default-dir (expand-file-name "~/"))
         (file (read-file-name "Save presentation to: " default-dir nil nil
                               (cj/--reveal-title-to-filename title))))
    (when (file-exists-p file)
      (user-error "File already exists: %s" file))
    (find-file file)
    (insert (cj/--reveal-header-template title))
    (insert "* Slide 1\n\n")
    (save-buffer)
    (message "New presentation: %s" file)))

;; -------------------------------- Keybindings --------------------------------

(global-set-key (kbd "C-; p SPC") #'cj/reveal-present)
(global-set-key (kbd "C-; p e") #'cj/reveal-export)
(global-set-key (kbd "C-; p p") #'cj/reveal-preview-start)
(global-set-key (kbd "C-; p s") #'cj/reveal-preview-stop)
(global-set-key (kbd "C-; p h") #'cj/reveal-insert-header)
(global-set-key (kbd "C-; p H") #'cj/reveal-remove-headers)
(global-set-key (kbd "C-; p n") #'cj/reveal-new)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; p" "presentations"
    "C-; p SPC" "present current buffer"
    "C-; p e" "export & open"
    "C-; p p" "start live preview"
    "C-; p s" "stop live preview"
    "C-; p h" "insert headers"
    "C-; p H" "remove headers"
    "C-; p n" "new presentation"))

(provide 'org-reveal-config)
;;; org-reveal-config.el ends here
