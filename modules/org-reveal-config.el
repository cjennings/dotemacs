;;; org-reveal-config.el --- Reveal.js Presentation Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integrates ox-reveal for creating reveal.js presentations from Org files.
;;
;; Fully offline workflow using a local reveal.js clone (managed by
;; scripts/setup-reveal.sh) and self-contained HTML export.
;;
;; Keybindings (C-; p prefix):
;; - C-; p e : Export to self-contained HTML and open in browser
;; - C-; p p : Start live preview (re-exports on save)
;; - C-; p s : Stop live preview
;; - C-; p h : Insert #+REVEAL_ header block at top of current buffer
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

(defun cj/--reveal-preview-export-on-save ()
  "Export current org buffer to reveal.js HTML silently.
Intended for use as a buffer-local `after-save-hook'."
  (when (derived-mode-p 'org-mode)
    (let ((inhibit-message t))
      (org-reveal-export-to-html))))

;; ----------------------------- Public Functions ------------------------------

(defun cj/reveal-export ()
  "Export current Org buffer to self-contained reveal.js HTML and open in browser."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
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

(defun cj/reveal-insert-header ()
  "Insert a #+REVEAL_ header block at the top of the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((title (read-from-minibuffer "Presentation title: ")))
    (save-excursion
      (goto-char (point-min))
      (insert (cj/--reveal-header-template title)))
    (message "Inserted reveal.js headers")))

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

(global-set-key (kbd "C-; p e") #'cj/reveal-export)
(global-set-key (kbd "C-; p p") #'cj/reveal-preview-start)
(global-set-key (kbd "C-; p s") #'cj/reveal-preview-stop)
(global-set-key (kbd "C-; p h") #'cj/reveal-insert-header)
(global-set-key (kbd "C-; p n") #'cj/reveal-new)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; p" "presentations"
    "C-; p e" "export & open"
    "C-; p p" "start live preview"
    "C-; p s" "stop live preview"
    "C-; p h" "insert headers"
    "C-; p n" "new presentation"))

(provide 'org-reveal-config)
;;; org-reveal-config.el ends here
