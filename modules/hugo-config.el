;;; hugo-config.el --- Hugo Blog Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integrates ox-hugo for publishing Org files to a Hugo website.
;;
;; One-file-per-post workflow:
;; - Each blog post is a standalone Org file in content-org/log/
;; - File-level keywords control Hugo front matter
;; - Export with C-; h e, create new posts with C-; h n
;;
;; Keybindings (C-; h prefix):
;; - C-; h n : New post (create from template)
;; - C-; h e : Export current post to Hugo markdown
;; - C-; h o : Open blog source directory in dirvish
;; - C-; h O : Open blog source directory in system file manager
;; - C-; h d : Toggle draft status (TODO/DONE)

;;; Code:

(require 'user-constants)
(require 'host-environment)

;; --------------------------------- Constants ---------------------------------

(defconst cj/hugo-content-org-dir
  (expand-file-name "content-org/log/" website-dir)
  "Directory containing Org source files for Hugo blog posts.")

;; ---------------------------------- ox-hugo ----------------------------------

(use-package ox-hugo
  :after ox)

;; ----------------------------- Hugo Blog Functions ---------------------------

(defun cj/hugo--post-file-path (title)
  "Return the file path for a Hugo post with TITLE.
Generates a slug from TITLE using `org-hugo-slug' and returns
the full path under `cj/hugo-content-org-dir'.
Assumes ox-hugo is already loaded (via use-package declaration above)."
  (let ((slug (org-hugo-slug title)))
    (expand-file-name (concat slug ".org") cj/hugo-content-org-dir)))

(defun cj/hugo--post-template (title date)
  "Return the Org front matter template for a Hugo post.
TITLE is the post title, DATE is the date string (YYYY-MM-DD)."
  (format "#+hugo_base_dir: ../../
#+hugo_section: log
#+hugo_auto_set_lastmod: t
#+title: %s
#+date: %s
#+hugo_tags:
#+hugo_draft: true
#+hugo_custom_front_matter: :description \"\"

" title date))

(defun cj/hugo-new-post ()
  "Create a new Hugo blog post as a standalone Org file.
Prompts for title, generates the slug filename, and opens the
new file with Hugo front matter keywords pre-filled."
  (interactive)
  (let* ((title (read-from-minibuffer "Post Title: "))
         (file (cj/hugo--post-file-path title))
         (date (format-time-string "%Y-%m-%d")))
    (when (file-exists-p file)
      (user-error "Post already exists: %s" file))
    (unless (file-directory-p cj/hugo-content-org-dir)
      (make-directory cj/hugo-content-org-dir t))
    (find-file file)
    (insert (cj/hugo--post-template title date))
    (save-buffer)
    (message "New post: %s" file)))

(defun cj/hugo-export-post ()
  "Export the current Org file to Hugo-compatible Markdown."
  (interactive)
  (require 'ox-hugo)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-hugo-export-to-md)
  (message "Exported: %s" (buffer-name)))

(defun cj/hugo-open-blog-dir ()
  "Open the blog source directory in dirvish/dired."
  (interactive)
  (unless (file-directory-p cj/hugo-content-org-dir)
    (make-directory cj/hugo-content-org-dir t))
  (dired cj/hugo-content-org-dir))

(defun cj/hugo-open-blog-dir-external ()
  "Open the blog source directory in the system file manager."
  (interactive)
  (unless (file-directory-p cj/hugo-content-org-dir)
    (make-directory cj/hugo-content-org-dir t))
  (let ((cmd (cond
              ((env-macos-p) "open")
              ((env-windows-p) "explorer.exe")
              (t "xdg-open"))))
    (start-process "hugo-file-manager" nil cmd cj/hugo-content-org-dir)))

(defun cj/hugo-toggle-draft ()
  "Toggle the draft status of the current Hugo post.
Switches #+hugo_draft between true and false."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+hugo_draft: *\\(true\\|false\\)" nil t)
        (let ((current (match-string 1)))
          (replace-match (if (string= current "true") "false" "true") t t nil 1)
          (save-buffer)
          (message "Draft: %s → %s" current
                   (if (string= current "true") "false" "true")))
      (user-error "No #+hugo_draft keyword found in this file"))))

;; -------------------------------- Keybindings --------------------------------

(global-set-key (kbd "C-; h n") #'cj/hugo-new-post)
(global-set-key (kbd "C-; h e") #'cj/hugo-export-post)
(global-set-key (kbd "C-; h o") #'cj/hugo-open-blog-dir)
(global-set-key (kbd "C-; h O") #'cj/hugo-open-blog-dir-external)
(global-set-key (kbd "C-; h d") #'cj/hugo-toggle-draft)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; h" "hugo blog menu"
    "C-; h n" "new post"
    "C-; h e" "export post"
    "C-; h o" "open in dirvish"
    "C-; h O" "open in file manager"
    "C-; h d" "toggle draft"))

(provide 'hugo-config)
;;; hugo-config.el ends here
