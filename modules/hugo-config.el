;;; hugo-config.el --- Hugo Blog Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integrates ox-hugo for publishing Org files to a Hugo website.
;;
;; One-file-per-post workflow:
;; - Each blog post is a standalone Org file in content-org/log/
;; - File-level keywords control Hugo front matter
;; - Publish by committing and pushing the website repo; the server-side
;;   post-receive hook on cjennings.net rebuilds Hugo and deploys
;;
;; Keybindings live under the C-; h prefix. See the bottom of this file
;; or the which-key panel for the full listing.

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

;; ------------------------------- Post Creation -------------------------------

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

;; -------------------------------- Post Export --------------------------------

(defun cj/hugo-export-post ()
  "Export the current Org file to Hugo-compatible Markdown."
  (interactive)
  (require 'ox-hugo)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-hugo-export-to-md)
  (message "Exported: %s" (buffer-name)))

;; ---------------------------- Directory Navigation ---------------------------

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

;; ----------------------------- Draft Management ------------------------------

(defun cj/hugo--post-metadata (file)
  "Return minimal front-matter metadata for Hugo post FILE, or nil if not one.
A file counts as a Hugo post only if it contains `#+hugo_draft: true' or
`#+hugo_draft: false' in its front matter region.
Returns a plist (:title TITLE :draft BOOL). TITLE falls back to the file
basename when `#+title:' is absent. Reads only the first 2048 bytes."
  (with-temp-buffer
    (insert-file-contents file nil 0 2048)
    (let (title draft is-hugo)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title: *\\(.+\\)$" nil t)
        (setq title (match-string 1)))
      (goto-char (point-min))
      (when (re-search-forward "^#\\+hugo_draft: *\\(true\\|false\\)" nil t)
        (setq draft (string= (match-string 1) "true")
              is-hugo t))
      (when is-hugo
        (list :title (or title (file-name-base file)) :draft draft)))))

(defun cj/hugo--collect-drafts (dir)
  "Return alist of (TITLE . FILEPATH) for draft Hugo posts under DIR.
Walks non-recursively through DIR for .org files and keeps only those
whose `cj/hugo--post-metadata' returns a :draft-t plist."
  (let (drafts)
    (dolist (f (directory-files dir t "\\.org\\'"))
      (let ((meta (cj/hugo--post-metadata f)))
        (when (and meta (plist-get meta :draft))
          (push (cons (plist-get meta :title) f) drafts))))
    drafts))

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

(defun cj/hugo-open-draft ()
  "Pick a draft post via completing-read and open it."
  (interactive)
  (let ((drafts (cj/hugo--collect-drafts cj/hugo-content-org-dir)))
    (if (null drafts)
        (message "No drafts found in %s" cj/hugo-content-org-dir)
      (let ((choice (completing-read "Open draft: "
                                     (mapcar #'car drafts) nil t)))
        (find-file (cdr (assoc choice drafts)))))))

;; ---------------------------- Preview and Publish ----------------------------

(defvar cj/hugo--preview-process nil
  "Handle to the running hugo preview server, or nil.")

(defun cj/hugo--preview-filter (proc output)
  "Process filter for the hugo preview server.
Appends OUTPUT to PROC's buffer and opens the browser the first time
Hugo reports the server is ready. Hugo prints a line containing
\"Web Server is available at\" once it has bound the port, so waiting
for that string is more reliable than a fixed delay."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert output)
          (set-marker (process-mark proc) (point)))
        (when moving (goto-char (process-mark proc))))))
  (when (and (process-live-p proc)
             (string-match-p "Web Server is available at" output))
    (browse-url "http://localhost:1313/")
    (set-process-filter proc nil)))

(defun cj/hugo--preview-sentinel (proc _event)
  "Sentinel for the hugo preview server.
Clears `cj/hugo--preview-process' on any exit and announces crashes.
User-initiated stops arrive with status `signal' and are silent here
because `cj/hugo-preview' already prints its own stop message."
  (when (memq (process-status proc) '(exit signal))
    (setq cj/hugo--preview-process nil)
    (when (and (eq (process-status proc) 'exit)
               (not (zerop (process-exit-status proc))))
      (message "hugo server crashed (exit %d) — see *hugo-server* buffer"
               (process-exit-status proc)))))

(defun cj/hugo-preview ()
  "Toggle the `hugo server' preview.
Start the server and open the browser if stopped; stop it if running.
The browser opens only once Hugo has finished its initial build and is
actually listening on the port. If Hugo exits on its own (for example
a template error), the sentinel reports the failure."
  (interactive)
  (if (process-live-p cj/hugo--preview-process)
      (progn
        (kill-process cj/hugo--preview-process)
        (setq cj/hugo--preview-process nil)
        (message "hugo server stopped"))
    (let ((default-directory website-dir))
      (setq cj/hugo--preview-process
            (start-process "hugo-server" "*hugo-server*"
                           "hugo" "server" "-D"
                           "--noHTTPCache" "--disableFastRender"))
      (set-process-filter cj/hugo--preview-process
                          #'cj/hugo--preview-filter)
      (set-process-sentinel cj/hugo--preview-process
                            #'cj/hugo--preview-sentinel)
      (message "hugo server starting — browser will open when ready"))))

(declare-function magit-status-setup-buffer "magit-status")

(defun cj/hugo-publish ()
  "Open magit-status on the website repo so a push triggers server-side deploy.
The cjennings.net bare repo's post-receive hook rebuilds Hugo and writes
to /var/www/cjennings/, so a successful push is the deploy."
  (interactive)
  (magit-status-setup-buffer website-dir))

;; -------------------------------- Keybindings --------------------------------

(global-set-key (kbd "C-; h n") #'cj/hugo-new-post)
(global-set-key (kbd "C-; h e") #'cj/hugo-export-post)
(global-set-key (kbd "C-; h o") #'cj/hugo-open-blog-dir)
(global-set-key (kbd "C-; h O") #'cj/hugo-open-blog-dir-external)
(global-set-key (kbd "C-; h d") #'cj/hugo-open-draft)
(global-set-key (kbd "C-; h D") #'cj/hugo-toggle-draft)
(global-set-key (kbd "C-; h p") #'cj/hugo-preview)
(global-set-key (kbd "C-; h P") #'cj/hugo-publish)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; h" "hugo blog menu"
    "C-; h n" "new post"
    "C-; h e" "export post"
    "C-; h o" "open in dirvish"
    "C-; h O" "open in file manager"
    "C-; h d" "open draft"
    "C-; h D" "toggle draft"
    "C-; h p" "preview (toggle)"
    "C-; h P" "publish (magit push)"))

(provide 'hugo-config)
;;; hugo-config.el ends here
