;;; reconcile-open-repos.el --- reconcile open repos -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; I have tried to keep this Emacs config as general as possible, and move all
;; config related to my personal workflows here.

;; I typically work with multiple git repositories the day, and can forget to
;; commit uncommitted work. Also, I often forget to start a session by pulling
;; changes. So, making it a habit to run cj/check-for-open-work when starting
;; and ending the work session allows me to know that I've iterated through all
;; git repositories in my projects and code directories and have reconciled all
;; changes.

;;; Code:

;; -------------------------- Reconcile Git Directory --------------------------

(defun cj/reconcile-git-directory (directory)
  "Reconcile unopened work in a git project directory leveraging magit."
  (message "checking: %s" directory)
  (let ((default-directory directory))
    ;; Check for the presence of the .git directory
    (if (file-directory-p (expand-file-name ".git" directory))
        (progn
          (let ((remote-url (shell-command-to-string "git config --get remote.origin.url")))
            (setq remote-url (string-trim remote-url))

            ;; skip local git repos, or remote URLs that are http or https,
            ;; these are typically cloned for reference only
            (unless (or (string-empty-p remote-url)
                        (string-match-p "^\\(http\\|https\\)://" remote-url))

              ;; if git directory is clean, pulling generates no errors
              (if (string-empty-p (shell-command-to-string "git status --porcelain"))
                  (progn
                    ;; (message "%s is a clean git repository" directory)
                    (shell-command "git pull --quiet"))

                ;; if directory not clean, pull latest changes and display Magit for manual intervention
                (progn
                  (message "%s contains uncommitted work" directory)
                  (shell-command "git stash --quiet")
                  (shell-command "git pull --quiet")
                  (shell-command "git stash pop --quiet")
                  (call-interactively #'magit-status)
                  ;; pause until magit buffer is closed
                  (while (buffer-live-p (get-buffer (format "*magit: %s*" (file-name-nondirectory directory))))
                    (sit-for 0.5))))))))))

;; ---------------------------- Check For Open Work ----------------------------

(defun cj/check-for-open-work ()
  "Check all project directories for open work."
  (interactive)
  ;; these are constants defined in init.el
  ;; children of these directories will be checked
  (dolist (base-dir (list projects-dir code-dir))
    (when (file-directory-p base-dir)
      (dolist (child-dir (directory-files base-dir t "^[^.]+$" 'nosort))
        (when (file-directory-p child-dir)
          (cj/reconcile-git-directory child-dir)))))

  ;; check these directories individually
  (cj/reconcile-git-directory org-dir)
  (cj/reconcile-git-directory user-emacs-directory)

  ;; communicate when finished.
  (message "Complete. All project repositories checked for uncommitted work and code updated from remote repository"))
(global-set-key (kbd "M-P") 'cj/check-for-open-work)

(provide 'reconcile-open-repos)
;;; reconcile-open-repos.el ends here.
