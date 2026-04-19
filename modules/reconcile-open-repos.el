;;; reconcile-open-repos.el --- reconcile open repos -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Git repository reconciliation workflow for multiple projects. Ensures all git
;; repositories in your projects/ and code/ directories are synchronized with
;; remotes and have no uncommitted work at the start and end of work sessions.
;; The workflow iterates through all git repositories in projects-dir and
;; code-dir, skips local-only repos and http/https remotes (reference clones),
;; silently pulls latest changes for clean repos, and for dirty repos stashes
;; changes, pulls, pops stash, and opens Magit for review. Also checks org-dir
;; and user-emacs-directory individually.
;;
;; Main function: cj/check-for-open-work (bound to M-P)
;;
;; Dependencies: Requires projects-dir, code-dir, and org-dir to be defined in
;; init.el. Uses Magit for manual intervention when repositories have uncommitted
;; changes.

;;; Code:

;; Forward declarations for variables defined in init.el
(eval-when-compile
  (defvar projects-dir)
  (defvar code-dir)
  (defvar org-dir))

;; Forward declaration for magit
(declare-function magit-status "magit" (&optional directory cache))

;; ------------------------------ Skip Predicate -------------------------------

(defun cj/reconcile--should-skip-p (directory)
  "Return non-nil if DIRECTORY should be skipped during reconciliation.
Skips directories without .git, without a remote, or with http/https remotes
\(reference clones)."
  (let ((default-directory directory))
    (or (not (file-directory-p (expand-file-name ".git" directory)))
        (let ((remote-url (string-trim (shell-command-to-string
                                        "git config --get remote.origin.url"))))
          (or (string-empty-p remote-url)
              (string-match-p "^\\(http\\|https\\)://" remote-url))))))

;; -------------------------------- Pull Clean --------------------------------

(defun cj/reconcile--pull-clean (directory)
  "Pull latest changes for clean git repo at DIRECTORY."
  (let* ((default-directory directory)
         (pull-result (shell-command "git pull --rebase --quiet")))
    (unless (= pull-result 0)
      (message "Warning: git pull failed for %s (exit code: %d)" directory pull-result))))

;; -------------------------------- Pull Dirty --------------------------------

(defun cj/reconcile--pull-dirty (directory)
  "Stash, pull, pop stash, and open Magit for dirty repo at DIRECTORY."
  (let ((default-directory directory))
    (message "%s contains uncommitted work" directory)
    (let ((stash-result (shell-command "git stash --quiet")))
      (if (= stash-result 0)
          (let ((pull-result (shell-command "git pull --rebase --quiet")))
            (when (= pull-result 0)
              (let ((stash-pop-result (shell-command "git stash pop --quiet")))
                (unless (= stash-pop-result 0)
                  (message "Warning: git stash pop failed for %s - opening Magit" directory))))
            (unless (= pull-result 0)
              (message "Warning: git pull failed for %s - opening Magit" directory)))
        (message "Warning: git stash failed for %s - opening Magit" directory)))
    (magit-status directory)))

;; -------------------------- Reconcile Git Directory --------------------------

(defun cj/reconcile-git-directory (directory)
  "Reconcile unopened work in a git project DIRECTORY.
Skips local-only repos and http/https remotes.  For clean repos, silently pulls
latest changes.  For dirty repos, stashes changes, pulls, pops stash, and opens
Magit for review."
  (message "checking: %s" directory)
  (unless (cj/reconcile--should-skip-p directory)
    (let ((default-directory directory))
      (if (string-empty-p (shell-command-to-string "git status --porcelain"))
          (cj/reconcile--pull-clean directory)
        (cj/reconcile--pull-dirty directory)))))

;; ---------------------------- Check For Open Work ----------------------------

(defun cj/find-git-repos (directory)
  "Recursively find all git repositories under DIRECTORY.
Returns a list of directory paths that contain a .git subdirectory."
  (let (repos)
    (dolist (child (directory-files directory t "^[^.]+$" 'nosort))
      (when (file-directory-p child)
        (when (file-directory-p (expand-file-name ".git" child))
          (push child repos))
        (setq repos (nconc repos (cj/find-git-repos child)))))
    repos))

(defun cj/check-for-open-work ()
  "Check all project directories for open work."
  (interactive)
  ;; these are constants defined in init.el
  ;; recursively find and check all git repos under these directories
  (dolist (base-dir (list projects-dir code-dir))
    (when (and (boundp 'base-dir) base-dir (file-directory-p base-dir))
      (dolist (repo (cj/find-git-repos base-dir))
        (cj/reconcile-git-directory repo))))

  ;; check these directories individually
  (when (and (boundp 'org-dir) org-dir (file-directory-p org-dir))
    (cj/reconcile-git-directory org-dir))
  (when (and (boundp 'user-emacs-directory) user-emacs-directory (file-directory-p user-emacs-directory))
    (cj/reconcile-git-directory user-emacs-directory))

  ;; communicate when finished.
  (message "Complete. All repositories checked and updated"))

(keymap-global-set "M-P" #'cj/check-for-open-work)

(provide 'reconcile-open-repos)
;;; reconcile-open-repos.el ends here.
