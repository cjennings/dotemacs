;;; reconcile-open-repos.el --- reconcile open repos -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Git repository reconciliation workflow for multiple projects. The workflow
;; iterates through all git repositories in projects-dir and code-dir, skips
;; local-only repos and remotes matching `cj/reconcile-skipped-remote-regexp',
;; pulls latest changes for clean repos, and opens Magit for dirty repos without
;; stashing, rebasing, or popping work automatically. Also checks org-dir and
;; user-emacs-directory individually.
;;
;; Main function: cj/check-for-open-work (bound to M-P)
;;
;; Dependencies: Requires projects-dir, code-dir, and org-dir to be defined in
;; init.el. Uses Magit for manual intervention when repositories have uncommitted
;; changes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Forward declarations for variables defined in init.el
(eval-when-compile
  (defvar projects-dir)
  (defvar code-dir)
  (defvar org-dir))

;; Forward declaration for magit
(declare-function magit-status "magit" (&optional directory cache))

(defcustom cj/reconcile-skipped-remote-regexp "^https?://"
  "Regexp matching remote URLs that should be skipped by reconciliation.
This defaults to HTTP/HTTPS remotes because this setup treats those as
reference clones rather than active work repositories."
  :type 'regexp
  :group 'cj)

(defcustom cj/reconcile-pruned-directory-names
  '(".git" ".hg" ".svn"
    "node_modules" ".venv" "venv" "__pycache__"
    "target" "build" "dist" ".next" ".cache" "vendor")
  "Directory basenames not descended while discovering git repositories."
  :type '(repeat string)
  :group 'cj)

(defvar cj/reconcile-results nil
  "Most recent list of repository reconciliation result plists.")

;; ------------------------------- Git Process --------------------------------

(defun cj/reconcile--git (directory &rest args)
  "Run git in DIRECTORY with ARGS.
Return a plist with :exit and :output.  Git is invoked through
`process-file' with an argv list, not through a shell."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((exit-code (apply #'process-file "git" nil (list t t) nil args)))
        (list :exit exit-code
              :output (buffer-string)
              :args args)))))

(defun cj/reconcile--git-output (directory &rest args)
  "Run git in DIRECTORY with ARGS and return trimmed output on success."
  (let ((result (apply #'cj/reconcile--git directory args)))
    (when (zerop (plist-get result :exit))
      (string-trim (plist-get result :output)))))

;; ------------------------------ Skip Predicate -------------------------------

(defun cj/reconcile--skip-reason (directory)
  "Return a skip reason symbol for DIRECTORY, or nil if it should be processed."
  (cond
   ((not (file-directory-p (expand-file-name ".git" directory)))
    'not-a-git-repo)
   (t
    (let ((remote-url (cj/reconcile--git-output
                       directory "config" "--get" "remote.origin.url")))
      (cond
       ((or (null remote-url) (string-empty-p remote-url)) 'no-remote)
       ((and cj/reconcile-skipped-remote-regexp
             (string-match-p cj/reconcile-skipped-remote-regexp remote-url))
        'skipped-remote)
       (t nil))))))

(defun cj/reconcile--should-skip-p (directory)
  "Return non-nil if DIRECTORY should be skipped during reconciliation.
Skips directories without .git, without a remote, or with remotes matching
`cj/reconcile-skipped-remote-regexp'."
  (and (cj/reconcile--skip-reason directory) t))

;; -------------------------------- Pull Clean --------------------------------

(defun cj/reconcile--pull-clean (directory)
  "Pull latest changes for clean git repo at DIRECTORY."
  (let ((result (cj/reconcile--git directory "pull" "--rebase" "--quiet")))
    (if (zerop (plist-get result :exit))
        (list :directory directory :status 'pulled :output (plist-get result :output))
      (message "Warning: git pull failed for %s (exit code: %d)"
               directory
               (plist-get result :exit))
      (list :directory directory
            :status 'pull-failed
            :exit (plist-get result :exit)
            :output (plist-get result :output)))))

;; -------------------------------- Pull Dirty --------------------------------

(defun cj/reconcile--pull-dirty (directory)
  "Open Magit for dirty repo at DIRECTORY without modifying worktree state."
  (message "%s contains uncommitted work; opening Magit for review" directory)
  (magit-status directory)
  (list :directory directory :status 'needs-review))

;; ------------------------------- Repo Status --------------------------------

(defun cj/reconcile--dirty-p (directory)
  "Return non-nil if git repo DIRECTORY has uncommitted work."
  (let ((status (cj/reconcile--git directory "status" "--porcelain")))
    (if (zerop (plist-get status :exit))
        (not (string-empty-p (string-trim (plist-get status :output))))
      (message "Warning: git status failed for %s (exit code: %d)"
               directory
               (plist-get status :exit))
      'status-failed)))

;; -------------------------- Reconcile Git Directory --------------------------

(defun cj/reconcile-git-directory (directory)
  "Reconcile unopened work in a git project DIRECTORY.
Skips local-only repos and configured remote policies.  For clean repos, pulls
latest changes.  For dirty repos, opens Magit for review without mutating the
worktree."
  (message "checking: %s" directory)
  (let ((skip-reason (cj/reconcile--skip-reason directory)))
    (cond
     (skip-reason
      (message "Skipping %s: %s" directory skip-reason)
      (list :directory directory :status 'skipped :reason skip-reason))
     (t
      (let ((dirty (cj/reconcile--dirty-p directory)))
        (cond
         ((eq dirty 'status-failed)
          (list :directory directory :status 'status-failed))
         (dirty
          (cj/reconcile--pull-dirty directory))
         (t
          (cj/reconcile--pull-clean directory))))))))

;; ---------------------------- Check For Open Work ----------------------------

(defun cj/reconcile--pruned-directory-p (directory)
  "Return non-nil if DIRECTORY should be pruned during repo discovery."
  (member (file-name-nondirectory (directory-file-name directory))
          cj/reconcile-pruned-directory-names))

(defun cj/find-git-repos (directory &optional include-nested)
  "Recursively find all git repositories under DIRECTORY.
Returns a list of directory paths that contain a .git subdirectory.
Prunes generated/heavy directories.  Once a repository root is found, do not
descend into it unless INCLUDE-NESTED is non-nil."
  (let (repos)
    (when (file-directory-p directory)
      (dolist (child (directory-files directory t "^[^.]+$" 'nosort))
        (when (and (file-directory-p child)
                   (not (cj/reconcile--pruned-directory-p child)))
          (if (file-directory-p (expand-file-name ".git" child))
              (progn
                (push child repos)
                (when include-nested
                  (setq repos (nconc repos (cj/find-git-repos child include-nested)))))
            (setq repos (nconc repos (cj/find-git-repos child include-nested)))))))
    repos))

(defun cj/reconcile--summary-message (results)
  "Return a concise summary string for reconciliation RESULTS."
  (let ((pulled 0)
        (review 0)
        (skipped 0)
        (failed 0))
    (dolist (result results)
      (pcase (plist-get result :status)
        ('pulled (cl-incf pulled))
        ('needs-review (cl-incf review))
        ('skipped (cl-incf skipped))
        ((or 'pull-failed 'status-failed) (cl-incf failed))))
    (format "Complete. Repositories checked: %d, pulled: %d, needs review: %d, skipped: %d, failed: %d"
            (length results) pulled review skipped failed)))

(defun cj/check-for-open-work ()
  "Check all project directories for open work."
  (interactive)
  ;; these are constants defined in init.el
  ;; recursively find and check all git repos under these directories
  (setq cj/reconcile-results nil)
  (dolist (base-dir (list projects-dir code-dir))
    (when (and base-dir (file-directory-p base-dir))
      (dolist (repo (cj/find-git-repos base-dir))
        (push (cj/reconcile-git-directory repo) cj/reconcile-results))))

  ;; check these directories individually
  (when (and (boundp 'org-dir) org-dir (file-directory-p org-dir))
    (push (cj/reconcile-git-directory org-dir) cj/reconcile-results))
  (when (and (boundp 'user-emacs-directory) user-emacs-directory (file-directory-p user-emacs-directory))
    (push (cj/reconcile-git-directory user-emacs-directory) cj/reconcile-results))

  ;; communicate when finished.
  (setq cj/reconcile-results (nreverse (delq nil cj/reconcile-results)))
  (message "%s" (cj/reconcile--summary-message cj/reconcile-results)))

(keymap-global-set "M-P" #'cj/check-for-open-work)

(provide 'reconcile-open-repos)
;;; reconcile-open-repos.el ends here.
