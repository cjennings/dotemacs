;;; git_status.el --- Read-only git status tool for gptel -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: convenience, tools, git

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gptel tool returning `git status --short --branch' for a path under
;; the user's home directory.  Read-only: never writes to the repo,
;; never runs anything that could mutate state.  Path validation
;; rejects anything outside HOME and any path that doesn't resolve to
;; a directory inside a git working tree.

;;; Code:

(require 'gptel)
(require 'cl-lib)

(defun cj/gptel-git-status--validate-path (path)
  "Validate PATH as a usable working directory for a git status call.
PATH must resolve under the user's home directory, must be an
existing directory, and must be inside a git working tree.  Returns
the expanded path string on success; signals `error' otherwise."
  (let* ((home (file-name-as-directory (file-truename (expand-file-name "~"))))
         (full (expand-file-name (or path "~") "~")))
    (unless (string-prefix-p (expand-file-name "~") full)
      (error "Path must be within home directory: %s" path))
    (unless (file-directory-p full)
      (error "Not a directory: %s" full))
    (let ((resolved (file-truename full)))
      (unless (or (string= resolved (directory-file-name home))
                  (string-prefix-p home resolved))
        (error "Resolved path must be within home directory: %s" path))
      (setq full resolved))
    (let ((default-directory full))
      (unless (zerop (process-file "git" nil nil nil
                                   "rev-parse" "--is-inside-work-tree"))
        (error "Not a git working tree: %s" full)))
    full))

(defun cj/gptel-git-status--run (path)
  "Run `git status --short --branch' in PATH.  Return the output.
Color is disabled via `-c color.ui=false' at the git level (`git status'
itself doesn't accept `--no-color' like `git log' / `git diff' do)."
  (let* ((dir (cj/gptel-git-status--validate-path path))
         (default-directory dir))
    (with-temp-buffer
      (let ((exit (process-file "git" nil t nil
                                "-c" "color.ui=false"
                                "status" "--short" "--branch")))
        (unless (zerop exit)
          (error "git status exited with %d: %s" exit (buffer-string)))
        ;; `--branch' always prints a `## <branch>' header, so empty
        ;; output is unreachable.  Detect a clean tree by counting the
        ;; non-branch lines: if only the header is present, no files
        ;; are modified / staged / untracked.
        (let* ((out (buffer-string))
               (non-branch-lines
                (cl-count-if
                 (lambda (l)
                   (and (not (string-empty-p l))
                        (not (string-prefix-p "## " l))))
                 (split-string out "\n"))))
          (if (zerop non-branch-lines)
              (format "Clean working tree in %s\n%s" dir (string-trim out))
            out))))))

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "git_status"
   :function (lambda (path) (cj/gptel-git-status--run path))
   :description "Return the output of `git status --short --branch' for a directory in the user's home tree.  Read-only.  Useful for seeing which files are modified, staged, or untracked, and how the current branch compares to its upstream."
   :args (list '(:name "path"
                       :type string
                       :description "Directory inside a git working tree.  Either an absolute path under the user's home directory or a path relative to it (e.g. 'code/myproject')."))
   :category "git"
   :confirm nil
   :include t)

  (add-to-list 'gptel-tools (gptel-get-tool '("git" "git_status"))))

(provide 'git_status)
;;; git_status.el ends here
