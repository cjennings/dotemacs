;;; git_log.el --- Read-only git log tool for gptel -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: convenience, tools, git

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gptel tool returning `git log --oneline -n N' for a path under the
;; user's home directory.  Read-only.  N is capped to keep the model's
;; context budget predictable.

;;; Code:

(require 'gptel)

(defconst cj/gptel-git-log--max-count 100
  "Hard cap on the number of commits `git_log' will return.")

(defconst cj/gptel-git-log--default-count 20
  "Default commit count when the caller doesn't specify one.")

(defun cj/gptel-git-log--validate-path (path)
  "Validate PATH for a git log call.  Return the expanded path on success.
Same contract as the git_status validator: must be under HOME, must
be a directory, must be inside a git working tree."
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

(defun cj/gptel-git-log--effective-count (n)
  "Return the commit count to use given caller-supplied N.
Nil / non-integer N → `cj/gptel-git-log--default-count'.
Values above `cj/gptel-git-log--max-count' get capped."
  (cond
   ((not (integerp n)) cj/gptel-git-log--default-count)
   ((< n 1) cj/gptel-git-log--default-count)
   ((> n cj/gptel-git-log--max-count) cj/gptel-git-log--max-count)
   (t n)))

(defun cj/gptel-git-log--run (path &optional n since)
  "Run `git log --oneline -n N' in PATH.  Return the output as a string.
SINCE, if a non-empty string, is passed as `--since=SINCE'."
  (let* ((dir (cj/gptel-git-log--validate-path path))
         (count (cj/gptel-git-log--effective-count n))
         (args (list "-c" "color.ui=false"
                     "log" "--oneline"
                     (format "-n%d" count)))
         (args (if (and (stringp since) (not (string-empty-p since)))
                   (append args (list (format "--since=%s" since)))
                 args))
         (default-directory dir))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (unless (zerop exit)
          (error "git log exited with %d: %s" exit (buffer-string)))
        (let ((out (buffer-string)))
          (if (string-empty-p out)
              (format "No commits in %s matching the filter" dir)
            out))))))

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "git_log"
   :function (lambda (path &optional n since)
               (cj/gptel-git-log--run path n since))
   :description "Return the output of `git log --oneline -n N' for a directory in the user's home tree.  Read-only.  N defaults to 20 and is capped at 100.  Use SINCE to filter commits more recent than a date expression git understands (e.g. '2 weeks ago', '2026-05-01')."
   :args (list '(:name "path"
                       :type string
                       :description "Directory inside a git working tree.  Either an absolute path under the user's home directory or a path relative to it (e.g. 'code/myproject').")
               '(:name "n"
                       :type integer
                       :description "Number of commits to return.  Defaults to 20; capped at 100."
                       :optional t)
               '(:name "since"
                       :type string
                       :description "Optional date expression for `git log --since='; e.g. '2 weeks ago' or '2026-05-01'."
                       :optional t))
   :category "git"
   :confirm nil
   :include t)

  (add-to-list 'gptel-tools (gptel-get-tool '("git" "git_log"))))

(provide 'git_log)
;;; git_log.el ends here
