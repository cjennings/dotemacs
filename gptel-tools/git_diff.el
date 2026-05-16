;;; git_diff.el --- Read-only git diff tool for gptel -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: convenience, tools, git

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gptel tool returning `git diff' output for a path under the user's
;; home directory.  Read-only.  Output is capped at ~500KB so a
;; runaway diff can't blow up the model's context budget; truncation
;; is reported in the output when it triggers.

;;; Code:

(require 'gptel)

(defconst cj/gptel-git-diff--max-output-bytes (* 500 1024)
  "Cap on diff output size.  Larger diffs are truncated with a note.")

(defun cj/gptel-git-diff--validate-path (path)
  "Validate PATH for a git diff call.  Return the expanded path on success.
Same contract as the other git_* validators: under HOME, a directory,
inside a git working tree."
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

(defun cj/gptel-git-diff--truncate (text)
  "Truncate TEXT to `cj/gptel-git-diff--max-output-bytes' bytes.
Returns TEXT unchanged when it's under the cap, otherwise returns the
prefix plus a one-line truncation marker."
  (if (<= (length text) cj/gptel-git-diff--max-output-bytes)
      text
    (concat (substring text 0 cj/gptel-git-diff--max-output-bytes)
            (format
             "\n\n[truncated: output exceeded %d bytes; %d bytes total]"
             cj/gptel-git-diff--max-output-bytes
             (length text)))))

(defun cj/gptel-git-diff--build-args (ref1 ref2 file)
  "Build the `git' argv from optional REF1, REF2, FILE.
Uses `-c color.ui=false' at the git level so output is plain across
git subcommands."
  (let ((args (list "-c" "color.ui=false" "diff")))
    (when (and (stringp ref1) (not (string-empty-p ref1)))
      (setq args (append args (list ref1))))
    (when (and (stringp ref2) (not (string-empty-p ref2)))
      (setq args (append args (list ref2))))
    (when (and (stringp file) (not (string-empty-p file)))
      (setq args (append args (list "--" file))))
    args))

(defun cj/gptel-git-diff--run (path &optional ref1 ref2 file)
  "Run `git diff [REF1 [REF2]] [-- FILE]' in PATH.  Return the output."
  (let* ((dir (cj/gptel-git-diff--validate-path path))
         (args (cj/gptel-git-diff--build-args ref1 ref2 file))
         (default-directory dir))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (unless (or (zerop exit) (= exit 1))
          (error "git diff exited with %d: %s" exit (buffer-string)))
        (let ((out (buffer-string)))
          (if (string-empty-p out)
              (format "No diff in %s for the given refs/file" dir)
            (cj/gptel-git-diff--truncate out)))))))

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "git_diff"
   :function (lambda (path &optional ref1 ref2 file)
               (cj/gptel-git-diff--run path ref1 ref2 file))
   :description "Return the output of `git diff' for a directory in the user's home tree.  Read-only.  REF1 and REF2 are optional git revisions (commit SHA, branch, tag, or expressions like HEAD~3); when both are present the diff is between them, when only REF1 is present the diff is between REF1 and the working tree, when neither is present the diff is unstaged-vs-HEAD.  FILE optionally narrows the diff to one path.  Output is capped at ~500KB."
   :args (list '(:name "path"
                       :type string
                       :description "Directory inside a git working tree.  Either an absolute path under the user's home directory or a path relative to it (e.g. 'code/myproject').")
               '(:name "ref1"
                       :type string
                       :description "Optional first git revision (commit, branch, tag, or expression like HEAD~3)."
                       :optional t)
               '(:name "ref2"
                       :type string
                       :description "Optional second git revision; pair with REF1 to diff between two refs."
                       :optional t)
               '(:name "file"
                       :type string
                       :description "Optional path inside the working tree to narrow the diff to."
                       :optional t))
   :category "git"
   :confirm nil
   :include t)

  (add-to-list 'gptel-tools (gptel-get-tool '("git" "git_diff"))))

(provide 'git_diff)
;;; git_diff.el ends here
