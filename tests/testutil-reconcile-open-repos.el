;;; testutil-reconcile-open-repos.el --- Test helpers for reconcile-open-repos -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides helper macros and functions for testing reconcile-open-repos.
;; Creates temporary directory trees with fake .git dirs and mocks shell commands.

;;; Code:

(require 'cl-lib)

(defmacro reconcile-test-with-temp-dirs (dir-spec &rest body)
  "Create a temp directory tree per DIR-SPEC, bind `test-root', then run BODY.
DIR-SPEC is a list of relative paths.  Paths ending in / create directories;
others create files.  A path containing `.git/' creates the .git dir automatically.

Example:
  (reconcile-test-with-temp-dirs
   (\"repo-a/.git/\" \"repo-b/subdir/\" \"not-a-repo/readme.txt\")
   ...use test-root...)"
  (declare (indent 1))
  `(let ((test-root (make-temp-file "reconcile-test-" t)))
     (unwind-protect
         (progn
           (dolist (path ',dir-spec)
             (let ((full (expand-file-name path test-root)))
               (if (string-suffix-p "/" path)
                   (make-directory full t)
                 (progn
                   (make-directory (file-name-directory full) t)
                   (write-region "" nil full)))))
           ,@body)
       (delete-directory test-root t))))

(defmacro reconcile-test-with-shell-mocks (shell-cmd-fn shell-cmd-to-str-fn &rest body)
  "Run BODY with `shell-command' and `shell-command-to-string' overridden.
SHELL-CMD-FN receives (command) and returns an exit code integer.
SHELL-CMD-TO-STR-FN receives (command) and returns a string."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'shell-command)
              (lambda (cmd &rest _) (funcall ,shell-cmd-fn cmd)))
             ((symbol-function 'shell-command-to-string)
              (lambda (cmd) (funcall ,shell-cmd-to-str-fn cmd))))
     ,@body))

(defvar reconcile-test-magit-calls nil
  "List of directories passed to magit-status during tests.")

(defmacro reconcile-test-with-magit-mock (&rest body)
  "Run BODY with `magit-status' mocked to record calls."
  (declare (indent 0))
  `(let ((reconcile-test-magit-calls nil))
     (cl-letf (((symbol-function 'magit-status)
                (lambda (dir &rest _) (push dir reconcile-test-magit-calls))))
       ,@body)))

(provide 'testutil-reconcile-open-repos)
;;; testutil-reconcile-open-repos.el ends here
