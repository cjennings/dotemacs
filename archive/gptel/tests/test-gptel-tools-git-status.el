;;; test-gptel-tools-git-status.el --- Tests for git_status gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests run against real temp git repos under HOME via `process-file'.
;; The tool is read-only so repos are torn down per test.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "gptel-tools" user-emacs-directory))
  (setq load-prefer-newer t)
  (unless (featurep 'gptel)
    (defvar gptel-tools nil)
    (defun gptel-make-tool (&rest _args) nil)
    (defun gptel-get-tool (&rest _args) nil)
    (provide 'gptel)))

(require 'git_status)

;; ---------- helpers

(defun test-gptel-tools-git-status--with-repo (fn)
  "Create a temp git repo under HOME, call FN with its absolute path, clean up."
  (let* ((name (format ".test-gptel-tools-git-status-%s"
                       (format-time-string "%s%N")))
         (dir (expand-file-name name "~")))
    (unwind-protect
        (progn
          (make-directory dir)
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (call-process "git" nil nil nil "config" "user.email" "test@x")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (call-process "git" nil nil nil "commit" "--allow-empty"
                          "--quiet" "-m" "initial"))
          (funcall fn dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

;; ---------- validate-path

(ert-deftest test-gptel-tools-git-status-validate-path-normal ()
  "Normal: validator accepts a directory inside a git working tree."
  (test-gptel-tools-git-status--with-repo
   (lambda (dir)
     (should (equal (cj/gptel-git-status--validate-path dir) dir)))))

(ert-deftest test-gptel-tools-git-status-validate-path-error-outside-home ()
  "Error: path outside HOME signals."
  (should-error (cj/gptel-git-status--validate-path "/etc")))

(ert-deftest test-gptel-tools-git-status-validate-path-error-not-a-directory ()
  "Error: path that's not a directory signals."
  (let ((file (make-temp-file
               (expand-file-name ".test-gptel-tools-git-status-" "~"))))
    (unwind-protect
        (should-error (cj/gptel-git-status--validate-path file))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest test-gptel-tools-git-status-validate-path-error-not-a-repo ()
  "Error: directory outside any git working tree signals."
  (let ((dir (make-temp-file
              (expand-file-name ".test-gptel-tools-git-status-" "~") t)))
    (unwind-protect
        (should-error (cj/gptel-git-status--validate-path dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

(ert-deftest test-gptel-tools-git-status-validate-path-error-symlink-outside-home ()
  "Error: symlinked directories resolving outside HOME are rejected."
  (let ((link (expand-file-name
               (format ".test-gptel-tools-git-status-link-%s"
                       (format-time-string "%s%N"))
               "~")))
    (unwind-protect
        (progn
          (make-symbolic-link "/tmp" link t)
          (should-error (cj/gptel-git-status--validate-path link)))
      (when (file-symlink-p link) (delete-file link)))))

;; ---------- run

(ert-deftest test-gptel-tools-git-status-run-clean-tree ()
  "Normal: a clean repo returns the clean-tree marker."
  (test-gptel-tools-git-status--with-repo
   (lambda (dir)
     (let ((out (cj/gptel-git-status--run dir)))
       (should (string-match-p "Clean working tree" out))))))

(ert-deftest test-gptel-tools-git-status-run-dirty-tree-includes-file ()
  "Normal: an untracked file appears in the output."
  (test-gptel-tools-git-status--with-repo
   (lambda (dir)
     (with-temp-file (expand-file-name "new.txt" dir) (insert "x"))
     (let ((out (cj/gptel-git-status--run dir)))
       (should (string-match-p "new.txt" out))
       (should (string-match-p "^\\?\\?" out))))))

(ert-deftest test-gptel-tools-git-status-run-includes-branch ()
  "Normal: the `--branch' line surfaces in the output."
  (test-gptel-tools-git-status--with-repo
   (lambda (dir)
     (with-temp-file (expand-file-name "f.txt" dir) (insert "x"))
     (let ((out (cj/gptel-git-status--run dir)))
       (should (string-match-p "^## " out))))))

(ert-deftest test-gptel-tools-git-status-run-error-on-git-status-failure ()
  "Error: non-zero git status exits are surfaced."
  (test-gptel-tools-git-status--with-repo
   (lambda (dir)
     (cl-letf (((symbol-function 'process-file)
                (lambda (program infile destination display &rest args)
                  (if (member "status" args)
                      (progn
                        (when (bufferp destination)
                          (with-current-buffer destination (insert "bad status")))
                        2)
                    (apply #'call-process program infile destination display args)))))
       (should-error (cj/gptel-git-status--run dir))))))

(provide 'test-gptel-tools-git-status)
;;; test-gptel-tools-git-status.el ends here
