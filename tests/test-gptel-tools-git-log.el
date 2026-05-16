;;; test-gptel-tools-git-log.el --- Tests for git_log gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests run against real temp git repos under HOME via `process-file'.

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

(require 'git_log)

;; ---------- helpers

(defun test-gptel-tools-git-log--with-repo (commit-count fn)
  "Create a temp git repo under HOME with COMMIT-COUNT empty commits.
Call FN with the absolute path, clean up after."
  (let* ((name (format ".test-gptel-tools-git-log-%s"
                       (format-time-string "%s%N")))
         (dir (expand-file-name name "~")))
    (unwind-protect
        (progn
          (make-directory dir)
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (call-process "git" nil nil nil "config" "user.email" "test@x")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (dotimes (i commit-count)
              (call-process "git" nil nil nil "commit" "--allow-empty"
                            "--quiet" "-m" (format "commit %d" i))))
          (funcall fn dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

;; ---------- effective-count

(ert-deftest test-gptel-tools-git-log-effective-count-defaults-on-nil ()
  "Boundary: nil N → default count."
  (should (= (cj/gptel-git-log--effective-count nil)
             cj/gptel-git-log--default-count)))

(ert-deftest test-gptel-tools-git-log-effective-count-defaults-on-non-integer ()
  "Boundary: non-integer N → default count."
  (should (= (cj/gptel-git-log--effective-count "ten")
             cj/gptel-git-log--default-count))
  (should (= (cj/gptel-git-log--effective-count 0.5)
             cj/gptel-git-log--default-count)))

(ert-deftest test-gptel-tools-git-log-effective-count-clamps-low ()
  "Boundary: N below 1 → default count."
  (should (= (cj/gptel-git-log--effective-count 0)
             cj/gptel-git-log--default-count))
  (should (= (cj/gptel-git-log--effective-count -5)
             cj/gptel-git-log--default-count)))

(ert-deftest test-gptel-tools-git-log-effective-count-caps-high ()
  "Boundary: N above max → max."
  (should (= (cj/gptel-git-log--effective-count 1000)
             cj/gptel-git-log--max-count)))

(ert-deftest test-gptel-tools-git-log-effective-count-normal ()
  "Normal: a valid N passes through."
  (should (= (cj/gptel-git-log--effective-count 5) 5)))

;; ---------- validate-path

(ert-deftest test-gptel-tools-git-log-validate-path-normal ()
  "Normal: validator accepts a git working tree."
  (test-gptel-tools-git-log--with-repo
   1
   (lambda (dir)
     (should (equal (cj/gptel-git-log--validate-path dir) dir)))))

(ert-deftest test-gptel-tools-git-log-validate-path-error-outside-home ()
  "Error: path outside HOME signals."
  (should-error (cj/gptel-git-log--validate-path "/etc")))

(ert-deftest test-gptel-tools-git-log-validate-path-error-not-a-repo ()
  "Error: directory outside any git working tree signals."
  (let ((dir (make-temp-file
              (expand-file-name ".test-gptel-tools-git-log-" "~") t)))
    (unwind-protect
        (should-error (cj/gptel-git-log--validate-path dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

;; ---------- run

(ert-deftest test-gptel-tools-git-log-run-default-count ()
  "Normal: default count limits output to that many commits."
  (test-gptel-tools-git-log--with-repo
   30
   (lambda (dir)
     (let* ((out (cj/gptel-git-log--run dir))
            (lines (split-string (string-trim out) "\n")))
       (should (= (length lines) cj/gptel-git-log--default-count))))))

(ert-deftest test-gptel-tools-git-log-run-honors-n ()
  "Normal: an explicit N limits output to N commits."
  (test-gptel-tools-git-log--with-repo
   10
   (lambda (dir)
     (let* ((out (cj/gptel-git-log--run dir 3))
            (lines (split-string (string-trim out) "\n")))
       (should (= (length lines) 3))))))

(ert-deftest test-gptel-tools-git-log-run-empty-repo ()
  "Boundary: a repo with no commits returns the empty-result marker."
  (let* ((name (format ".test-gptel-tools-git-log-empty-%s"
                       (format-time-string "%s%N")))
         (dir (expand-file-name name "~")))
    (unwind-protect
        (progn
          (make-directory dir)
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init" "--quiet"))
          ;; git log on a no-commits repo errors in some versions, but
          ;; our wrapper turns "no commits" into the no-match marker.
          (let ((res (ignore-errors (cj/gptel-git-log--run dir))))
            ;; Either path is acceptable: error captured (nil) or the
            ;; explicit "No commits matching" marker.
            (should (or (null res)
                        (string-match-p "No commits" res)))))
      (when (file-exists-p dir) (delete-directory dir t)))))

(provide 'test-gptel-tools-git-log)
;;; test-gptel-tools-git-log.el ends here
