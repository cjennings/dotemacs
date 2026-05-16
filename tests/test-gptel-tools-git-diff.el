;;; test-gptel-tools-git-diff.el --- Tests for git_diff gptel tool -*- lexical-binding: t; -*-

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

(require 'git_diff)

;; ---------- helpers

(defun test-gptel-tools-git-diff--with-repo (fn)
  "Create a temp git repo under HOME with one committed file, call FN."
  (let* ((name (format ".test-gptel-tools-git-diff-%s"
                       (format-time-string "%s%N")))
         (dir (expand-file-name name "~")))
    (unwind-protect
        (progn
          (make-directory dir)
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (call-process "git" nil nil nil "config" "user.email" "test@x")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "f.txt" dir)
              (insert "original\n"))
            (call-process "git" nil nil nil "add" "f.txt")
            (call-process "git" nil nil nil "commit" "--quiet" "-m" "initial"))
          (funcall fn dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

;; ---------- build-args

(ert-deftest test-gptel-tools-git-diff-build-args-no-refs ()
  "Normal: no refs / no file → bare diff args."
  (should (equal (cj/gptel-git-diff--build-args nil nil nil)
                 '("-c" "color.ui=false" "diff"))))

(ert-deftest test-gptel-tools-git-diff-build-args-with-ref1 ()
  "Normal: REF1 appended."
  (should (equal (cj/gptel-git-diff--build-args "HEAD~1" nil nil)
                 '("-c" "color.ui=false" "diff" "HEAD~1"))))

(ert-deftest test-gptel-tools-git-diff-build-args-with-both-refs ()
  "Normal: REF1 and REF2 both appended."
  (should (equal (cj/gptel-git-diff--build-args "HEAD~1" "HEAD" nil)
                 '("-c" "color.ui=false" "diff" "HEAD~1" "HEAD"))))

(ert-deftest test-gptel-tools-git-diff-build-args-with-file ()
  "Normal: FILE appended after `--'."
  (should (equal (cj/gptel-git-diff--build-args nil nil "foo.txt")
                 '("-c" "color.ui=false" "diff" "--" "foo.txt"))))

(ert-deftest test-gptel-tools-git-diff-build-args-boundary-empty-strings ()
  "Boundary: empty-string REF/FILE values are ignored."
  (should (equal (cj/gptel-git-diff--build-args "" "" "")
                 '("-c" "color.ui=false" "diff"))))

;; ---------- truncate

(ert-deftest test-gptel-tools-git-diff-truncate-under-cap ()
  "Normal: short input returns unchanged."
  (should (equal (cj/gptel-git-diff--truncate "small diff") "small diff")))

(ert-deftest test-gptel-tools-git-diff-truncate-over-cap ()
  "Boundary: output exceeding the cap is truncated with a marker."
  (let* ((cap cj/gptel-git-diff--max-output-bytes)
         (huge (make-string (+ cap 1000) ?x))
         (out (cj/gptel-git-diff--truncate huge)))
    (should (string-match-p "\\[truncated:" out))
    (should (> (length huge) (length out)))))

;; ---------- validate-path

(ert-deftest test-gptel-tools-git-diff-validate-path-normal ()
  "Normal: validator accepts a git working tree."
  (test-gptel-tools-git-diff--with-repo
   (lambda (dir)
     (should (equal (cj/gptel-git-diff--validate-path dir) dir)))))

(ert-deftest test-gptel-tools-git-diff-validate-path-error-outside-home ()
  "Error: path outside HOME signals."
  (should-error (cj/gptel-git-diff--validate-path "/etc")))

(ert-deftest test-gptel-tools-git-diff-validate-path-error-not-a-repo ()
  "Error: non-git directory signals."
  (let ((dir (make-temp-file
              (expand-file-name ".test-gptel-tools-git-diff-" "~") t)))
    (unwind-protect
        (should-error (cj/gptel-git-diff--validate-path dir))
      (when (file-exists-p dir) (delete-directory dir t)))))

;; ---------- run

(ert-deftest test-gptel-tools-git-diff-run-no-changes ()
  "Boundary: a clean tree with no refs returns the no-diff marker."
  (test-gptel-tools-git-diff--with-repo
   (lambda (dir)
     (let ((out (cj/gptel-git-diff--run dir)))
       (should (string-match-p "No diff" out))))))

(ert-deftest test-gptel-tools-git-diff-run-unstaged-change ()
  "Normal: an unstaged edit appears as a real diff."
  (test-gptel-tools-git-diff--with-repo
   (lambda (dir)
     (with-temp-file (expand-file-name "f.txt" dir)
       (insert "changed\n"))
     (let ((out (cj/gptel-git-diff--run dir)))
       (should (string-match-p "^-original" out))
       (should (string-match-p "^\\+changed" out))))))

(ert-deftest test-gptel-tools-git-diff-run-narrow-to-file ()
  "Normal: FILE argument narrows the diff."
  (test-gptel-tools-git-diff--with-repo
   (lambda (dir)
     (with-temp-file (expand-file-name "f.txt" dir)
       (insert "changed\n"))
     (with-temp-file (expand-file-name "g.txt" dir)
       (insert "second file\n"))
     (let ((out (cj/gptel-git-diff--run dir nil nil "f.txt")))
       (should (string-match-p "f.txt" out))
       (should-not (string-match-p "g.txt" out))))))

(provide 'test-gptel-tools-git-diff)
;;; test-gptel-tools-git-diff.el ends here
