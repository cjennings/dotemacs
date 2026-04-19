;;; test-reconcile--git-directory.el --- Tests for cj/reconcile-git-directory -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the top-level reconcile function that dispatches to skip/clean/dirty.

;;; Code:

(require 'ert)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

;;; Normal Cases

(ert-deftest test-reconcile-git-directory-normal-clean-repo-pulls ()
  "Clean SSH repo calls pull-clean, not pull-dirty."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (clean-called nil)
         (dirty-called nil))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (cond ((string-match-p "remote.origin.url" cmd) "git@host:repo.git")
                 ((string-match-p "status --porcelain" cmd) "")
                 (t "")))
       (cl-letf (((symbol-function 'cj/reconcile--pull-clean)
                  (lambda (_dir) (setq clean-called t)))
                 ((symbol-function 'cj/reconcile--pull-dirty)
                  (lambda (_dir) (setq dirty-called t)))
                 ((symbol-function 'message) (lambda (_fmt &rest _args))))
         (cj/reconcile-git-directory dir)))
     (should clean-called)
     (should-not dirty-called))))

(ert-deftest test-reconcile-git-directory-normal-dirty-repo-stashes ()
  "Dirty SSH repo calls pull-dirty, not pull-clean."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (clean-called nil)
         (dirty-called nil))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (cond ((string-match-p "remote.origin.url" cmd) "git@host:repo.git")
                 ((string-match-p "status --porcelain" cmd) " M file.el\n")
                 (t "")))
       (cl-letf (((symbol-function 'cj/reconcile--pull-clean)
                  (lambda (_dir) (setq clean-called t)))
                 ((symbol-function 'cj/reconcile--pull-dirty)
                  (lambda (_dir) (setq dirty-called t)))
                 ((symbol-function 'message) (lambda (_fmt &rest _args))))
         (cj/reconcile-git-directory dir)))
     (should-not clean-called)
     (should dirty-called))))

(ert-deftest test-reconcile-git-directory-normal-skipped-repo-no-calls ()
  "HTTP repo is skipped entirely — neither pull-clean nor pull-dirty called."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (clean-called nil)
         (dirty-called nil))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd)
               "https://github.com/user/repo.git"
             ""))
       (cl-letf (((symbol-function 'cj/reconcile--pull-clean)
                  (lambda (_dir) (setq clean-called t)))
                 ((symbol-function 'cj/reconcile--pull-dirty)
                  (lambda (_dir) (setq dirty-called t)))
                 ((symbol-function 'message) (lambda (_fmt &rest _args))))
         (cj/reconcile-git-directory dir)))
     (should-not clean-called)
     (should-not dirty-called))))

;;; Boundary Cases

(ert-deftest test-reconcile-git-directory-boundary-emits-checking-message ()
  "Always emits 'checking: <dir>' message, even for skipped repos."
  (reconcile-test-with-temp-dirs
   ("repo/readme.txt")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
       (cj/reconcile-git-directory dir))
     (should (cl-some (lambda (m) (string-match-p "checking:" m)) messages)))))

(provide 'test-reconcile--git-directory)
;;; test-reconcile--git-directory.el ends here
