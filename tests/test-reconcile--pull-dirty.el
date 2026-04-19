;;; test-reconcile--pull-dirty.el --- Tests for cj/reconcile--pull-dirty -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the dirty-repo reconciliation: stash, pull, pop, magit.

;;; Code:

(require 'ert)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

;;; Normal Cases

(ert-deftest test-pull-dirty-normal-stash-pull-pop-success ()
  "When stash, pull, and pop all succeed, magit is still opened."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-magit-mock
       (reconcile-test-with-shell-mocks
           (lambda (_cmd) 0)
           (lambda (_cmd) "")
         (cj/reconcile--pull-dirty dir))
       (should (member dir reconcile-test-magit-calls))))))

(ert-deftest test-pull-dirty-normal-stash-fails-opens-magit ()
  "When stash fails, magit is opened and warning emitted."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-magit-mock
       (reconcile-test-with-shell-mocks
           (lambda (cmd)
             (if (string-match-p "stash --quiet\\'" cmd) 1 0))
           (lambda (_cmd) "")
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
           (cj/reconcile--pull-dirty dir)))
       (should (member dir reconcile-test-magit-calls))
       (should (cl-some (lambda (m) (string-match-p "stash failed" m)) messages))))))

(ert-deftest test-pull-dirty-normal-pull-fails-warns ()
  "When stash succeeds but pull fails, warning mentions pull failure."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-magit-mock
       (reconcile-test-with-shell-mocks
           (lambda (cmd)
             (cond ((string-match-p "stash --quiet\\'" cmd) 0)
                   ((string-match-p "pull" cmd) 1)
                   (t 0)))
           (lambda (_cmd) "")
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
           (cj/reconcile--pull-dirty dir)))
       (should (cl-some (lambda (m) (string-match-p "git pull failed" m)) messages))))))

(ert-deftest test-pull-dirty-normal-stash-pop-fails-warns ()
  "When stash and pull succeed but pop fails, warning mentions stash pop."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-magit-mock
       (reconcile-test-with-shell-mocks
           (lambda (cmd)
             (cond ((string-match-p "stash pop" cmd) 1)
                   ((string-match-p "stash" cmd) 0)
                   (t 0)))
           (lambda (_cmd) "")
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
           (cj/reconcile--pull-dirty dir)))
       (should (cl-some (lambda (m) (string-match-p "stash pop failed" m)) messages))))))

;;; Boundary Cases

(ert-deftest test-pull-dirty-boundary-always-opens-magit ()
  "Magit is opened regardless of whether pull succeeds or fails."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     ;; Test with pull failure
     (reconcile-test-with-magit-mock
       (reconcile-test-with-shell-mocks
           (lambda (cmd)
             (if (string-match-p "pull" cmd) 1 0))
           (lambda (_cmd) "")
         (cl-letf (((symbol-function 'message) (lambda (_fmt &rest _args))))
           (cj/reconcile--pull-dirty dir)))
       (should (member dir reconcile-test-magit-calls))))))

(ert-deftest test-pull-dirty-boundary-uncommitted-work-message ()
  "Always emits 'contains uncommitted work' message."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-magit-mock
       (reconcile-test-with-shell-mocks
           (lambda (_cmd) 0)
           (lambda (_cmd) "")
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
           (cj/reconcile--pull-dirty dir)))
       (should (cl-some (lambda (m) (string-match-p "uncommitted work" m)) messages))))))

(provide 'test-reconcile--pull-dirty)
;;; test-reconcile--pull-dirty.el ends here
