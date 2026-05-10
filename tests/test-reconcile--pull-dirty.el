;;; test-reconcile--pull-dirty.el --- Tests for dirty repo review handling -*- lexical-binding: t; -*-

;;; Commentary:
;; Dirty repositories should be review-first: no stash, pull, or stash-pop.

;;; Code:

(require 'ert)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

(ert-deftest test-pull-dirty-normal-opens-magit-for-review ()
  "Dirty repo handling opens Magit and returns a needs-review result."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-magit-mock
       (let ((result (cj/reconcile--pull-dirty dir)))
         (should (member dir reconcile-test-magit-calls))
         (should (eq (plist-get result :status) 'needs-review))
         (should (equal (plist-get result :directory) dir)))))))

(ert-deftest test-pull-dirty-normal-does-not-run-git-commands ()
  "Dirty repo handling must not mutate the worktree with git commands."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-magit-mock
       (reconcile-test-with-git-mock
           (lambda (_args)
             (ert-fail "Dirty repo handler should not run git commands"))
         (cj/reconcile--pull-dirty dir))
       (should (member dir reconcile-test-magit-calls))))))

(ert-deftest test-pull-dirty-boundary-uncommitted-work-message ()
  "Dirty repo handling announces review instead of auto-reconciling."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-magit-mock
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args)
                    (push (apply #'format fmt args) messages))))
         (cj/reconcile--pull-dirty dir)))
     (should (cl-some (lambda (m)
                        (string-match-p "opening Magit for review" m))
                      messages)))))

(provide 'test-reconcile--pull-dirty)
;;; test-reconcile--pull-dirty.el ends here
