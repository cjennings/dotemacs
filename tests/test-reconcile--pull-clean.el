;;; test-reconcile--pull-clean.el --- Tests for cj/reconcile--pull-clean -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for pulling latest changes on a clean git repository.

;;; Code:

(require 'ert)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

;;; Normal Cases

(ert-deftest test-pull-clean-normal-success ()
  "Successful pull produces no warning message."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (_cmd) "")
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
         (cj/reconcile--pull-clean dir)))
     (should-not (cl-some (lambda (m) (string-match-p "Warning" m)) messages)))))

(ert-deftest test-pull-clean-normal-failure-warns ()
  "Failed pull produces a warning message with directory and exit code."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 1)
         (lambda (_cmd) "")
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
         (cj/reconcile--pull-clean dir)))
     (should (cl-some (lambda (m) (string-match-p "Warning.*git pull failed" m)) messages))
     (should (cl-some (lambda (m) (string-match-p "exit code: 1" m)) messages)))))

;;; Boundary Cases

(ert-deftest test-pull-clean-boundary-nonzero-exit-128 ()
  "Exit code 128 (common git error) is reported in warning."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (messages nil))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 128)
         (lambda (_cmd) "")
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
         (cj/reconcile--pull-clean dir)))
     (should (cl-some (lambda (m) (string-match-p "exit code: 128" m)) messages)))))

(provide 'test-reconcile--pull-clean)
;;; test-reconcile--pull-clean.el ends here
