;;; test-reconcile--should-skip-p.el --- Tests for cj/reconcile--should-skip-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the skip predicate that filters out non-git dirs, local-only repos,
;; and http/https reference clones.

;;; Code:

(require 'ert)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

;;; Normal Cases

(ert-deftest test-should-skip-p-normal-ssh-remote-not-skipped ()
  "SSH remote repo should NOT be skipped."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd)
               "git@github.com:user/repo.git"
             ""))
       (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-http-remote-skipped ()
  "HTTP remote repo should be skipped (reference clone)."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd)
               "http://github.com/user/repo.git"
             ""))
       (should (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-https-remote-skipped ()
  "HTTPS remote repo should be skipped (reference clone)."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd)
               "https://github.com/user/repo.git"
             ""))
       (should (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-no-remote-skipped ()
  "Local-only repo (no remote) should be skipped."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd) "" ""))
       (should (cj/reconcile--should-skip-p dir))))))

;;; Boundary Cases

(ert-deftest test-should-skip-p-boundary-no-git-dir ()
  "Directory without .git should be skipped."
  (reconcile-test-with-temp-dirs
   ("not-a-repo/readme.txt")
   (let ((dir (expand-file-name "not-a-repo" test-root)))
     (should (cj/reconcile--should-skip-p dir)))))

(ert-deftest test-should-skip-p-boundary-scp-style-remote-not-skipped ()
  "SCP-style remote (user@host:path) should NOT be skipped."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd)
               "user@myserver.com:repos/project.git"
             ""))
       (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-boundary-ssh-protocol-url-not-skipped ()
  "ssh:// protocol URL should NOT be skipped."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
     (reconcile-test-with-shell-mocks
         (lambda (_cmd) 0)
         (lambda (cmd)
           (if (string-match-p "remote.origin.url" cmd)
               "ssh://git@github.com/user/repo.git"
             ""))
       (should-not (cj/reconcile--should-skip-p dir))))))

(provide 'test-reconcile--should-skip-p)
;;; test-reconcile--should-skip-p.el ends here
