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
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "git@github.com:user/repo.git\n")
            '(:exit 0 :output "")))
      (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-http-remote-skipped ()
  "HTTP remote repo should be skipped (reference clone)."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "http://github.com/user/repo.git\n")
            '(:exit 0 :output "")))
      (should (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-https-remote-skipped ()
  "HTTPS remote repo should be skipped (reference clone)."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "https://github.com/user/repo.git\n")
            '(:exit 0 :output "")))
      (should (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-https-remote-not-skipped-when-policy-disabled ()
  "HTTPS remote repos can be included by disabling the skip regexp."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root))
         (cj/reconcile-skipped-remote-regexp nil))
     (reconcile-test-with-git-mock
         (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "https://github.com/user/repo.git\n")
            '(:exit 0 :output "")))
      (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-normal-no-remote-skipped ()
  "Local-only repo (no remote) should be skipped."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
    (reconcile-test-with-git-mock
        (lambda (_args) '(:exit 1 :output ""))
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
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "user@myserver.com:repos/project.git\n")
            '(:exit 0 :output "")))
      (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-boundary-ssh-protocol-url-not-skipped ()
  "ssh:// protocol URL should NOT be skipped."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "ssh://git@github.com/user/repo.git\n")
            '(:exit 0 :output "")))
      (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-boundary-file-protocol-remote-not-skipped ()
  "file:// remote repo should NOT be skipped by the default policy."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "file:///srv/git/repo.git\n")
            '(:exit 0 :output "")))
      (should-not (cj/reconcile--should-skip-p dir))))))

(ert-deftest test-should-skip-p-boundary-local-path-remote-not-skipped ()
  "Plain local path remote should NOT be skipped by the default policy."
  (reconcile-test-with-temp-dirs
   ("repo/.git/")
   (let ((dir (expand-file-name "repo" test-root)))
    (reconcile-test-with-git-mock
        (lambda (args)
          (if (equal args '("config" "--get" "remote.origin.url"))
              '(:exit 0 :output "/srv/git/repo.git\n")
            '(:exit 0 :output "")))
      (should-not (cj/reconcile--should-skip-p dir))))))

(provide 'test-reconcile--should-skip-p)
;;; test-reconcile--should-skip-p.el ends here
