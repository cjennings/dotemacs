;;; test-vc-config--git-clone.el --- Tests for clipboard git-clone hardening -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--git-clone-dir-name (robust repo-dir derivation across
;; HTTPS, scp-style SSH, ssh:// and local URLs) and for cj/git-clone-clipboard-url
;; reporting a failed clone from the process exit status instead of silently
;; assuming the directory appeared.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'vc-config)

;;; cj/--git-clone-dir-name — Normal Cases

(ert-deftest test-vc-git-clone-dir-name-https-with-git-suffix ()
  "Normal: an HTTPS URL with a .git suffix yields the bare repo name."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "https://example.com/user/repo.git"))))

(ert-deftest test-vc-git-clone-dir-name-https-without-git-suffix ()
  "Normal: an HTTPS URL without a .git suffix yields the bare repo name."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "https://example.com/user/repo"))))

(ert-deftest test-vc-git-clone-dir-name-ssh-scp-with-user ()
  "Normal: scp-style SSH with a user path yields the repo name."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "git@example.com:user/repo.git"))))

(ert-deftest test-vc-git-clone-dir-name-ssh-url-scheme ()
  "Normal: an ssh:// URL yields the repo name."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "ssh://git@example.com/user/repo.git"))))

;;; Boundary Cases

(ert-deftest test-vc-git-clone-dir-name-ssh-scp-without-user ()
  "Boundary: scp-style SSH with no user path (host:repo.git) still works.
This is the case the old file-name-nondirectory derivation got wrong,
since there is no `/' separator."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "git@example.com:repo.git"))))

(ert-deftest test-vc-git-clone-dir-name-local-path ()
  "Boundary: a local filesystem path yields the repo name."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "/home/me/src/repo.git"))))

(ert-deftest test-vc-git-clone-dir-name-trailing-slash ()
  "Boundary: a trailing slash does not swallow the repo name."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "https://example.com/user/repo.git/"))))

(ert-deftest test-vc-git-clone-dir-name-surrounding-whitespace ()
  "Boundary: clipboard whitespace around the URL is trimmed."
  (should (equal "repo"
                 (cj/--git-clone-dir-name "  https://example.com/user/repo.git\n"))))

;;; cj/git-clone-clipboard-url — Error Cases

(ert-deftest test-vc-git-clone-clipboard-url-reports-clone-failure ()
  "Error: a nonzero git exit status surfaces a user-error, not silence.
Uses a real writable temp dir as the target (so the file predicates run
for real) and mocks only the clone process to fail."
  (let ((target (make-temp-file "cj-clone-fail-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 128))
                  ((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (should-error
           (cj/git-clone-clipboard-url "https://example.com/user/repo.git" target)
           :type 'user-error))
      (delete-directory target t))))

(ert-deftest test-vc-git-clone-clipboard-url-empty-clipboard-errors ()
  "Error: an empty clipboard URL aborts before any clone attempt."
  (let ((cloned nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (setq cloned t) 0)))
      (should-error (cj/git-clone-clipboard-url "   " "/tmp") :type 'user-error))
    (should-not cloned)))

(provide 'test-vc-config--git-clone)
;;; test-vc-config--git-clone.el ends here
