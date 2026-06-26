;;; test-eshell-config--prompt.el --- Tests for eshell prompt helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the pure prompt-segment helpers added for zsh parity: the
;; .git/HEAD branch reader and the exit-status segment.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eshell-config)

(defvar eshell-last-command-status)  ; declared special for the status tests

;;; cj/--eshell-git-branch

(ert-deftest test-eshell-git-branch-reads-head ()
  "Normal: a .git/HEAD pointing at a branch returns the branch name."
  (let ((dir (make-temp-file "esh-git-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (with-temp-file (expand-file-name ".git/HEAD" dir)
            (insert "ref: refs/heads/feature-x\n"))
          (let ((default-directory (file-name-as-directory dir)))
            (should (equal (cj/--eshell-git-branch) "feature-x"))))
      (delete-directory dir t))))

(ert-deftest test-eshell-git-branch-no-repo-nil ()
  "Boundary: a directory with no .git returns nil."
  (let ((dir (make-temp-file "esh-nogit-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (should-not (cj/--eshell-git-branch)))
      (delete-directory dir t))))

(ert-deftest test-eshell-git-branch-detached-nil ()
  "Boundary: a detached HEAD (a raw SHA, no ref) returns nil."
  (let ((dir (make-temp-file "esh-detached-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (with-temp-file (expand-file-name ".git/HEAD" dir)
            (insert "a1b2c3d4e5f6\n"))
          (let ((default-directory (file-name-as-directory dir)))
            (should-not (cj/--eshell-git-branch))))
      (delete-directory dir t))))

(ert-deftest test-eshell-git-branch-remote-skipped ()
  "Boundary: a remote default-directory is skipped (no TRAMP read)."
  (let ((default-directory "/ssh:host:/some/path/"))
    (should-not (cj/--eshell-git-branch))))

;;; cj/--eshell-prompt-status-segment

(ert-deftest test-eshell-prompt-status-zero-empty ()
  "Normal: a zero exit status yields an empty segment."
  (let ((eshell-last-command-status 0))
    (should (equal (cj/--eshell-prompt-status-segment) ""))))

(ert-deftest test-eshell-prompt-status-nonzero-bracketed ()
  "Normal: a non-zero exit status is shown in brackets."
  (let ((eshell-last-command-status 1))
    (should (equal (cj/--eshell-prompt-status-segment) " [1]")))
  (let ((eshell-last-command-status 130))
    (should (equal (cj/--eshell-prompt-status-segment) " [130]"))))

(ert-deftest test-eshell-prompt-status-unset-empty ()
  "Boundary: an unset status yields an empty segment, no error."
  (let ((eshell-last-command-status nil))
    (should (equal (cj/--eshell-prompt-status-segment) ""))))

(provide 'test-eshell-config--prompt)
;;; test-eshell-config--prompt.el ends here
