;;; test-vc-config--git-clone.el --- Tests for clipboard git-clone hardening -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--git-clone-dir-name (robust repo-dir derivation across
;; HTTPS, scp-style SSH, ssh:// and local URLs), for the async clone process
;; wiring in cj/git-clone-clipboard-url (make-process argv, no shell), and
;; for the sentinel built by cj/--git-clone-make-sentinel (open on success,
;; surface the process buffer on failure).  Sentinel tests drive real
;; short-lived processes rather than mocking process primitives.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'vc-config)

(defun test-vc-clone--run-sentinel (command sentinel buffer)
  "Run COMMAND with SENTINEL and BUFFER; wait for process exit."
  (let ((proc (make-process :name "test-vc-clone"
                            :buffer buffer
                            :command command
                            :sentinel sentinel)))
    (while (process-live-p proc)
      (accept-process-output proc 0.05))
    ;; Give the sentinel a chance to run after exit.
    (accept-process-output nil 0.05)
    proc))

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

;;; cj/--git-clone-dir-name — Boundary Cases

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

;;; cj/--git-clone-open — Normal / Boundary Cases

(ert-deftest test-vc-git-clone-open-finds-readme ()
  "Normal: a README in the clone is opened."
  (let ((dir (make-temp-file "cj-clone-open-" t))
        (opened nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "README.md" dir) (insert "hi"))
          (cl-letf (((symbol-function 'find-file)
                     (lambda (f &rest _) (setq opened f)))
                    ((symbol-function 'dired) #'ignore))
            (cj/--git-clone-open dir)
            (should (equal (expand-file-name "README.md" dir) opened))))
      (delete-directory dir t))))

(ert-deftest test-vc-git-clone-open-no-readme-dires ()
  "Boundary: with no README, the clone directory is dired."
  (let ((dir (make-temp-file "cj-clone-open-" t))
        (dired-dir nil))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file)
                   (lambda (&rest _) (error "find-file should not run")))
                  ((symbol-function 'dired)
                   (lambda (d &rest _) (setq dired-dir d))))
          (cj/--git-clone-open dir)
          (should (equal dir dired-dir)))
      (delete-directory dir t))))

;;; cj/--git-clone-make-sentinel — Normal / Error Cases

(ert-deftest test-vc-git-clone-sentinel-success-opens-clone ()
  "Normal: a zero-exit clone process opens the clone directory."
  (let ((buffer (generate-new-buffer " *test-clone-ok*"))
        (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--git-clone-open)
                   (lambda (d) (setq opened d)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (test-vc-clone--run-sentinel
           '("true") (cj/--git-clone-make-sentinel "url" "/tmp/clone-dst") buffer)
          (should (equal "/tmp/clone-dst" opened)))
      (kill-buffer buffer))))

(ert-deftest test-vc-git-clone-sentinel-failure-pops-process-buffer ()
  "Error: a nonzero exit surfaces the process buffer, never opens the clone."
  (let ((buffer (generate-new-buffer " *test-clone-fail*"))
        (opened nil)
        (popped nil))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--git-clone-open)
                   (lambda (d) (setq opened d)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (b &rest _) (setq popped b)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (test-vc-clone--run-sentinel
           '("false") (cj/--git-clone-make-sentinel "url" "/tmp/clone-dst") buffer)
          (should-not opened)
          (should (eq buffer popped)))
      (kill-buffer buffer))))

;;; cj/git-clone-clipboard-url — Normal / Error Cases

(ert-deftest test-vc-git-clone-clipboard-url-spawns-async-argv ()
  "Normal: the clone runs as an async process with a plain argv, no shell.
The `--' separator must precede the URL so a leading-dash URL cannot be
read as a git flag."
  (let ((target (make-temp-file "cj-clone-async-" t))
        (spawned nil))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq spawned (plist-get args :command))
                     nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (cj/git-clone-clipboard-url "https://example.com/user/repo.git" target)
          (should (equal (list "git" "clone" "--"
                               "https://example.com/user/repo.git"
                               (expand-file-name "repo" target))
                         spawned)))
      (delete-directory target t))))

(ert-deftest test-vc-git-clone-clipboard-url-empty-clipboard-errors ()
  "Error: an empty clipboard URL aborts before any clone attempt."
  (let ((spawned nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (setq spawned t) nil)))
      (should-error (cj/git-clone-clipboard-url "   " "/tmp") :type 'user-error))
    (should-not spawned)))

(ert-deftest test-vc-git-clone-clipboard-url-existing-destination-errors ()
  "Error: an existing clone destination aborts before any clone attempt."
  (let ((target (make-temp-file "cj-clone-exists-" t))
        (spawned nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "repo" target))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest _) (setq spawned t) nil)))
            (should-error
             (cj/git-clone-clipboard-url "https://example.com/user/repo.git" target)
             :type 'user-error))
          (should-not spawned))
      (delete-directory target t))))

(provide 'test-vc-config--git-clone)
;;; test-vc-config--git-clone.el ends here
