;;; test-system-defaults.el --- Smoke tests for system-defaults settings -*- lexical-binding: t; -*-

;;; Commentary:

;; system-defaults.el holds high-impact defaults with no behavior of its own
;; to unit-test, so these are characterization smoke tests on the *settings*
;; most likely to cause damage if they regressed silently: where Customize
;; writes land, where backups go, and whether the minibuffer GC hooks are
;; installed.  Load happens in the shared sandbox (testutil-system-defaults.el).
;;
;; The module's functions (cj/disabled, the GC hook bodies, unpropertize-kill-ring,
;; cj/log-comp-warning) are covered by test-system-defaults-functions.el, and the
;; vc-follow-symlinks default by test-system-defaults-vc-follow-symlinks.el.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-system-defaults)

;;; custom-file redirection

(ert-deftest test-system-defaults-custom-file-redirected-to-tempfile ()
  "Normal: custom-file points at a throwaway temp file, never the repo.
This is what stops accidental Customize writes from landing in tracked init."
  (test-system-defaults--with-load-environment
    (let ((custom-file nil))
      (test-system-defaults--load)
      (should (stringp custom-file))
      (should (string-prefix-p (file-name-as-directory
                                (expand-file-name temporary-file-directory))
                               (expand-file-name custom-file)))
      (should (string-match-p "emacs-customizations-trashbin-"
                              (file-name-nondirectory custom-file)))
      (should-not (string-prefix-p (expand-file-name user-emacs-directory)
                                   (expand-file-name custom-file))))))

;;; backup directory

(ert-deftest test-system-defaults-backups-redirected-under-user-emacs-dir ()
  "Normal: backups are redirected into user-emacs-directory/backups.
cj/backup-directory is a defvar that only recomputes when unbound, so the
test clears it first to capture the path derived from the sandbox."
  (test-system-defaults--with-load-environment
    (makunbound 'cj/backup-directory)
    (let ((backup-directory-alist nil))
      (test-system-defaults--load)
      (let ((dir (cdr (assoc "." backup-directory-alist))))
        (should dir)
        (should (string-prefix-p (expand-file-name user-emacs-directory)
                                 (expand-file-name dir)))
        (should (string-suffix-p "backups" (directory-file-name dir)))))))

;;; minibuffer GC hooks

(ert-deftest test-system-defaults-minibuffer-gc-hooks-registered ()
  "Normal: the minibuffer GC raise/restore hooks are installed.
Their bodies are tested in test-system-defaults-functions.el; this asserts
they are actually wired onto the minibuffer hooks."
  (test-system-defaults--with-load-environment
    (let ((minibuffer-setup-hook nil)
          (minibuffer-exit-hook nil))
      (test-system-defaults--load)
      (should (memq 'cj/minibuffer-setup-hook minibuffer-setup-hook))
      (should (memq 'cj/minibuffer-exit-hook minibuffer-exit-hook)))))

;;; Customize-save warning

(ert-deftest test-system-defaults-customize-save-warns-once ()
  "Normal: the first custom-save-all warns; the second does not (one-shot).
Customize writes land in a throwaway custom-file, so the user must be told
their edit will not persist.  The advice removes itself after warning once."
  (test-system-defaults--with-load-environment
    (let ((warnings '()))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (group &rest _) (push group warnings)))
                ((symbol-function 'custom-save-all) #'ignore))
        (test-system-defaults--load)
        ;; No warning merely from loading the module.
        (should (null warnings))
        (custom-save-all)
        (should (equal warnings '(cj/system-defaults)))
        ;; Second save must not warn again.
        (custom-save-all)
        (should (equal warnings '(cj/system-defaults)))))))

(provide 'test-system-defaults)
;;; test-system-defaults.el ends here
