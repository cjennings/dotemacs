;;; test-dev-fkeys--projectile-capture-cmd.el --- Tests for cj/--projectile-capture-cmd -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the prior-cmd capture helper used by the auto-revert advice.
;; Captures the current cached cmd at the project root into a plist so a
;; later finish-hook can restore it if the compile fails after the cmd was
;; modified.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;; Test stub: projectile cmd-maps as defvars so `boundp' is non-nil and
;; `let'-binding has a target. In real use, projectile defines these.
(defvar projectile-compile-cmd-map nil)
(defvar projectile-test-cmd-map nil)
(defvar projectile-run-cmd-map nil)

;;; Normal Cases

(ert-deftest test-dev-fkeys-projectile-capture-cmd-stores-prior-value ()
  "Normal: captures the cached cmd at the project root into the state plist."
  (let* ((cj/--projectile-revert-state nil)
         (projectile-compile-cmd-map (make-hash-table :test 'equal))
         state)
    (puthash "/p/" "make build" projectile-compile-cmd-map)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (setq state (cj/--projectile-capture-cmd 'projectile-compile-cmd-map)))
    (should (equal (plist-get state :map)
                   'projectile-compile-cmd-map))
    (should (equal (plist-get state :root) "/p/"))
    (should (equal (plist-get state :prior) "make build"))
    (should (null cj/--projectile-revert-state))))

(ert-deftest test-dev-fkeys-projectile-capture-cmd-no-prior-stores-nil ()
  "Normal: when no cmd is cached, captures :prior nil — distinct from
\"didn't capture at all\" because :map and :root are still set."
  (let* ((cj/--projectile-revert-state nil)
         (projectile-test-cmd-map (make-hash-table :test 'equal))
         state)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (setq state (cj/--projectile-capture-cmd 'projectile-test-cmd-map)))
    (should (eq (plist-get state :map)
                'projectile-test-cmd-map))
    (should (null (plist-get state :prior)))
    (should (null cj/--projectile-revert-state))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-projectile-capture-cmd-nil-root-leaves-state-nil ()
  "Boundary: when no project root resolves, state stays nil so the
finish hook treats it as a no-op."
  (let ((cj/--projectile-revert-state nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () nil)))
      (cj/--projectile-capture-cmd 'projectile-compile-cmd-map))
    (should (null cj/--projectile-revert-state))))

;;; Error Cases

(ert-deftest test-dev-fkeys-projectile-capture-cmd-unbound-map-leaves-state-nil ()
  "Error: when the cmd-map symbol is unbound (projectile not loaded),
state stays nil and no error is raised."
  (let ((cj/--projectile-revert-state nil))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      ;; Use a clearly-unbound symbol to simulate projectile-not-loaded.
      (cj/--projectile-capture-cmd 'cj-test--definitely-not-bound-xyzzy))
    (should (null cj/--projectile-revert-state))))

(provide 'test-dev-fkeys--projectile-capture-cmd)
;;; test-dev-fkeys--projectile-capture-cmd.el ends here
