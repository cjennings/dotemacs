;;; test-dev-fkeys--projectile-revert-on-fail.el --- Tests for cj/--projectile-revert-on-fail -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the compilation-finish hook that reverts projectile's
;; per-project cache when a build/test failed AND the cmd was modified.
;; Test-fails-because-of-real-bug (cmd unchanged) leaves the cache alone.
;; The hook also self-removes from `compilation-finish-functions' on
;; first invocation and clears `cj/--projectile-revert-state'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defvar projectile-compile-cmd-map nil)

;;; Normal Cases

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-failure-and-modified-reverts ()
  "Normal: failure status + cmd modified from prior → revert to prior."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (cj/--projectile-revert-state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior "make build")))
    (puthash "/p/" "make buidl" projectile-compile-cmd-map)
    (cj/--projectile-revert-on-fail nil "exited abnormally with code 2\n")
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "make build"))))

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-success-leaves-cache ()
  "Normal: success status → no revert, cache keeps the modified cmd."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (cj/--projectile-revert-state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior "make build")))
    (puthash "/p/" "make build-fast" projectile-compile-cmd-map)
    (cj/--projectile-revert-on-fail nil "finished\n")
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "make build-fast"))))

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-clears-state ()
  "Normal: hook clears `cj/--projectile-revert-state' regardless of outcome."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (cj/--projectile-revert-state
          (list :map 'projectile-compile-cmd-map :root "/p/" :prior "x")))
    (cj/--projectile-revert-on-fail nil "finished\n")
    (should (null cj/--projectile-revert-state))))

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-removes-itself ()
  "Normal: hook removes itself from `compilation-finish-functions'."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (cj/--projectile-revert-state
          (list :map 'projectile-compile-cmd-map :root "/p/" :prior "x"))
         (compilation-finish-functions
          (list #'cj/--projectile-revert-on-fail)))
    (cj/--projectile-revert-on-fail nil "finished\n")
    (should-not (member #'cj/--projectile-revert-on-fail
                        compilation-finish-functions))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-failure-but-unchanged-no-revert ()
  "Boundary: failure status + cmd unchanged from prior → no revert.
This is the test-fails-because-of-real-bug case. Don't flap the cache.

Components integrated:
- `cj/--projectile-revert-on-fail' (unit under test)
- `cj/--projectile-revert-state' (real, scoped via let)
- `projectile-compile-cmd-map' (test stub)"
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (cj/--projectile-revert-state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior "pytest")))
    (puthash "/p/" "pytest" projectile-compile-cmd-map)
    (cj/--projectile-revert-on-fail nil "exited abnormally with code 1\n")
    ;; Cache value still equals the prior value (unchanged through the run).
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "pytest"))))

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-failure-with-nil-prior-no-revert ()
  "Boundary: failure with no prior cmd cached → don't store nil.
A nil prior means projectile cached for the first time on this run; even
if it failed, there's nothing to revert to."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (cj/--projectile-revert-state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior nil)))
    (puthash "/p/" "broken-cmd" projectile-compile-cmd-map)
    (cj/--projectile-revert-on-fail nil "exited abnormally\n")
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "broken-cmd"))))

;;; Error Cases

(ert-deftest test-dev-fkeys-projectile-revert-on-fail-nil-state-is-noop ()
  "Error: nil state (capture didn't run) → hook is a no-op, no error."
  (let ((cj/--projectile-revert-state nil))
    (cj/--projectile-revert-on-fail nil "exited abnormally\n")
    (should (null cj/--projectile-revert-state))))

(provide 'test-dev-fkeys--projectile-revert-on-fail)
;;; test-dev-fkeys--projectile-revert-on-fail.el ends here
