;;; test-dev-fkeys--projectile-revert-on-fail.el --- Tests for projectile revert decisions -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for reverting Projectile's per-project cache when a build/test failed
;; AND the command was modified. Test-fails-because-of-real-bug (cmd unchanged)
;; leaves the cache alone.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defvar projectile-compile-cmd-map nil)

;;; Normal Cases

(ert-deftest test-dev-fkeys-projectile-revert-state-on-fail-failure-and-modified-reverts ()
  "Normal: failure status + cmd modified from prior → revert to prior."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior "make build")))
    (puthash "/p/" "make buidl" projectile-compile-cmd-map)
    (cj/--projectile-revert-state-on-fail
     state "exited abnormally with code 2\n")
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "make build"))))

(ert-deftest test-dev-fkeys-projectile-revert-state-on-fail-success-leaves-cache ()
  "Normal: success status → no revert, cache keeps the modified cmd."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior "make build")))
    (puthash "/p/" "make build-fast" projectile-compile-cmd-map)
    (cj/--projectile-revert-state-on-fail state "finished\n")
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "make build-fast"))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-projectile-revert-state-on-fail-failure-but-unchanged-no-revert ()
  "Boundary: failure status + cmd unchanged from prior → no revert.
This is the test-fails-because-of-real-bug case. Don't flap the cache.

Components integrated:
- `cj/--projectile-revert-state-on-fail' (unit under test)
- `projectile-compile-cmd-map' (test stub)"
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior "pytest")))
    (puthash "/p/" "pytest" projectile-compile-cmd-map)
    (cj/--projectile-revert-state-on-fail
     state "exited abnormally with code 1\n")
    ;; Cache value still equals the prior value (unchanged through the run).
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "pytest"))))

(ert-deftest test-dev-fkeys-projectile-revert-state-on-fail-failure-with-nil-prior-no-revert ()
  "Boundary: failure with no prior cmd cached → don't store nil.
A nil prior means projectile cached for the first time on this run; even
if it failed, there's nothing to revert to."
  (let* ((projectile-compile-cmd-map (make-hash-table :test 'equal))
         (state
          (list :map 'projectile-compile-cmd-map
                :root "/p/"
                :prior nil)))
    (puthash "/p/" "broken-cmd" projectile-compile-cmd-map)
    (cj/--projectile-revert-state-on-fail state "exited abnormally\n")
    (should (string= (gethash "/p/" projectile-compile-cmd-map) "broken-cmd"))))

;;; Error Cases

(ert-deftest test-dev-fkeys-projectile-revert-state-on-fail-nil-state-is-noop ()
  "Error: nil state (capture didn't run) → hook is a no-op, no error."
  (should-not (cj/--projectile-revert-state-on-fail
               nil "exited abnormally\n")))

(provide 'test-dev-fkeys--projectile-revert-on-fail)
;;; test-dev-fkeys--projectile-revert-on-fail.el ends here
