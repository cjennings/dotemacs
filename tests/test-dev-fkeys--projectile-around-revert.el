;;; test-dev-fkeys--projectile-around-revert.el --- Tests for cj/--projectile-around-revert -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the around-advice that wires the capture / finish-hook pair
;; to projectile cmd runners. The advice:
;;
;; 1. Captures the prior cached cmd via `cj/--projectile-capture-cmd'.
;; 2. Adds `cj/--projectile-revert-on-fail' to `compilation-finish-functions'.
;; 3. Calls ORIG-FN with ARGS so projectile's normal flow proceeds.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defvar projectile-compile-cmd-map nil)

;;; Normal Cases

(ert-deftest test-dev-fkeys-projectile-around-revert-invokes-orig-fn ()
  "Normal: advice calls the wrapped function with its args."
  (let ((calls nil)
        (cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (cj/--projectile-around-revert
       'projectile-compile-cmd-map
       (lambda (&rest args) (push args calls))
       'arg1 'arg2))
    (should (equal calls '((arg1 arg2))))))

(ert-deftest test-dev-fkeys-projectile-around-revert-captures-prior ()
  "Normal: advice captures the prior cmd into the buffer-local hook."
  (let ((cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (puthash "/p/" "make build" projectile-compile-cmd-map)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (let ((compile-buffer (get-buffer-create " *compile-capture*")))
        (unwind-protect
            (progn
              (cj/--projectile-around-revert
               'projectile-compile-cmd-map
               (lambda (&rest _) compile-buffer))
              (puthash "/p/" "make typo" projectile-compile-cmd-map)
              (with-current-buffer compile-buffer
                (run-hook-with-args 'compilation-finish-functions
                                    compile-buffer "exited abnormally\n"))
              (should (equal (gethash "/p/" projectile-compile-cmd-map)
                             "make build")))
          (kill-buffer compile-buffer))))))

(ert-deftest test-dev-fkeys-projectile-around-revert-installs-finish-hook ()
  "Normal: advice adds a buffer-local revert hook to the compilation buffer."
  (let ((cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (puthash "/p/" "make build" projectile-compile-cmd-map)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (with-current-buffer (get-buffer-create " *compile-a*")
        (setq-local compilation-finish-functions nil))
      (unwind-protect
          (let ((compile-buffer
                 (cj/--projectile-around-revert
                  'projectile-compile-cmd-map
                  (lambda (&rest _) (get-buffer-create " *compile-a*")))))
            (should-not compilation-finish-functions)
            (with-current-buffer compile-buffer
              (should compilation-finish-functions)))
        (kill-buffer " *compile-a*")))))

(ert-deftest test-dev-fkeys-projectile-around-revert-overlapping-compiles-use-own-state ()
  "Regression: overlapping compiles finishing out of order use their own state."
  (let ((cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal))
        (roots '("/one/" "/two/")))
    (puthash "/one/" "make one" projectile-compile-cmd-map)
    (puthash "/two/" "make two" projectile-compile-cmd-map)
    (cl-letf (((symbol-function 'cj/--f4-project-root)
               (lambda () (pop roots))))
      (let ((buf-one (get-buffer-create " *compile-one*"))
            (buf-two (get-buffer-create " *compile-two*")))
        (unwind-protect
            (progn
              (cj/--projectile-around-revert
               'projectile-compile-cmd-map
               (lambda (&rest _) buf-one))
              (cj/--projectile-around-revert
               'projectile-compile-cmd-map
               (lambda (&rest _) buf-two))
              (puthash "/one/" "make one typo" projectile-compile-cmd-map)
              (puthash "/two/" "make two typo" projectile-compile-cmd-map)
              (with-current-buffer buf-two
                (run-hook-with-args 'compilation-finish-functions
                                    buf-two "exited abnormally\n"))
              (should (string= (gethash "/two/" projectile-compile-cmd-map)
                               "make two"))
              (should (string= (gethash "/one/" projectile-compile-cmd-map)
                               "make one typo"))
              (with-current-buffer buf-one
                (run-hook-with-args 'compilation-finish-functions
                                    buf-one "exited abnormally\n"))
              (should (string= (gethash "/one/" projectile-compile-cmd-map)
                               "make one")))
          (kill-buffer buf-one)
          (kill-buffer buf-two))))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-projectile-around-revert-no-project-still-runs-orig-fn ()
  "Boundary: no project root → capture is a no-op, orig-fn still runs.
The state stays nil so the finish hook will be a no-op too."
  (let ((calls 0)
        (cj/--projectile-revert-state nil)
        (compilation-finish-functions nil))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () nil)))
      (cj/--projectile-around-revert
       'projectile-compile-cmd-map
       (lambda (&rest _) (cl-incf calls))))
    (should (= calls 1))
    (should (null cj/--projectile-revert-state))
    (should-not compilation-finish-functions)))

(provide 'test-dev-fkeys--projectile-around-revert)
;;; test-dev-fkeys--projectile-around-revert.el ends here
