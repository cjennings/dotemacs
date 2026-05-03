;;; test-dev-fkeys--f6-current-file-tests.el --- Smoke tests for cj/f6-current-file-tests -*- lexical-binding: t -*-

;;; Commentary:
;; Smoke tests for the C-F6 fast path. Resolves buffer-file-name and the
;; projectile root, then delegates to `cj/--f6-current-file-tests-impl'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-f6-current-file-tests-routes-to-impl ()
  "Normal: C-F6 invokes the orchestrator with buffer file and projectile root."
  (let (seen-file seen-root)
    (cl-letf (((symbol-function 'buffer-file-name) (lambda () "/p/foo.el"))
              ((symbol-function 'cj/--f4-project-root) (lambda () "/p/"))
              ((symbol-function 'cj/--f6-current-file-tests-impl)
               (lambda (file root)
                 (setq seen-file file seen-root root))))
      (cj/f6-current-file-tests)
      (should (string= seen-file "/p/foo.el"))
      (should (string= seen-root "/p/")))))

(provide 'test-dev-fkeys--f6-current-file-tests)
;;; test-dev-fkeys--f6-current-file-tests.el ends here
