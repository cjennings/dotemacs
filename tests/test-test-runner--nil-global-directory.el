;;; test-test-runner--nil-global-directory.el --- Tests for the no-test-directory path -*- lexical-binding: t -*-

;;; Commentary:
;; Outside a Projectile project, `cj/test--get-test-directory' falls back
;; to `cj/test-global-directory', which defaults to nil.  These tests pin
;; the contract for that nil case: discovery helpers return nil instead
;; of crashing on (file-directory-p nil), and the interactive commands
;; signal `user-error' instead of wrong-type-argument.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'test-runner)

(defmacro test-runner-nil-dir--outside-project (&rest body)
  "Run BODY with no project root and a nil `cj/test-global-directory'."
  (declare (indent 0))
  `(let ((cj/test-global-directory nil))
     (cl-letf (((symbol-function 'cj/test--project-root)
                (lambda () nil)))
       ,@body)))

;;; Boundary Cases

(ert-deftest test-test-runner-nil-dir-get-test-directory-returns-nil ()
  "Boundary: no project and no global dir yields nil, not an error."
  (test-runner-nil-dir--outside-project
    (should (null (cj/test--get-test-directory)))))

(ert-deftest test-test-runner-nil-dir-get-test-files-returns-nil ()
  "Boundary: file discovery returns nil instead of crashing on a nil dir."
  (test-runner-nil-dir--outside-project
    (should (null (cj/test--get-test-files)))))

;;; Error Cases

(ert-deftest test-test-runner-nil-dir-load-all-signals-user-error ()
  "Error: `cj/test-load-all' signals `user-error', not wrong-type-argument."
  (test-runner-nil-dir--outside-project
    (should-error (cj/test-load-all) :type 'user-error)))

(ert-deftest test-test-runner-nil-dir-focus-add-signals-user-error ()
  "Error: `cj/test-focus-add' signals `user-error', not wrong-type-argument."
  (test-runner-nil-dir--outside-project
    (should-error (cj/test-focus-add) :type 'user-error)))

(provide 'test-test-runner--nil-global-directory)
;;; test-test-runner--nil-global-directory.el ends here
