;;; test-testutil-general.el --- Tests for shared test utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies shared test scratch paths are sandbox-friendly and guarded.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-general)

(ert-deftest test-testutil-general-default-base-dir-is-under-temp ()
  "The default test root should not require home-directory write access."
  (skip-unless (not (getenv "CJ_EMACS_TEST_DIR")))
  (should (file-in-directory-p cj/test-base-dir temporary-file-directory)))

(ert-deftest test-testutil-general-env-override-shape ()
  "A stable local test root can still be supplied via environment variable."
  (let ((process-environment
         (cons "CJ_EMACS_TEST_DIR=/tmp/cj-custom-test-root" process-environment)))
    (should (equal
             (file-name-as-directory
              (expand-file-name
               (or (getenv "CJ_EMACS_TEST_DIR")
                   (expand-file-name "cj-emacs-tests/" temporary-file-directory))))
             "/tmp/cj-custom-test-root/"))))

(ert-deftest test-testutil-general-create-file-rejects-parent-escape ()
  "Relative paths must not escape the selected test root."
  (let ((cj/test-base-dir (make-temp-file "testutil-base-" t)))
    (should-error
     (cj/create-directory-or-file-ensuring-parents "../escape.txt" "bad"))))

(ert-deftest test-testutil-general-delete-refuses-temp-root ()
  "Cleanup must refuse broad roots such as `temporary-file-directory'."
  (let ((cj/test-base-dir temporary-file-directory))
    (should-error (cj/delete-test-base-dir))))

(ert-deftest test-testutil-general-delete-removes-selected-root ()
  "Cleanup should remove a specific selected test root."
  (let ((cj/test-base-dir (make-temp-file "testutil-delete-" t)))
    (cj/create-directory-or-file-ensuring-parents "nested/file.txt" "ok")
    (should (file-directory-p cj/test-base-dir))
    (cj/delete-test-base-dir)
    (should-not (file-exists-p cj/test-base-dir))))

(provide 'test-testutil-general)
;;; test-testutil-general.el ends here
