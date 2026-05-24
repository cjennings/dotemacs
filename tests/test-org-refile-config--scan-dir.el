;;; test-org-refile-config--scan-dir.el --- Tests for refile dir scan -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--org-refile-scan-dir, which lists the todo.org files
;; under one root for the refile-target scan.  A missing, unreadable, or
;; permission-denied root must be non-fatal: it logs a warning and returns
;; nil so the rest of the scan continues, instead of silently swallowing the
;; failure or crashing on a missing directory.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-refile-config)

;;; Normal Cases

(ert-deftest test-org-refile-scan-dir-normal-finds-todo ()
  "Normal: a readable directory with a todo.org returns it."
  (let ((dir (make-temp-file "cj-refile-scan-" t)))
    (unwind-protect
        (progn
          (write-region "* TODO x\n" nil (expand-file-name "todo.org" dir))
          (let ((found (cj/--org-refile-scan-dir dir)))
            (should (= 1 (length found)))
            (should (string-suffix-p "todo.org" (car found)))))
      (delete-directory dir t))))

;;; Boundary Cases

(ert-deftest test-org-refile-scan-dir-boundary-missing-warns-and-returns-nil ()
  "Boundary: a missing directory warns and returns nil, without erroring."
  (let ((warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (should (null (cj/--org-refile-scan-dir "/no/such/cj-refile/dir/")))
      (should warned))))

;;; Error Cases

(ert-deftest test-org-refile-scan-dir-error-permission-denied-warns-and-returns-nil ()
  "Error: a permission-denied during the scan warns and returns nil."
  (let ((dir (make-temp-file "cj-refile-perm-" t))
        (warned nil))
    (unwind-protect
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (&rest _) (signal 'permission-denied (list dir))))
                  ((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warned t))))
          (should (null (cj/--org-refile-scan-dir dir)))
          (should warned))
      (delete-directory dir t))))

(provide 'test-org-refile-config--scan-dir)
;;; test-org-refile-config--scan-dir.el ends here
