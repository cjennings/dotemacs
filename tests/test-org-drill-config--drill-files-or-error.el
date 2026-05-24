;;; test-org-drill-config--drill-files-or-error.el --- Tests for validated drill file listing -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--drill-files-or-error, the single validated entry point
;; that drill capture templates and the drill commands share.  It returns the
;; drill files in a directory or signals a clear `user-error' when the
;; directory is missing, unreadable, or empty — instead of leaking a low-level
;; error from `directory-files' or handing `completing-read' an empty list.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-drill-config)

;;; Normal Cases

(ert-deftest test-org-drill--files-or-error-normal-lists-org-files ()
  "Normal: a directory with drill files returns their names."
  (let ((dir (make-temp-file "cj-drill-or-error-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "a.org" dir))
          (write-region "" nil (expand-file-name "b.org" dir))
          (should (equal '("a.org" "b.org")
                         (cj/--drill-files-or-error dir))))
      (delete-directory dir t))))

;;; Boundary Cases

(ert-deftest test-org-drill--files-or-error-boundary-empty-dir-signals ()
  "Boundary: an existing but empty directory signals a clear user-error."
  (let ((dir (make-temp-file "cj-drill-or-error-empty-" t)))
    (unwind-protect
        (should-error (cj/--drill-files-or-error dir) :type 'user-error)
      (delete-directory dir t))))

(ert-deftest test-org-drill--files-or-error-boundary-only-non-org-signals ()
  "Boundary: a directory with no .org files signals a user-error."
  (let ((dir (make-temp-file "cj-drill-or-error-nonorg-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "notes.txt" dir))
          (should-error (cj/--drill-files-or-error dir) :type 'user-error))
      (delete-directory dir t))))

;;; Error Cases

(ert-deftest test-org-drill--files-or-error-missing-dir-signals-user-error ()
  "Error: a missing directory signals a `user-error', not a low-level error."
  (should-error (cj/--drill-files-or-error "/no/such/cj-drill/dir/")
                :type 'user-error))

(provide 'test-org-drill-config--drill-files-or-error)
;;; test-org-drill-config--drill-files-or-error.el ends here
