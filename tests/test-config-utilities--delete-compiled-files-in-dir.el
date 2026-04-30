;;; test-config-utilities--delete-compiled-files-in-dir.el --- Tests for cj/--delete-compiled-files-in-dir -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--delete-compiled-files-in-dir'. The helper walks DIR
;; recursively, deletes every `.elc' and `.eln' file, and returns the
;; count. Tests run against real temp directories so the file-system
;; recursion and suffix matching are exercised end-to-end.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(defun test-config-utilities--make-temp-fixture-dir (specs)
  "Create a fresh temp directory containing files described by SPECS.
SPECS is a list of file paths relative to the temp dir.  Each path
gets created as an empty file (subdirectories created as needed).
Returns the temp dir path."
  (let ((dir (make-temp-file "delete-compiled-test-" t)))
    (dolist (spec specs)
      (let ((full (expand-file-name spec dir)))
        (let ((parent (file-name-directory full)))
          (when (and parent (not (file-exists-p parent)))
            (make-directory parent t)))
        (with-temp-file full (insert ""))))
    dir))

(ert-deftest test-config-utilities-delete-compiled-deletes-elc-and-eln ()
  "Normal: a directory with mixed .el / .elc / .eln files keeps .el and
deletes both compiled forms.  Returns the count of deletions."
  (let ((dir (test-config-utilities--make-temp-fixture-dir
              '("a.el" "a.elc" "b.eln" "c.el"))))
    (unwind-protect
        (let ((count (cj/--delete-compiled-files-in-dir dir)))
          (should (= count 2))
          (should (file-exists-p (expand-file-name "a.el" dir)))
          (should (file-exists-p (expand-file-name "c.el" dir)))
          (should-not (file-exists-p (expand-file-name "a.elc" dir)))
          (should-not (file-exists-p (expand-file-name "b.eln" dir))))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-delete-compiled-recursive ()
  "Boundary: deletion descends into subdirectories."
  (let ((dir (test-config-utilities--make-temp-fixture-dir
              '("top.elc" "sub/nested.elc" "sub/nested.el"
                "sub/deep/very-nested.eln"))))
    (unwind-protect
        (let ((count (cj/--delete-compiled-files-in-dir dir)))
          (should (= count 3))
          (should (file-exists-p (expand-file-name "sub/nested.el" dir)))
          (should-not (file-exists-p (expand-file-name "top.elc" dir)))
          (should-not (file-exists-p (expand-file-name "sub/nested.elc" dir)))
          (should-not (file-exists-p
                       (expand-file-name "sub/deep/very-nested.eln" dir))))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-delete-compiled-no-compiled-files ()
  "Boundary: a directory with no compiled files reports 0 and changes nothing."
  (let ((dir (test-config-utilities--make-temp-fixture-dir
              '("only.el" "another.el" "sub/more.el"))))
    (unwind-protect
        (let ((count (cj/--delete-compiled-files-in-dir dir)))
          (should (zerop count))
          (should (file-exists-p (expand-file-name "only.el" dir))))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-delete-compiled-empty-dir ()
  "Boundary: an empty directory returns 0."
  (let ((dir (make-temp-file "empty-delete-test-" t)))
    (unwind-protect
        (should (zerop (cj/--delete-compiled-files-in-dir dir)))
      (delete-directory dir t))))

(ert-deftest test-config-utilities-delete-compiled-suffix-not-substring ()
  "Boundary: a file whose name CONTAINS \".elc\" but doesn't end in it is kept."
  (let ((dir (test-config-utilities--make-temp-fixture-dir
              '("not-elc-suffix.el"
                "looks.elc.bak"
                "real.elc"))))
    (unwind-protect
        (let ((count (cj/--delete-compiled-files-in-dir dir)))
          (should (= count 1))
          (should (file-exists-p (expand-file-name "not-elc-suffix.el" dir)))
          (should (file-exists-p (expand-file-name "looks.elc.bak" dir)))
          (should-not (file-exists-p (expand-file-name "real.elc" dir))))
      (delete-directory dir t))))

(provide 'test-config-utilities--delete-compiled-files-in-dir)
;;; test-config-utilities--delete-compiled-files-in-dir.el ends here
