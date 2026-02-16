;;; test-org-agenda-config-add-files.el --- Tests for cj/add-files-to-org-agenda-files-list -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/add-files-to-org-agenda-files-list from org-agenda-config.el.
;; Uses testutil-general.el for filesystem setup/teardown.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-general)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

;;; Normal Cases

(ert-deftest test-org-agenda-config-add-files-normal-finds-todo-in-subdirs ()
  "Subdirectories containing todo.org should be added to org-agenda-files."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project-a/todo.org" "* TODO Task A\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project-b/todo.org" "* TODO Task B\n")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          (should (= 2 (length org-agenda-files)))
          (should (cl-every #'file-exists-p org-agenda-files))
          (should (cl-every (lambda (f) (string-suffix-p "todo.org" f)) org-agenda-files))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-agenda-config-add-files-normal-ignores-subdirs-without-todo ()
  "Subdirectories without todo.org should not add anything."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project-a/readme.org" "Notes\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project-b/notes.txt" "Stuff\n")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          (should (= 0 (length org-agenda-files)))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-agenda-config-add-files-normal-mixed-subdirs ()
  "Only subdirectories with todo.org should be added."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/has-todo/todo.org" "* TODO Yes\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/no-todo/readme.org" "No tasks\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/also-has/todo.org" "* TODO Also\n")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          (should (= 2 (length org-agenda-files)))))
    (cj/delete-test-base-dir)))

;;; Boundary Cases

(ert-deftest test-org-agenda-config-add-files-boundary-empty-directory ()
  "Empty directory should add nothing."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          (should (= 0 (length org-agenda-files)))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-agenda-config-add-files-boundary-hidden-dirs-excluded ()
  "Hidden subdirectories (starting with .) should be excluded."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/.hidden-project/todo.org" "* TODO Hidden\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/visible-project/todo.org" "* TODO Visible\n")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          (should (= 1 (length org-agenda-files)))
          (should (string-match-p "visible-project" (car org-agenda-files)))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-agenda-config-add-files-boundary-files-at-top-level-ignored ()
  "Regular files (not directories) at the top level should be ignored."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/todo.org" "* TODO Top-level\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project/todo.org" "* TODO In subdir\n")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          ;; Only the subdir's todo.org, not the top-level one
          (should (= 1 (length org-agenda-files)))
          (should (string-match-p "project" (car org-agenda-files)))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-agenda-config-add-files-boundary-no-deep-recursion ()
  "Should not recurse into nested subdirectories."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project/todo.org" "* TODO Shallow\n")
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project/subdir/todo.org" "* TODO Deep\n")
        (let ((org-agenda-files nil)
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          ;; Only immediate subdir's todo.org
          (should (= 1 (length org-agenda-files)))
          (should-not (string-match-p "subdir" (car org-agenda-files)))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-agenda-config-add-files-boundary-appends-to-existing ()
  "Should append to existing org-agenda-files, not replace."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (cj/create-directory-or-file-ensuring-parents "agenda-test/project/todo.org" "* TODO New\n")
        (let ((org-agenda-files '("/existing/file.org"))
              (test-dir (expand-file-name "agenda-test" cj/test-base-dir)))
          (cj/add-files-to-org-agenda-files-list test-dir)
          (should (= 2 (length org-agenda-files)))
          (should (member "/existing/file.org" org-agenda-files))))
    (cj/delete-test-base-dir)))

(provide 'test-org-agenda-config-add-files)
;;; test-org-agenda-config-add-files.el ends here
