;;; test-org-refile-config-ensure-org-mode.el --- Tests for cj/org-refile-ensure-org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/org-refile-ensure-org-mode from org-refile-config.el.
;; Uses testutil-general.el for filesystem setup/teardown.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-general)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-refile-config)

;;; Normal Cases

(ert-deftest test-org-refile-ensure-org-mode-normal-org-file-returns-buffer ()
  "A .org file already in org-mode should return its buffer."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/notes.org" "* Heading\n")))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (let ((org-mode-hook nil) (text-mode-hook nil))
                      (org-mode)))
                  (let ((result (cj/org-refile-ensure-org-mode file)))
                    (should (bufferp result))
                    (with-current-buffer result
                      (should (derived-mode-p 'org-mode)))))
              (kill-buffer buf)))))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-refile-ensure-org-mode-normal-switches-fundamental-to-org ()
  "A .org file in fundamental-mode should be switched to org-mode."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/stuck.org" "* TODO Task\n")))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (fundamental-mode))
                  (should (with-current-buffer buf
                            (eq major-mode 'fundamental-mode)))
                  (let ((org-mode-hook nil) (text-mode-hook nil))
                    (cj/org-refile-ensure-org-mode file))
                  (with-current-buffer buf
                    (should (derived-mode-p 'org-mode))))
              (kill-buffer buf)))))
    (cj/delete-test-base-dir)))

;;; Error Cases

(ert-deftest test-org-refile-ensure-org-mode-error-non-org-extension ()
  "A non-.org file should signal an error."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/notes.txt" "Some text\n")))
          (should-error (cj/org-refile-ensure-org-mode file)
                        :type 'error)))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-refile-ensure-org-mode-error-markdown-extension ()
  "A .md file should signal an error."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/notes.md" "# Heading\n")))
          (should-error (cj/org-refile-ensure-org-mode file)
                        :type 'error)))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-refile-ensure-org-mode-error-no-extension ()
  "A file without an extension should signal an error."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/notes" "content\n")))
          (should-error (cj/org-refile-ensure-org-mode file)
                        :type 'error)))
    (cj/delete-test-base-dir)))

;;; Boundary Cases

(ert-deftest test-org-refile-ensure-org-mode-boundary-org-in-dirname ()
  "A file with .org in the directory name but not the extension should error."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/my.org.files/todo.txt" "* TODO\n")))
          (should-error (cj/org-refile-ensure-org-mode file)
                        :type 'error)))
    (cj/delete-test-base-dir)))

(ert-deftest test-org-refile-ensure-org-mode-boundary-uppercase-org-extension ()
  "A .ORG file (uppercase) should be accepted (case-insensitive match)."
  (unwind-protect
      (progn
        (cj/create-test-base-dir)
        (let ((file (cj/create-directory-or-file-ensuring-parents
                     "refile-test/notes.ORG" "* Heading\n")))
          (let ((buf (cj/org-refile-ensure-org-mode file)))
            (unwind-protect
                (should (bufferp buf))
              (when (buffer-live-p buf) (kill-buffer buf))))))
    (cj/delete-test-base-dir)))

(provide 'test-org-refile-config-ensure-org-mode)
;;; test-org-refile-config-ensure-org-mode.el ends here
