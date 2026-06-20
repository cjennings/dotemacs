;;; test-prog-general--find-project-root-file.el --- Tests for cj/find-project-root-file -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/find-project-root-file returns the first file in the current Projectile
;; project root matching a regexp (string or rx form), case-insensitively.  It
;; was defined inside the projectile use-package :config (unreachable under
;; `make test'); lifting it to top level makes it unit-testable.  projectile's
;; root and directory-files are mocked at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

(defmacro test-prg--with-root (files &rest body)
  "Run BODY with projectile-project-root \"/proj/\" and directory-files = FILES."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'projectile-project-root) (lambda (&rest _) "/proj/"))
             ((symbol-function 'directory-files) (lambda (&rest _) ,files)))
     ,@body))

(ert-deftest test-prg-find-root-file-string-regexp ()
  "Normal: a string regexp matches case-insensitively."
  (test-prg--with-root '("README.md" "TODO.org" "src")
    (should (equal (cj/find-project-root-file "^todo\\.org$") "TODO.org"))))

(ert-deftest test-prg-find-root-file-rx-form ()
  "Normal: an rx form is converted and matched."
  (test-prg--with-root '("notes.txt" "todo.md" "x")
    (should (equal (cj/find-project-root-file
                    '(seq bos "todo." (or "org" "md" "txt") eos))
                   "todo.md"))))

(ert-deftest test-prg-find-root-file-no-match ()
  "Boundary: no matching file yields nil."
  (test-prg--with-root '("a.el" "b.el")
    (should (null (cj/find-project-root-file "^todo\\.org$")))))

(ert-deftest test-prg-find-root-file-no-project ()
  "Boundary: outside a project (nil root) yields nil."
  (cl-letf (((symbol-function 'projectile-project-root) (lambda (&rest _) nil)))
    (should (null (cj/find-project-root-file "^todo\\.org$")))))

(provide 'test-prog-general--find-project-root-file)
;;; test-prog-general--find-project-root-file.el ends here
