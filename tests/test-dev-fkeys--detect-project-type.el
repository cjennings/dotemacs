;;; test-dev-fkeys--detect-project-type.el --- Tests for cj/--detect-project-type -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the project-type classifier in dev-fkeys.el.
;; The classifier returns 'compiled, 'interpreted, or 'unknown based on
;; marker files at the project root. Interpreted markers are checked first
;; so a Python or Node project with a Makefile for tasks classifies as
;; interpreted.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys--with-project-dir (markers &rest body)
  "Create a temp project dir with each filename in MARKERS as an empty file.
Bind the dir path to ROOT in BODY. Cleans up on exit."
  (declare (indent 1))
  `(let ((root (make-temp-file "test-dev-fkeys-" t)))
     (unwind-protect
         (progn
           (dolist (marker ,markers)
             (write-region "" nil (expand-file-name marker root)))
           ,@body)
       (delete-directory root t))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-detect-project-type-go-mod-is-compiled ()
  "Normal: go.mod marker classifies as compiled."
  (test-dev-fkeys--with-project-dir '("go.mod")
    (should (eq (cj/--detect-project-type root) 'compiled))))

(ert-deftest test-dev-fkeys-detect-project-type-cargo-toml-is-compiled ()
  "Normal: Cargo.toml marker classifies as compiled."
  (test-dev-fkeys--with-project-dir '("Cargo.toml")
    (should (eq (cj/--detect-project-type root) 'compiled))))

(ert-deftest test-dev-fkeys-detect-project-type-cmakelists-is-compiled ()
  "Normal: CMakeLists.txt marker classifies as compiled."
  (test-dev-fkeys--with-project-dir '("CMakeLists.txt")
    (should (eq (cj/--detect-project-type root) 'compiled))))

(ert-deftest test-dev-fkeys-detect-project-type-makefile-is-compiled ()
  "Normal: Makefile marker classifies as compiled."
  (test-dev-fkeys--with-project-dir '("Makefile")
    (should (eq (cj/--detect-project-type root) 'compiled))))

(ert-deftest test-dev-fkeys-detect-project-type-eask-is-compiled ()
  "Normal: Eask marker classifies as compiled."
  (test-dev-fkeys--with-project-dir '("Eask")
    (should (eq (cj/--detect-project-type root) 'compiled))))

(ert-deftest test-dev-fkeys-detect-project-type-pyproject-is-interpreted ()
  "Normal: pyproject.toml marker classifies as interpreted."
  (test-dev-fkeys--with-project-dir '("pyproject.toml")
    (should (eq (cj/--detect-project-type root) 'interpreted))))

(ert-deftest test-dev-fkeys-detect-project-type-requirements-is-interpreted ()
  "Normal: requirements.txt marker classifies as interpreted."
  (test-dev-fkeys--with-project-dir '("requirements.txt")
    (should (eq (cj/--detect-project-type root) 'interpreted))))

(ert-deftest test-dev-fkeys-detect-project-type-pipfile-is-interpreted ()
  "Normal: Pipfile marker classifies as interpreted."
  (test-dev-fkeys--with-project-dir '("Pipfile")
    (should (eq (cj/--detect-project-type root) 'interpreted))))

(ert-deftest test-dev-fkeys-detect-project-type-package-json-is-interpreted ()
  "Normal: package.json marker classifies as interpreted."
  (test-dev-fkeys--with-project-dir '("package.json")
    (should (eq (cj/--detect-project-type root) 'interpreted))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-detect-project-type-interpreted-wins-over-compiled ()
  "Boundary: a Python project with a Makefile classifies as interpreted.
Interpreted markers are checked first so task-runner Makefiles in
Python/Node projects don't misclassify them as compiled."
  (test-dev-fkeys--with-project-dir '("pyproject.toml" "Makefile")
    (should (eq (cj/--detect-project-type root) 'interpreted))))

(ert-deftest test-dev-fkeys-detect-project-type-package-json-wins-over-makefile ()
  "Boundary: package.json + Makefile classifies as interpreted."
  (test-dev-fkeys--with-project-dir '("package.json" "Makefile")
    (should (eq (cj/--detect-project-type root) 'interpreted))))

(ert-deftest test-dev-fkeys-detect-project-type-pure-c-with-makefile-is-compiled ()
  "Boundary: a Makefile alone (no interpreted markers) classifies as compiled.
This is the typical pure-C project case."
  (test-dev-fkeys--with-project-dir '("Makefile")
    (should (eq (cj/--detect-project-type root) 'compiled))))

(ert-deftest test-dev-fkeys-detect-project-type-no-markers-is-unknown ()
  "Boundary: directory with no markers classifies as unknown."
  (test-dev-fkeys--with-project-dir '()
    (should (eq (cj/--detect-project-type root) 'unknown))))

(ert-deftest test-dev-fkeys-detect-project-type-irrelevant-file-is-unknown ()
  "Boundary: unrelated files in root don't trigger any classification."
  (test-dev-fkeys--with-project-dir '("README.md" "LICENSE" ".gitignore")
    (should (eq (cj/--detect-project-type root) 'unknown))))

;;; Error Cases

(ert-deftest test-dev-fkeys-detect-project-type-nil-root-is-unknown ()
  "Error: nil ROOT returns 'unknown (buffer not in a project)."
  (should (eq (cj/--detect-project-type nil) 'unknown)))

(ert-deftest test-dev-fkeys-detect-project-type-nonexistent-root-is-unknown ()
  "Error: a directory path that doesn't exist returns 'unknown.
file-exists-p on a non-existent path returns nil, so no marker matches."
  (should (eq (cj/--detect-project-type "/nonexistent/path/xyzzy") 'unknown)))

(provide 'test-dev-fkeys--detect-project-type)
;;; test-dev-fkeys--detect-project-type.el ends here
