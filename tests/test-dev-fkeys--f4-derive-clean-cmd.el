;;; test-dev-fkeys--f4-derive-clean-cmd.el --- Tests for cj/--f4-derive-clean-cmd -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the heuristic clean-command picker used by M-F4
;; (Clean + Rebuild). Returns a shell command string based on the project
;; root's marker files, or nil when no marker matches.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys-clean--with-project-dir (markers &rest body)
  "Create a temp project dir with each filename in MARKERS as an empty file.
Bind the dir path to ROOT in BODY. Cleans up on exit."
  (declare (indent 1))
  `(let ((root (make-temp-file "test-dev-fkeys-clean-" t)))
     (unwind-protect
         (progn
           (dolist (marker ,markers)
             (write-region "" nil (expand-file-name marker root)))
           ,@body)
       (delete-directory root t))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-derive-clean-cmd-go-mod-uses-go-clean ()
  "Normal: go.mod yields go clean ./..."
  (test-dev-fkeys-clean--with-project-dir '("go.mod")
    (should (string= (cj/--f4-derive-clean-cmd root) "go clean ./..."))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-cargo-toml-uses-cargo-clean ()
  "Normal: Cargo.toml yields cargo clean."
  (test-dev-fkeys-clean--with-project-dir '("Cargo.toml")
    (should (string= (cj/--f4-derive-clean-cmd root) "cargo clean"))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-eask-uses-eask-clean ()
  "Normal: Eask yields eask clean."
  (test-dev-fkeys-clean--with-project-dir '("Eask")
    (should (string= (cj/--f4-derive-clean-cmd root) "eask clean"))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-makefile-uses-make-clean ()
  "Normal: Makefile yields make clean."
  (test-dev-fkeys-clean--with-project-dir '("Makefile")
    (should (string= (cj/--f4-derive-clean-cmd root) "make clean"))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-cmakelists-uses-cmake-clean ()
  "Normal: CMakeLists.txt yields cmake build-dir clean target."
  (test-dev-fkeys-clean--with-project-dir '("CMakeLists.txt")
    (should (string= (cj/--f4-derive-clean-cmd root)
                     "cmake --build build --target clean"))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-derive-clean-cmd-go-wins-over-makefile ()
  "Boundary: go.mod + Makefile picks go clean (go.mod is checked first)."
  (test-dev-fkeys-clean--with-project-dir '("go.mod" "Makefile")
    (should (string= (cj/--f4-derive-clean-cmd root) "go clean ./..."))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-cargo-wins-over-makefile ()
  "Boundary: Cargo.toml + Makefile picks cargo clean."
  (test-dev-fkeys-clean--with-project-dir '("Cargo.toml" "Makefile")
    (should (string= (cj/--f4-derive-clean-cmd root) "cargo clean"))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-eask-wins-over-makefile ()
  "Boundary: Eask + Makefile picks eask clean."
  (test-dev-fkeys-clean--with-project-dir '("Eask" "Makefile")
    (should (string= (cj/--f4-derive-clean-cmd root) "eask clean"))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-no-markers-returns-nil ()
  "Boundary: directory with no recognized markers returns nil."
  (test-dev-fkeys-clean--with-project-dir '()
    (should (null (cj/--f4-derive-clean-cmd root)))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-irrelevant-files-returns-nil ()
  "Boundary: only unrelated files returns nil."
  (test-dev-fkeys-clean--with-project-dir '("README.md" "LICENSE")
    (should (null (cj/--f4-derive-clean-cmd root)))))

;;; Error Cases

(ert-deftest test-dev-fkeys-derive-clean-cmd-nil-root-returns-nil ()
  "Error: nil ROOT returns nil with no error."
  (should (null (cj/--f4-derive-clean-cmd nil))))

(ert-deftest test-dev-fkeys-derive-clean-cmd-nonexistent-root-returns-nil ()
  "Error: a non-existent path returns nil. file-exists-p returns nil so no
marker matches."
  (should (null (cj/--f4-derive-clean-cmd "/nonexistent/path/xyzzy"))))

(provide 'test-dev-fkeys--f4-derive-clean-cmd)
;;; test-dev-fkeys--f4-derive-clean-cmd.el ends here
