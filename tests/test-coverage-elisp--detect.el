;;; test-coverage-elisp--detect.el --- Tests for cj/--coverage-elisp-detect -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--coverage-elisp-detect', the heuristic that
;; decides whether a given project root is an Elisp project.
;;
;; Heuristic: requires BOTH a Makefile/Eask/Cask AND some .el files
;; (at root or under modules/).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-elisp)

(defun test-coverage-elisp-detect--make-project (spec)
  "Create a temp directory matching SPEC.
SPEC is a list of relative paths to create.  Paths ending in `/' are
directories; others are files.  Returns the temp directory path."
  (let ((root (make-temp-file "test-elisp-project-" t)))
	(dolist (path spec)
	  (let ((full (expand-file-name path root)))
		(if (string-suffix-p "/" path)
			(make-directory full t)
		  (make-directory (file-name-directory full) t)
		  (write-region "" nil full))))
	root))

;;; Normal cases

(ert-deftest test-coverage-elisp-detect-with-makefile-and-modules ()
  "Normal: Makefile plus modules/foo.el is detected as elisp."
  (let ((root (test-coverage-elisp-detect--make-project
			   '("Makefile" "modules/foo.el"))))
	(unwind-protect
		(should (cj/--coverage-elisp-detect root))
	  (delete-directory root t))))

(ert-deftest test-coverage-elisp-detect-with-eask-and-root-el ()
  "Normal: Eask plus root-level .el file is detected."
  (let ((root (test-coverage-elisp-detect--make-project
			   '("Eask" "main.el"))))
	(unwind-protect
		(should (cj/--coverage-elisp-detect root))
	  (delete-directory root t))))

(ert-deftest test-coverage-elisp-detect-with-cask-and-modules ()
  "Normal: Cask plus modules/ directory is detected."
  (let ((root (test-coverage-elisp-detect--make-project
			   '("Cask" "modules/bar.el"))))
	(unwind-protect
		(should (cj/--coverage-elisp-detect root))
	  (delete-directory root t))))

;;; Boundary cases

(ert-deftest test-coverage-elisp-detect-no-build-file ()
  "Boundary: .el files without a Makefile/Eask/Cask is NOT detected."
  (let ((root (test-coverage-elisp-detect--make-project
			   '("main.el" "other.el"))))
	(unwind-protect
		(should-not (cj/--coverage-elisp-detect root))
	  (delete-directory root t))))

(ert-deftest test-coverage-elisp-detect-makefile-without-el ()
  "Boundary: Makefile with no .el files is NOT detected."
  (let ((root (test-coverage-elisp-detect--make-project
			   '("Makefile" "README.md"))))
	(unwind-protect
		(should-not (cj/--coverage-elisp-detect root))
	  (delete-directory root t))))

(ert-deftest test-coverage-elisp-detect-empty-directory ()
  "Boundary: an empty directory is not an elisp project."
  (let ((root (make-temp-file "test-empty-" t)))
	(unwind-protect
		(should-not (cj/--coverage-elisp-detect root))
	  (delete-directory root t))))

;;; Error cases

(ert-deftest test-coverage-elisp-detect-nonexistent-root ()
  "Error: a nonexistent ROOT returns nil, not an error."
  (should-not (cj/--coverage-elisp-detect "/nonexistent/path/for-test-12345")))

;;; Registry integration

(ert-deftest test-coverage-elisp-registered-on-load ()
  "Normal: loading coverage-elisp registers the `elisp' backend."
  (let ((backend (cj/--coverage-backend-by-name 'elisp)))
	(should backend)
	(should (eq 'elisp (plist-get backend :name)))
	(should (functionp (plist-get backend :detect)))
	(should (functionp (plist-get backend :run)))
	(should (functionp (plist-get backend :lcov-path)))))

(provide 'test-coverage-elisp--detect)
;;; test-coverage-elisp--detect.el ends here
