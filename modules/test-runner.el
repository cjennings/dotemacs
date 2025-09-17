;;; test-runner.el --- Test Runner for Emacs Configuration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Provides utilities for running ERT tests with focus/unfocus workflow.
;; Tests should be located in =user-emacs-directory'/tests/
;; and follow the naming pattern test-*.el

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Variables

(defvar cj/test-directory (expand-file-name "tests" user-emacs-directory)
  "Directory containing test files.")

(defvar cj/test-focused-files '()
  "List of test files for focused test execution.
Each element is a filename (without path) to run.")

(defvar cj/test-mode 'all
  "Current test execution mode.
Either 'all (run all tests) or 'focused (run only focused tests).")

(defvar cj/test-last-results nil
  "Results from the last test run.")

;;; Core Functions

(defun cj/test--get-test-files ()
  "Return a list of test file names (without path) in the test directory."
  (when (file-directory-p cj/test-directory)
	(mapcar #'file-name-nondirectory
			(directory-files cj/test-directory t "^test-.*\\.el$"))))

(defun cj/test-load-all ()
  "Load all test files from the test directory."
  (interactive)
  (unless (file-directory-p cj/test-directory)
	(user-error "Test directory %s does not exist" cj/test-directory))

  (let ((test-files (directory-files cj/test-directory t "^test-.*\\.el$"))
		(loaded-count 0))
	(dolist (file test-files)
	  (condition-case err
		  (progn
			(load-file file)
			(setq loaded-count (1+ loaded-count))
			(message "Loaded test file: %s" (file-name-nondirectory file)))
		(error
		 (message "Error loading %s: %s"
				  (file-name-nondirectory file)
				  (error-message-string err)))))
	(message "Loaded %d test file(s)" loaded-count)))

(defun cj/test-run-all ()
  "Load and run all tests."
  (interactive)
  (cj/test-load-all)
  (ert t))

(defun cj/test-focus-add ()
  "Select test file(s) to add to the focused list."
  (interactive)
  (let ((available-files (cj/test--get-test-files)))
	(if (null available-files)
		(user-error "No test files found in %s" cj/test-directory)
	  (let* ((unfocused-files (cl-set-difference available-files
												  cj/test-focused-files
												  :test #'string=))
			 (selected (if unfocused-files
						   (completing-read "Add test file to focus: "
											unfocused-files
											nil t)
						 (user-error "All test files are already focused"))))
		(push selected cj/test-focused-files)
		(message "Added to focus: %s" selected)
		(when (called-interactively-p 'interactive)
		  (cj/test-view-focused))))))

(defun cj/test-focus-remove ()
  "Remove a test file from the focused list."
  (interactive)
  (if (null cj/test-focused-files)
	  (user-error "No focused files to remove")
	(let ((selected (completing-read "Remove from focus: "
									  cj/test-focused-files
									  nil t)))
	  (setq cj/test-focused-files
			(delete selected cj/test-focused-files))
	  (message "Removed from focus: %s" selected)
	  (when (called-interactively-p 'interactive)
		(cj/test-view-focused)))))

(defun cj/test-focus-clear ()
  "Clear all focused test files."
  (interactive)
  (setq cj/test-focused-files '())
  (message "Cleared all focused test files"))

(defun cj/test--extract-test-names (file)
  "Extract test names from FILE.
Returns a list of test name symbols defined in the file."
  (let ((test-names '()))
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  ;; Find all (ert-deftest NAME ...) forms
	  (while (re-search-forward "^\\s-*(ert-deftest\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
		(push (match-string 1) test-names)))
	test-names))

(defun cj/test-run-focused ()
  "Run only the focused test files."
  (interactive)
  (if (null cj/test-focused-files)
	  (user-error "No focused files set. Use `cj/test-focus-add' first")
	(let ((all-test-names '())
		  (loaded-count 0))
	  ;; Load the focused files and collect their test names
	  (dolist (file cj/test-focused-files)
		(let ((full-path (expand-file-name file cj/test-directory)))
		  (when (file-exists-p full-path)
			(load-file full-path)
			(setq loaded-count (1+ loaded-count))
			;; Extract test names from this file
			(let ((test-names (cj/test--extract-test-names full-path)))
			  (setq all-test-names (append all-test-names test-names))))))

	  (if (null all-test-names)
		  (message "No tests found in focused files")
		;; Build a regexp that matches any of our test names
		(let ((pattern (regexp-opt all-test-names)))
		  (message "Running %d test(s) from %d focused file(s)"
				   (length all-test-names) loaded-count)
		  ;; Run only the tests we found
		  (ert (concat "^" pattern "$")))))))

(defun cj/test-toggle-mode ()
  "Toggle between 'all and 'focused test execution modes."
  (interactive)
  (setq cj/test-mode (if (eq cj/test-mode 'all) 'focused 'all))
  (message "Test mode: %s" cj/test-mode))

(defun cj/test-view-focus ()
  "Display test files in focus."
  (interactive)
  (if (null cj/test-focused-files)
	  (message "No focused test files")
	(message "Focused files: %s"
			 (mapconcat 'identity cj/test-focused-files ", "))))

;;; Convenience function for smart execution

(defun cj/test-run-smart ()
  "Run tests based on current mode (all or focused)."
  (interactive)
  (if (eq cj/test-mode 'all)
	  (cj/test-run-all)
	(cj/test-run-focused)))

;; Test runner operations prefix and keymap
(define-prefix-command 'cj/test-map nil
					   "Keymap for test-runner operations.")
(define-key cj/custom-keymap "t" 'cj/test-map)
(define-key cj/test-map "L" 'cj/test-load-all)
(define-key cj/test-map "R" 'cj/test-run-all)
(define-key cj/test-map "r" 'cj/test-run-smart)
(define-key cj/test-map "a" 'cj/test-focus-add)
(define-key cj/test-map "c" 'cj/test-focus-clear)
(define-key cj/test-map "v" 'cj/test-view-focus)
(define-key cj/test-map "t" 'cj/test-toggle-mode)

(provide 'test-runner)
;;; test-runner.el ends here
