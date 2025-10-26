;;; test-runner.el --- Test Runner for Emacs Configuration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Provides utilities for running ERT tests with focus/unfocus workflow
;;
;; Tests should be located in the Projectile project test directories,
;; typically "test" or "tests" under the project root.
;; Falls back to =~/.emacs.d/tests= if not in a Projectile project.
;;
;; The default mode is to load and run all tests.
;;
;; To focus on running a specific set of test files:
;; - Toggle the mode to "focus" mode
;; - Add specific test files to the list of tests in "focus"
;; - Running tests (smartly) will now just run those tests
;;
;; Don't forget to run all tests again in default mode at least once before finishing.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Variables

(defvar cj/test-global-directory nil
  "Fallback global test directory when not in a Projectile project.")

(defvar cj/test-focused-files '()
  "List of test files for focused test execution.

Each element is a filename (without path) to run.")

(defvar cj/test-mode 'all
  "Current test execution mode.

Either 'all (run all tests) or 'focused (run only focused tests).")

(defvar cj/test-last-results nil
  "Results from the last test run.")

;;; Core Functions

;;;###autoload
(defun cj/test--get-test-directory ()
  "Return the test directory path for the current project.

If in a Projectile project, prefers a 'test' or 'tests' directory inside the project root.
Falls back to =cj/test-global-directory= if not found or not in a project."
  (require 'projectile)
  (let ((project-root (ignore-errors (projectile-project-root))))
	(if (not (and project-root (file-directory-p project-root)))
		;; fallback global test directory
		cj/test-global-directory
	  (let ((test-dir (expand-file-name "test" project-root))
			(tests-dir (expand-file-name "tests" project-root)))
		(cond
		 ((file-directory-p test-dir) test-dir)
		 ((file-directory-p tests-dir) tests-dir)
		 (t cj/test-global-directory))))))

;;;###autoload
(defun cj/test--get-test-files ()
  "Return a list of test file names (without path) in the appropriate test directory."
  (let ((dir (cj/test--get-test-directory)))
	(when (file-directory-p dir)
	  (mapcar #'file-name-nondirectory
			  (directory-files dir t "^test-.*\\.el$")))))

;;;###autoload
(defun cj/test-load-all ()
  "Load all test files from the appropriate test directory."
  (interactive)
  (cj/test--ensure-test-dir-in-load-path)
  (let ((dir (cj/test--get-test-directory)))
	(unless (file-directory-p dir)
	  (user-error "Test directory %s does not exist" dir))
	(let ((test-files (directory-files dir t "^test-.*\\.el$"))
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
	  (message "Loaded %d test file(s)" loaded-count))))

;;;###autoload
(defun cj/test-focus-add ()
  "Select test file(s) to add to the focused list."
  (interactive)
  (cj/test--ensure-test-dir-in-load-path)
  (let* ((dir (cj/test--get-test-directory))
		 (available-files (when (file-directory-p dir)
							(mapcar #'file-name-nondirectory
									(directory-files dir t "^test-.*\\.el$")))))
	(if (null available-files)
		(user-error "No test files found in %s" dir)
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

;;;###autoload
(defun cj/test-focus-add-this-buffer-file ()
  "Add the current buffer's file to the focused test list."
  (interactive)
  (let ((file (buffer-file-name))
        (dir (cj/test--get-test-directory)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (unless (string-prefix-p (file-truename dir) (file-truename file))
      (user-error "File is not inside the test directory: %s" dir))
	(let ((relative (file-relative-name file dir)))
	  (if (member relative cj/test-focused-files)
		  (message "Already focused: %s" relative)
		(push relative cj/test-focused-files)
		(message "Added to focus: %s" relative)
		(when (called-interactively-p 'interactive)
		  (cj/test-view-focused))))))

;;;###autoload
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

;;;###autoload
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
;;	  (while (re-search-forward "^\s-*(ert-deftest\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
	  (while (re-search-forward "^[[:space:]]*(ert-deftest[[:space:]]+\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
		(push (match-string 1) test-names)))
	test-names))

;;;###autoload
(defun cj/test-run-focused ()
  "Run only the focused test files."
  (interactive)
  (if (null cj/test-focused-files)
	  (user-error "No focused files set. Use =cj/test-focus-add' first")
	(let ((all-test-names '())
		  (loaded-count 0)
		  (dir (cj/test--get-test-directory)))
	  ;; Load the focused files and collect their test names
	  (dolist (file cj/test-focused-files)
		(let ((full-path (expand-file-name file dir)))
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

(defun cj/test--ensure-test-dir-in-load-path ()
  "Ensure the directory returned by cj/test--get-test-directory is in `load-path`."
  (let ((dir (cj/test--get-test-directory)))
	(when (and dir (file-directory-p dir))
	  (add-to-list 'load-path dir))))

;;;###autoload
(defun cj/run-test-at-point ()
  "Run the ERT test at point.
If point is inside an `ert-deftest` definition, run that test only.
Otherwise, message that no test is found."
  (interactive)
  (let ((original-point (point)))
	(save-excursion
	  (beginning-of-defun)
	  (condition-case nil
		  (let ((form (read (current-buffer))))
			(if (and (listp form)
					 (eq (car form) 'ert-deftest)
					 (symbolp (cadr form)))
				(ert (cadr form))
			  (message "Not in an ERT test method.")))
		(error (message "No ERT test methods found at point."))))
	(goto-char original-point)))

;;;###autoload
(defun cj/test-run-all ()
  "Load and run all tests."
  (interactive)
  (cj/test-load-all)
  (ert t))

;;;###autoload
(defun cj/test-toggle-mode ()
  "Toggle between 'all and 'focused test execution modes."
  (interactive)
  (setq cj/test-mode (if (eq cj/test-mode 'all) 'focused 'all))
  (message "Test mode: %s" cj/test-mode))

;;;###autoload
(defun cj/test-view-focused ()
  "Display test files in focus."
  (interactive)
  (if (null cj/test-focused-files)
	  (message "No focused test files")
	(message "Focused files: %s"
			 (mapconcat 'identity cj/test-focused-files ", "))))

;;;###autoload
(defun cj/test-run-smart ()
  "Run tests based on current mode (all or focused)."
  (interactive)
  (if (eq cj/test-mode 'all)
	  (cj/test-run-all)
	(cj/test-run-focused)))

;; Test runner operations prefix and keymap
(defvar-keymap cj/testrunner-map
  :doc "Keymap for test-runner operations"
  "L" #'cj/test-load-all
  "R" #'cj/test-run-all
  "." #'cj/run-test-at-point
  "r" #'cj/test-run-smart
  "a" #'cj/test-focus-add
  "b" #'cj/test-focus-add-this-buffer-file
  "c" #'cj/test-focus-clear
  "v" #'cj/test-view-focused
  "t" #'cj/test-toggle-mode)

(keymap-set cj/custom-keymap "t" cj/testrunner-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; t" "test runner menu"))

(provide 'test-runner)
;;; test-runner.el ends here
