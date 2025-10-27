;;; test-runner.el --- Test Runner for Emacs Configuration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:

;; This module provides a powerful ERT test runner with focus/unfocus workflow
;; for efficient test-driven development in Emacs Lisp projects.
;;
;; PURPOSE:
;;
;; When working on large Emacs Lisp projects with many test files, you often
;; want to focus on running just the tests relevant to your current work without
;; waiting for the entire suite to run. This module provides a smart test runner
;; that supports both running all tests and focusing on specific test files.
;;
;; WORKFLOW:
;;
;; 1. Run all tests initially to establish baseline (C-; t R)
;; 2. Add test files to focus while working on a feature (C-; t a)
;; 3. Run focused tests repeatedly as you develop (C-; t r)
;; 4. Add more test files as needed (C-; t b from within test buffer)
;; 5. View your focused test list at any time (C-; t v)
;; 6. Clear focus and run all tests before finishing (C-; t c, then C-; t R)
;;
;; PROJECT INTEGRATION:
;;
;; - Automatically discovers test directories in Projectile projects
;;   (looks for "test" or "tests" under project root)
;; - Falls back to ~/.emacs.d/tests if not in a Projectile project
;; - Test files must match pattern: test-*.el
;;
;; SPECIAL BEHAVIORS:
;;
;; - Smart test running: Automatically runs all or focused tests based on mode
;; - Test extraction: Discovers test names via regex to run specific tests
;; - At-point execution: Run individual test at cursor position (C-; t .)
;; - Error handling: Continues loading tests even if individual files fail
;;
;; KEYBINDINGS:
;;
;; C-; t L   Load all test files
;; C-; t R   Run all tests (full suite)
;; C-; t r   Run tests smartly (all or focused based on mode)
;; C-; t .   Run test at point
;; C-; t a   Add test file to focus (with completion)
;; C-; t b   Add current buffer's test file to focus
;; C-; t c   Clear all focused test files
;; C-; t v   View list of focused test files
;; C-; t t   Toggle mode between 'all and 'focused
;;
;; RECOMMENDED USAGE:
;;
;; While implementing a feature:
;;   - Add the main test file for the feature you're working on
;;   - Add any related test files that might be affected
;;   - Use C-; t r to repeatedly run just those focused tests
;;   - This provides fast feedback during development
;;
;; Before committing:
;;   - Clear the focus with C-; t c
;;   - Run the full suite with C-; t R to ensure nothing broke
;;   - Verify all tests pass before pushing changes
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

Either \\='all (run all tests) or \\='focused (run only focused tests).")

(defvar cj/test-last-results nil
  "Results from the last test run.")

;;; Core Functions

(defun cj/test--get-test-directory ()
  "Return the test directory path for the current project.

If in a Projectile project, prefers \\='test or \\='tests directory
inside the project root.  Falls back to `cj/test-global-directory'
if not found or not in a project."
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

(defun cj/test--get-test-files ()
  "Return a list of test file names (without path) in the appropriate test directory."
  (let ((dir (cj/test--get-test-directory)))
	(when (file-directory-p dir)
	  (mapcar #'file-name-nondirectory
			  (directory-files dir t "^test-.*\\.el$")))))

(defun cj/test--do-load-files (dir files)
  "Load test FILES from DIR.
Returns: (cons \\='success loaded-count) on success,
         (cons \\='error (list failed-files errors)) on errors."
  (let ((loaded-count 0)
        (errors '()))
    (dolist (file files)
      (condition-case err
          (progn
            (load-file file)
            (setq loaded-count (1+ loaded-count)))
        (error
         (push (cons (file-name-nondirectory file)
                     (error-message-string err))
               errors))))
    (if (null errors)
        (cons 'success loaded-count)
      (cons 'error (list loaded-count (nreverse errors))))))

(defun cj/test-load-all ()
  "Load all test files from the appropriate test directory."
  (interactive)
  (cj/test--ensure-test-dir-in-load-path)
  (let ((dir (cj/test--get-test-directory)))
	(unless (file-directory-p dir)
	  (user-error "Test directory %s does not exist" dir))
	(let ((test-files (directory-files dir t "^test-.*\\.el$")))
      (pcase (cj/test--do-load-files dir test-files)
        (`(success . ,count)
         (message "Loaded %d test file(s)" count))
        (`(error ,count ,errors)
         (dolist (err errors)
           (message "Error loading %s: %s" (car err) (cdr err)))
         (message "Loaded %d test file(s) with %d error(s)" count (length errors)))))))

(defun cj/test--do-focus-add (filename available-files focused-files)
  "Add FILENAME to focused test files.
AVAILABLE-FILES is the list of all available test files.
FOCUSED-FILES is the current list of focused files.
Returns: \\='success if added successfully,
         \\='already-focused if file is already focused,
         \\='not-available if file is not in available-files."
  (cond
   ((not (member filename available-files)) 'not-available)
   ((member filename focused-files) 'already-focused)
   (t 'success)))

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
        (pcase (cj/test--do-focus-add selected available-files cj/test-focused-files)
          ('success
           (push selected cj/test-focused-files)
           (message "Added to focus: %s" selected)
           (when (called-interactively-p 'interactive)
             (cj/test-view-focused)))
          ('already-focused
           (message "Already focused: %s" selected))
          ('not-available
           (user-error "File not available: %s" selected)))))))

(defun cj/test--do-focus-add-file (filepath testdir focused-files)
  "Validate and add FILEPATH to focused list.
TESTDIR is the test directory path.
FOCUSED-FILES is the current list of focused files.
Returns: \\='success if added successfully,
         \\='no-file if filepath is nil,
         \\='not-in-testdir if file is not inside test directory,
         \\='already-focused if file is already focused.
Second value is the relative filename if successful."
  (cond
   ((null filepath) (cons 'no-file nil))
   ((not (string-prefix-p (file-truename testdir) (file-truename filepath)))
    (cons 'not-in-testdir nil))
   (t
    (let ((relative (file-relative-name filepath testdir)))
      (if (member relative focused-files)
          (cons 'already-focused relative)
        (cons 'success relative))))))

(defun cj/test-focus-add-this-buffer-file ()
  "Add the current buffer's file to the focused test list."
  (interactive)
  (let ((file (buffer-file-name))
        (dir (cj/test--get-test-directory)))
    (pcase (cj/test--do-focus-add-file file dir cj/test-focused-files)
      (`(no-file . ,_)
       (user-error "Current buffer is not visiting a file"))
      (`(not-in-testdir . ,_)
       (user-error "File is not inside the test directory: %s" dir))
      (`(already-focused . ,relative)
       (message "Already focused: %s" relative))
      (`(success . ,relative)
       (push relative cj/test-focused-files)
       (message "Added to focus: %s" relative)
       (when (called-interactively-p 'interactive)
         (cj/test-view-focused))))))

(defun cj/test--do-focus-remove (filename focused-files)
  "Remove FILENAME from FOCUSED-FILES.
Returns: \\='success if removed successfully,
         \\='empty-list if focused-files is empty,
         \\='not-found if filename is not in focused-files."
  (cond
   ((null focused-files) 'empty-list)
   ((not (member filename focused-files)) 'not-found)
   (t 'success)))

(defun cj/test-focus-remove ()
  "Remove a test file from the focused list."
  (interactive)
  (if (null cj/test-focused-files)
	  (user-error "No focused files to remove")
	(let ((selected (completing-read "Remove from focus: "
									 cj/test-focused-files
									 nil t)))
      (pcase (cj/test--do-focus-remove selected cj/test-focused-files)
        ('success
         (setq cj/test-focused-files
               (delete selected cj/test-focused-files))
         (message "Removed from focus: %s" selected)
         (when (called-interactively-p 'interactive)
           (cj/test-view-focused)))
        ('not-found
         (message "File not in focused list: %s" selected))
        ('empty-list
         (user-error "No focused files to remove"))))))

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

(defun cj/test--do-get-focused-tests (focused-files test-dir)
  "Get test names from FOCUSED-FILES in TEST-DIR.
Returns: (cons \\='success (list test-names loaded-count)) if successful,
         (cons \\='no-tests nil) if no tests found,
         (cons \\='empty-list nil) if focused-files is empty."
  (if (null focused-files)
      (cons 'empty-list nil)
    (let ((all-test-names '())
          (loaded-count 0))
      (dolist (file focused-files)
        (let ((full-path (expand-file-name file test-dir)))
          (when (file-exists-p full-path)
            (load-file full-path)
            (setq loaded-count (1+ loaded-count))
            (let ((test-names (cj/test--extract-test-names full-path)))
              (setq all-test-names (append all-test-names test-names))))))
      (if (null all-test-names)
          (cons 'no-tests nil)
        (cons 'success (list all-test-names loaded-count))))))

(defun cj/test-run-focused ()
  "Run only the focused test files."
  (interactive)
  (let ((dir (cj/test--get-test-directory)))
    (pcase (cj/test--do-get-focused-tests cj/test-focused-files dir)
      (`(empty-list . ,_)
       (user-error "No focused files set. Use =cj/test-focus-add' first"))
      (`(no-tests . ,_)
       (message "No tests found in focused files"))
      (`(success ,test-names ,loaded-count)
       (let ((pattern (regexp-opt test-names)))
         (message "Running %d test(s) from %d focused file(s)"
                  (length test-names) loaded-count)
         (ert (concat "^" pattern "$")))))))

(defun cj/test--ensure-test-dir-in-load-path ()
  "Ensure the directory returned by cj/test--get-test-directory is in `load-path`."
  (let ((dir (cj/test--get-test-directory)))
	(when (and dir (file-directory-p dir))
	  (add-to-list 'load-path dir))))

(defun cj/test--extract-test-at-pos ()
  "Extract test name at current position.
Returns: test name symbol if found, nil otherwise."
  (save-excursion
    (beginning-of-defun)
    (condition-case nil
        (let ((form (read (current-buffer))))
          (when (and (listp form)
                     (eq (car form) 'ert-deftest)
                     (symbolp (cadr form)))
            (cadr form)))
      (error nil))))

(defun cj/run-test-at-point ()
  "Run the ERT test at point.
If point is inside an `ert-deftest` definition, run that test only.
Otherwise, message that no test is found."
  (interactive)
  (let ((test-name (cj/test--extract-test-at-pos)))
    (if test-name
        (ert test-name)
      (message "Not in an ERT test method."))))

(defun cj/test-run-all ()
  "Load and run all tests."
  (interactive)
  (cj/test-load-all)
  (ert t))

(defun cj/test-toggle-mode ()
  "Toggle between 'all and 'focused test execution modes."
  (interactive)
  (setq cj/test-mode (if (eq cj/test-mode 'all) 'focused 'all))
  (message "Test mode: %s" cj/test-mode))

(defun cj/test-view-focused ()
  "Display test files in focus."
  (interactive)
  (if (null cj/test-focused-files)
	  (message "No focused test files")
	(message "Focused files: %s"
			 (mapconcat 'identity cj/test-focused-files ", "))))

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

;; which-key integration
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; t" "test runner menu"
    "C-; t L" "load all tests"
    "C-; t R" "run all tests"
    "C-; t r" "run smart"
    "C-; t ." "run test at point"
    "C-; t a" "add to focus"
    "C-; t b" "add buffer to focus"
    "C-; t c" "clear focus"
    "C-; t v" "view focused"
    "C-; t t" "toggle mode"))

(provide 'test-runner)
;;; test-runner.el ends here
