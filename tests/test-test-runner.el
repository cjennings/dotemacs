;;; test-test-runner.el --- Tests for test-runner.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for test-runner.el - ERT test runner with focus/unfocus workflow.
;;
;; Testing approach:
;; - Tests focus on internal `cj/test--do-*` functions (pure business logic)
;; - File system operations use temp directories
;; - Tests are isolated with setup/teardown
;; - Tests verify return values, not user messages

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load the module (ignore keymap error in batch mode)
(condition-case nil
    (require 'test-runner)
  (error nil))

;;; Test Utilities

(defvar test-testrunner--temp-dir nil
  "Temporary directory for test files during tests.")

(defvar test-testrunner--original-focused-files nil
  "Backup of focused files list before test.")

(defun test-testrunner-setup ()
  "Setup test environment before each test."
  ;; Backup current state
  (setq test-testrunner--original-focused-files cj/test-focused-files)
  ;; Reset to clean state
  (setq cj/test-focused-files '())
  ;; Create temp directory for file tests
  (setq test-testrunner--temp-dir (make-temp-file "test-runner-test" t)))

(defun test-testrunner-teardown ()
  "Clean up test environment after each test."
  ;; Restore state
  (setq cj/test-focused-files test-testrunner--original-focused-files)
  ;; Clean up temp directory
  (when (and test-testrunner--temp-dir
             (file-directory-p test-testrunner--temp-dir))
    (delete-directory test-testrunner--temp-dir t))
  (setq test-testrunner--temp-dir nil))

(defun test-testrunner-create-test-file (filename content)
  "Create test file FILENAME with CONTENT in temp directory."
  (let ((filepath (expand-file-name filename test-testrunner--temp-dir)))
    (with-temp-file filepath
      (insert content))
    filepath))

;;; Normal Cases - Load Files

(ert-deftest test-testrunner-load-files-success ()
  "Should successfully load test files."
  (test-testrunner-setup)
  (let* ((file1 (test-testrunner-create-test-file "test-simple.el"
                                                    "(defun test-func () t)"))
         (file2 (test-testrunner-create-test-file "test-other.el"
                                                    "(defun other-func () nil)"))
         (result (cj/test--do-load-files test-testrunner--temp-dir
                                         (list file1 file2))))
    (should (eq (car result) 'success))
    (should (= (cdr result) 2)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-load-files-with-errors ()
  "Should handle errors during file loading."
  (test-testrunner-setup)
  (let* ((good-file (test-testrunner-create-test-file "test-good.el"
                                                        "(defun good () t)"))
         (bad-file (test-testrunner-create-test-file "test-bad.el"
                                                       "(defun bad ( "))
         (result (cj/test--do-load-files test-testrunner--temp-dir
                                         (list good-file bad-file))))
    (should (eq (car result) 'error))
    (should (= (nth 1 result) 1))  ; loaded-count
    (should (= (length (nth 2 result)) 1)))  ; errors list
  (test-testrunner-teardown))

;;; Normal Cases - Focus Add

(ert-deftest test-testrunner-focus-add-success ()
  "Should successfully add file to focus."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-add "test-foo.el"
                                       '("test-foo.el" "test-bar.el")
                                       '())))
    (should (eq result 'success)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-add-already-focused ()
  "Should detect already focused file."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-add "test-foo.el"
                                       '("test-foo.el" "test-bar.el")
                                       '("test-foo.el"))))
    (should (eq result 'already-focused)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-add-not-available ()
  "Should detect file not in available list."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-add "test-missing.el"
                                       '("test-foo.el" "test-bar.el")
                                       '())))
    (should (eq result 'not-available)))
  (test-testrunner-teardown))

;;; Normal Cases - Focus Add File

(ert-deftest test-testrunner-focus-add-file-success ()
  "Should successfully validate and add file to focus."
  (test-testrunner-setup)
  (let* ((filepath (expand-file-name "test-foo.el" test-testrunner--temp-dir))
         (result (cj/test--do-focus-add-file filepath test-testrunner--temp-dir '())))
    (should (eq (car result) 'success))
    (should (string= (cdr result) "test-foo.el")))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-add-file-no-file ()
  "Should detect nil filepath."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-add-file nil test-testrunner--temp-dir '())))
    (should (eq (car result) 'no-file)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-add-file-not-in-testdir ()
  "Should detect file outside test directory."
  (test-testrunner-setup)
  (let* ((filepath "/tmp/outside-test.el")
         (result (cj/test--do-focus-add-file filepath test-testrunner--temp-dir '())))
    (should (eq (car result) 'not-in-testdir)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-add-file-already-focused ()
  "Should detect already focused file."
  (test-testrunner-setup)
  (let* ((filepath (expand-file-name "test-foo.el" test-testrunner--temp-dir))
         (result (cj/test--do-focus-add-file filepath
                                             test-testrunner--temp-dir
                                             '("test-foo.el"))))
    (should (eq (car result) 'already-focused))
    (should (string= (cdr result) "test-foo.el")))
  (test-testrunner-teardown))

;;; Normal Cases - Focus Remove

(ert-deftest test-testrunner-focus-remove-success ()
  "Should successfully remove file from focus."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-remove "test-foo.el" '("test-foo.el" "test-bar.el"))))
    (should (eq result 'success)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-remove-empty-list ()
  "Should detect empty focused list."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-remove "test-foo.el" '())))
    (should (eq result 'empty-list)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-remove-not-found ()
  "Should detect file not in focused list."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-remove "test-missing.el" '("test-foo.el"))))
    (should (eq result 'not-found)))
  (test-testrunner-teardown))

;;; Normal Cases - Get Focused Tests

(ert-deftest test-testrunner-get-focused-tests-success ()
  "Should extract test names from focused files."
  (test-testrunner-setup)
  (let* ((file1 (test-testrunner-create-test-file "test-first.el"
                   "(ert-deftest test-alpha-one () (should t))\n(ert-deftest test-alpha-two () (should t))"))
         (result (cj/test--do-get-focused-tests '("test-first.el") test-testrunner--temp-dir)))
    (should (eq (car result) 'success))
    (should (= (length (nth 1 result)) 2))  ; 2 test names
    (should (= (nth 2 result) 1)))  ; 1 file loaded
  (test-testrunner-teardown))

(ert-deftest test-testrunner-get-focused-tests-empty-list ()
  "Should detect empty focused files list."
  (test-testrunner-setup)
  (let ((result (cj/test--do-get-focused-tests '() test-testrunner--temp-dir)))
    (should (eq (car result) 'empty-list)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-get-focused-tests-no-tests ()
  "Should detect when no tests found in files."
  (test-testrunner-setup)
  (test-testrunner-create-test-file "test-empty.el" "(defun not-a-test () t)")
  (let ((result (cj/test--do-get-focused-tests '("test-empty.el") test-testrunner--temp-dir)))
    (should (eq (car result) 'no-tests)))
  (test-testrunner-teardown))

;;; Normal Cases - Extract Test Names

(ert-deftest test-testrunner-extract-test-names-simple ()
  "Should extract test names from file."
  (test-testrunner-setup)
  (let* ((file (test-testrunner-create-test-file "test-simple.el"
                  "(ert-deftest test-foo () (should t))\n(ert-deftest test-bar () (should nil))"))
         (names (cj/test--extract-test-names file)))
    (should (= (length names) 2))
    (should (member "test-foo" names))
    (should (member "test-bar" names)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-extract-test-names-with-whitespace ()
  "Should extract test names with various whitespace."
  (test-testrunner-setup)
  (let* ((file (test-testrunner-create-test-file "test-whitespace.el"
                  "(ert-deftest  test-spaces  () (should t))\n  (ert-deftest test-indent () t)"))
         (names (cj/test--extract-test-names file)))
    (should (= (length names) 2))
    (should (member "test-spaces" names))
    (should (member "test-indent" names)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-extract-test-names-no-tests ()
  "Should return empty list when no tests in file."
  (test-testrunner-setup)
  (let* ((file (test-testrunner-create-test-file "test-none.el"
                  "(defun not-a-test () t)"))
         (names (cj/test--extract-test-names file)))
    (should (null names)))
  (test-testrunner-teardown))

;;; Normal Cases - Extract Test at Position

(ert-deftest test-testrunner-extract-test-at-pos-found ()
  "Should extract test name at point."
  (test-testrunner-setup)
  (with-temp-buffer
    (insert "(ert-deftest test-sample ()\n  (should t))")
    (goto-char (point-min))
    (let ((name (cj/test--extract-test-at-pos)))
      (should (eq name 'test-sample))))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-extract-test-at-pos-not-found ()
  "Should return nil when not in a test."
  (test-testrunner-setup)
  (with-temp-buffer
    (insert "(defun regular-function ()\n  (message \"hi\"))")
    (goto-char (point-min))
    (let ((name (cj/test--extract-test-at-pos)))
      (should (null name))))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-extract-test-at-pos-invalid-syntax ()
  "Should return nil for invalid syntax."
  (test-testrunner-setup)
  (with-temp-buffer
    (insert "(ert-deftest")
    (goto-char (point-min))
    (let ((name (cj/test--extract-test-at-pos)))
      (should (null name))))
  (test-testrunner-teardown))

;;; Boundary Cases - Load Files

(ert-deftest test-testrunner-load-files-empty-list ()
  "Should handle empty file list."
  (test-testrunner-setup)
  (let ((result (cj/test--do-load-files test-testrunner--temp-dir '())))
    (should (eq (car result) 'success))
    (should (= (cdr result) 0)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-load-files-nonexistent ()
  "Should handle nonexistent files."
  (test-testrunner-setup)
  (let* ((fake-file (expand-file-name "nonexistent.el" test-testrunner--temp-dir))
         (result (cj/test--do-load-files test-testrunner--temp-dir (list fake-file))))
    (should (eq (car result) 'error))
    (should (= (nth 1 result) 0)))  ; 0 files loaded
  (test-testrunner-teardown))

;;; Boundary Cases - Focus Add

(ert-deftest test-testrunner-focus-add-single-available ()
  "Should add when only one file available."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-add "test-only.el" '("test-only.el") '())))
    (should (eq result 'success)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-focus-add-case-sensitive ()
  "Should be case-sensitive for filenames."
  (test-testrunner-setup)
  (let ((result (cj/test--do-focus-add "Test-Foo.el"
                                       '("test-foo.el")
                                       '())))
    (should (eq result 'not-available)))
  (test-testrunner-teardown))

;;; Boundary Cases - Get Focused Tests

(ert-deftest test-testrunner-get-focused-tests-multiple-files ()
  "Should collect tests from multiple files."
  (test-testrunner-setup)
  (test-testrunner-create-test-file "test-first.el"
    "(ert-deftest test-beta-one () t)")
  (test-testrunner-create-test-file "test-second.el"
    "(ert-deftest test-beta-two () t)")
  (let ((result (cj/test--do-get-focused-tests '("test-first.el" "test-second.el")
                                               test-testrunner--temp-dir)))
    (should (eq (car result) 'success))
    (should (= (length (nth 1 result)) 2))  ; 2 tests total
    (should (= (nth 2 result) 2)))  ; 2 files loaded
  (test-testrunner-teardown))

(ert-deftest test-testrunner-get-focused-tests-skip-nonexistent ()
  "Should skip nonexistent files."
  (test-testrunner-setup)
  (test-testrunner-create-test-file "test-exists.el"
    "(ert-deftest test-gamma-one () t)")
  (let ((result (cj/test--do-get-focused-tests '("test-exists.el" "test-missing.el")
                                               test-testrunner--temp-dir)))
    (should (eq (car result) 'success))
    (should (= (length (nth 1 result)) 1))  ; 1 test found
    (should (= (nth 2 result) 1)))  ; 1 file loaded (missing skipped)
  (test-testrunner-teardown))

;;; Boundary Cases - Extract Test Names

(ert-deftest test-testrunner-extract-test-names-hyphens-underscores ()
  "Should handle test names with hyphens and underscores."
  (test-testrunner-setup)
  (let* ((file (test-testrunner-create-test-file "test-names.el"
                  "(ert-deftest test-with-hyphens () t)\n(ert-deftest test_with_underscores () t)"))
         (names (cj/test--extract-test-names file)))
    (should (= (length names) 2))
    (should (member "test-with-hyphens" names))
    (should (member "test_with_underscores" names)))
  (test-testrunner-teardown))

(ert-deftest test-testrunner-extract-test-names-ignore-comments ()
  "Should not extract test names from comments."
  (test-testrunner-setup)
  (let* ((file (test-testrunner-create-test-file "test-comments.el"
                  ";; (ert-deftest test-commented () t)\n(ert-deftest test-real () t)"))
         (names (cj/test--extract-test-names file)))
    (should (= (length names) 1))
    (should (member "test-real" names)))
  (test-testrunner-teardown))

(provide 'test-test-runner)
;;; test-test-runner.el ends here
