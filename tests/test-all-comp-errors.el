;;; test-all-comp-errors.el --- ERT tests for compilation errors -*- lexical-binding: t; -*-

;; Author: Claude Code and cjennings
;; Keywords: tests, compilation

;;; Commentary:
;; ERT tests to check all .el files in modules/ and custom/ directories
;; for byte-compilation and native-compilation errors.
;;
;; These tests help ensure code quality by catching compilation warnings
;; and errors across the entire configuration.

;;; Code:

(require 'ert)
(require 'testutil-general)
(require 'bytecomp)

;;; Configuration

(defvar test-comp-errors-directories '("modules" "custom")
  "List of directories to check for compilation errors.
Each directory path should be relative to the Emacs configuration root.
Example: '(\"modules\" \"custom\" \"libs\")")

(defvar test-comp-errors-single-file nil
  "If non-nil, test only this single file instead of all files.
Should be a relative path to a .el file (e.g., \"modules/ui-config.el\").
Useful for debugging specific file compilation issues.")

(defvar test-comp-errors-core-dependencies
  '("modules/user-constants.el"
    "modules/host-environment.el"
    "modules/system-defaults.el"
    "modules/keybindings.el")
  "List of core dependency files to pre-load before compilation.
These files are loaded before compilation starts to reduce recursion depth.
Should be files that many other files depend on.")

(defvar test-comp-errors-byte-compile-report-file
  (expand-file-name "~/.emacs-tests-byte-compile-errors.txt")
  "File path where byte-compilation error reports are written.
Only created when byte-compilation errors are detected.")

(defvar test-comp-errors-native-compile-report-file
  (expand-file-name "~/.emacs-tests-native-compile-errors.txt")
  "File path where native-compilation error reports are written.
Only created when native-compilation errors are detected.")

;;; Setup and Teardown

(defun test-comp-errors--preload-core-dependencies ()
  "Pre-load core dependency files to reduce recursion during compilation.
Loads files specified in 'test-comp-errors-core-dependencies'."
  ;; Ensure load-path includes modules, custom, and assets directories
  (let ((user-emacs-directory (expand-file-name default-directory)))
    (add-to-list 'load-path (concat user-emacs-directory "assets/"))
    (add-to-list 'load-path (concat user-emacs-directory "custom/"))
    (add-to-list 'load-path (concat user-emacs-directory "modules/")))

  (let ((max-lisp-eval-depth 3000))  ; Allow depth for loading core files
    (dolist (file test-comp-errors-core-dependencies)
      (let ((full-path (expand-file-name file)))
        (if (file-exists-p full-path)
            (condition-case err
                (progn
                  (message "Pre-loading core dependency: %s" full-path)
                  (load full-path nil t))
              (error
               (message "Warning: Could not pre-load core dependency %s: %s"
                        full-path (error-message-string err))))
          (message "Warning: Core dependency file not found: %s" full-path))))))

(defun test-comp-errors-setup (compile-type)
  "Set up test environment for compilation tests.
COMPILE-TYPE should be either 'byte or 'native."
  (cj/create-test-base-dir)
  (let ((subdir (format "compile-tests/%s/" (symbol-name compile-type))))
    (cj/create-test-subdirectory subdir))
  ;; Pre-load core dependencies to reduce recursion depth during compilation
  (test-comp-errors--preload-core-dependencies))

(defun test-comp-errors-teardown ()
  "Clean up test environment after compilation tests."
  (cj/delete-test-base-dir))

;;; Helper Functions

(defun test-comp-errors--get-compile-dir (compile-type)
  "Get the compilation output directory for COMPILE-TYPE ('byte or 'native)."
  (expand-file-name
   (format "compile-tests/%s/" (symbol-name compile-type))
   cj/test-base-dir))

(defun test-comp-errors--get-source-files ()
  "Return list of all .el files to test.
If 'test-comp-errors-single-file' is set, return only that file.
Otherwise, return all files in directories specified by 'test-comp-errors-directories'."
  (if test-comp-errors-single-file
      (if (file-exists-p test-comp-errors-single-file)
          (list test-comp-errors-single-file)
        (error "Single file does not exist: %s" test-comp-errors-single-file))
    (let ((all-files '()))
      (dolist (dir test-comp-errors-directories)
        (when (file-directory-p dir)
          (setq all-files
                (append all-files
                        (directory-files-recursively dir "\\.el$")))))
      all-files)))

(defun test-comp-errors--byte-compile-file (source-file output-dir)
  "Byte-compile SOURCE-FILE to OUTPUT-DIR.
Returns a list of (FILE . ERROR-MESSAGES) if errors occurred, nil otherwise."
  (let* ((max-lisp-eval-depth 3000)  ; Increase to handle deep dependency chains
         (byte-compile-dest-file-function
          (lambda (source)
            (expand-file-name
             (file-name-nondirectory (byte-compile-dest-file source))
             output-dir)))
         (byte-compile-log-buffer (get-buffer-create "*Byte-Compile-Test-Log*"))
         (errors nil))
    (with-current-buffer byte-compile-log-buffer
      (erase-buffer))
    ;; Attempt compilation
    (condition-case err
        (progn
          (byte-compile-file source-file)
          ;; Check log for warnings/errors
          (with-current-buffer byte-compile-log-buffer
            (goto-char (point-min))
            (let ((log-content (buffer-substring-no-properties (point-min) (point-max))))
              (when (or (string-match-p "Warning:" log-content)
                       (string-match-p "Error:" log-content))
                (setq errors (cons source-file log-content))))))
      (error
       (setq errors (cons source-file (error-message-string err)))))
    errors))

(defun test-comp-errors--native-compile-file (source-file output-dir)
  "Native-compile SOURCE-FILE to OUTPUT-DIR.
Returns a list of (FILE . ERROR-MESSAGES) if errors occurred, nil otherwise."
  (if (not (and (fboundp 'native-comp-available-p)
               (native-comp-available-p)))
      nil  ; Skip if native compilation not available
    (let* ((max-lisp-eval-depth 3000)  ; Increase to handle deep dependency chains
           (errors nil))
      ;; Set native-compile-target-directory dynamically
      ;; This variable must be dynamically bound, not lexically
      (setq native-compile-target-directory output-dir)
      (condition-case err
          (progn
            (native-compile source-file)
            ;; Native compile warnings go to *Warnings* buffer
            (when-let ((warnings-buf (get-buffer "*Warnings*")))
              (with-current-buffer warnings-buf
                (let ((log-content (buffer-substring-no-properties (point-min) (point-max))))
                  (when (and (> (length log-content) 0)
                            (string-match-p (regexp-quote (file-name-nondirectory source-file))
                                          log-content))
                    (setq errors (cons source-file log-content)))))))
        (error
         (setq errors (cons source-file (error-message-string err)))))
      errors)))

(defun test-comp-errors--format-error-report (errors)
  "Format ERRORS list into a readable report string.
ERRORS is a list of (FILE . ERROR-MESSAGES) cons cells."
  (if (null errors)
      ""
    (let ((report (format "\n\nCompilation errors found in %d file%s:\n\n"
                         (length errors)
                         (if (= (length errors) 1) "" "s")))
          (files-only ""))
      ;; First, show just the list of all affected files
      (setq files-only (concat "\nAffected files (" (number-to-string (length errors)) " total):\n"))
      (dolist (error-entry errors)
        (setq files-only (concat files-only "  - " (car error-entry) "\n")))
      (setq report (concat report files-only "\n"))

      ;; Then show detailed error messages for each file
      (setq report (concat report "Detailed error messages:\n\n"))
      (dolist (error-entry errors)
        (let ((file (car error-entry))
              (messages (cdr error-entry)))
          (setq report
                (concat report
                        (format "%s:\n" file)
                        (if (stringp messages)
                            (mapconcat (lambda (line)
                                        (concat "  " line))
                                      (split-string messages "\n" t)
                                      "\n")
                          messages)
                        "\n\n"))))
      report)))

;;; Tests

(ert-deftest test-byte-compile-all-files ()
  "Check all .el files in configured directories for byte-compilation errors.
Directories are specified by 'test-comp-errors-directories'."
  (test-comp-errors-setup 'byte)
  (unwind-protect
      (let* ((output-dir (test-comp-errors--get-compile-dir 'byte))
             (source-files (test-comp-errors--get-source-files))
             (errors '()))
        ;; Compile each file and collect errors
        (dolist (file source-files)
          (when-let ((error (test-comp-errors--byte-compile-file file output-dir)))
            (push error errors)))
        ;; Kill the compile log buffer
        (when-let ((buf (get-buffer "*Byte-Compile-Test-Log*")))
          (kill-buffer buf))
        ;; Write detailed error report to file for analysis (before teardown)
        (when errors
          (with-temp-file test-comp-errors-byte-compile-report-file
            (insert (test-comp-errors--format-error-report (nreverse errors))))
          (message "Full byte-compile error report written to: %s"
                   test-comp-errors-byte-compile-report-file))
        ;; Assert no errors
        (should (null errors)))
    (test-comp-errors-teardown)))

(ert-deftest test-native-compile-all-files ()
  "Check all .el files in configured directories for native-compilation errors.
Directories are specified by 'test-comp-errors-directories'."
  (unless (and (fboundp 'native-comp-available-p)
              (native-comp-available-p))
    (ert-skip "Native compilation not available"))
  (test-comp-errors-setup 'native)
  (unwind-protect
      (let* ((output-dir (test-comp-errors--get-compile-dir 'native))
             (source-files (test-comp-errors--get-source-files))
             (errors '()))
        ;; Clear warnings buffer
        (when-let ((buf (get-buffer "*Warnings*")))
          (with-current-buffer buf
            (erase-buffer)))
        ;; Compile each file and collect errors
        (dolist (file source-files)
          (when-let ((error (test-comp-errors--native-compile-file file output-dir)))
            (push error errors)))
        ;; Write detailed error report to file for analysis (before teardown)
        (when errors
          (with-temp-file test-comp-errors-native-compile-report-file
            (insert (test-comp-errors--format-error-report (nreverse errors))))
          (message "Full native-compile error report written to: %s"
                   test-comp-errors-native-compile-report-file))
        ;; Assert no errors
        (should (null errors)))
    (test-comp-errors-teardown)))

(provide 'test-all-comp-errors)
;;; test-all-comp-errors.el ends here
