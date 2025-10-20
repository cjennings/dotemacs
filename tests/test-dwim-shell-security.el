;;; test-dwim-shell-security.el --- ERT tests for dwim-shell-config security functions -*- lexical-binding: t; -*-

;; Author: Claude Code and cjennings
;; Keywords: tests, dwim-shell, security

;;; Commentary:
;; ERT tests for security-related dwim-shell-config.el functions.
;; Tests are organized into normal, boundary, and error cases.
;;
;; These tests verify that password-protected operations:
;; - Do not expose passwords in process lists or command output
;; - Use temporary files with restrictive permissions (mode 600)
;; - Clean up temporary files after use (even on error)
;; - Properly handle edge cases and errors

;;; Code:

(require 'ert)
(require 'dwim-shell-config)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-dwim-shell-security-setup ()
  "Set up test environment for dwim-shell-security tests."
  (cj/create-test-base-dir)
  ;; Create test PDF file
  (setq test-pdf-file (expand-file-name "test.pdf" cj/test-base-dir))
  ;; Create minimal valid PDF (this is a minimal PDF structure)
  (with-temp-file test-pdf-file
    (insert "%PDF-1.4\n")
    (insert "1 0 obj\n<< /Type /Catalog /Pages 2 0 R >>\nendobj\n")
    (insert "2 0 obj\n<< /Type /Pages /Kids [3 0 R] /Count 1 >>\nendobj\n")
    (insert "3 0 obj\n<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] >>\nendobj\n")
    (insert "xref\n0 4\n")
    (insert "0000000000 65535 f\n")
    (insert "0000000009 00000 n\n")
    (insert "0000000058 00000 n\n")
    (insert "0000000115 00000 n\n")
    (insert "trailer\n<< /Size 4 /Root 1 0 R >>\nstartxref\n203\n%%EOF\n"))
  ;; Create test files for archive operations
  (setq test-file-1 (expand-file-name "file1.txt" cj/test-base-dir))
  (setq test-file-2 (expand-file-name "file2.txt" cj/test-base-dir))
  (with-temp-file test-file-1 (insert "Test content 1"))
  (with-temp-file test-file-2 (insert "Test content 2")))

(defun test-dwim-shell-security-teardown ()
  "Clean up test environment after dwim-shell-security tests."
  ;; Clean up test directory
  (cj/delete-test-base-dir))

;;; Helper Functions

(defun test-dwim-check-temp-file-cleanup (pattern)
  "Check that no temporary files matching PATTERN remain after operation."
  (let ((temp-files (directory-files temporary-file-directory nil pattern)))
    (should (null temp-files))))

(defun test-dwim-check-file-permissions (file expected-mode)
  "Check that FILE has EXPECTED-MODE permissions."
  (when (file-exists-p file)
    (should (equal (file-modes file) expected-mode))))

;;; Normal Cases - PDF Password Protect

(ert-deftest test-dwim-pdf-password-protect-creates-temp-file-normal ()
  "Normal: PDF password protect creates temporary file with secure permissions."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let* ((captured-temp-file nil)
             (original-make-temp-file (symbol-function 'make-temp-file)))
        ;; Wrap make-temp-file to capture the temp file path
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest args)
                     (setq captured-temp-file (apply original-make-temp-file args))
                     captured-temp-file))
                  ;; Mock read-passwd to avoid interactive prompts
                  ((symbol-function 'read-passwd)
                   (lambda (_prompt) "test-password"))
                  ;; Mock dwim-shell-command-on-marked-files to check behavior
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description command &rest _args)
                     ;; Verify temp file exists with correct permissions during execution
                     (should (file-exists-p captured-temp-file))
                     (test-dwim-check-file-permissions captured-temp-file #o600)
                     ;; Verify password is in temp file, not in command
                     (should (string-match-p captured-temp-file command))
                     (should-not (string-match-p "test-password" command)))))
          (cj/dwim-shell-commands-pdf-password-protect)
          ;; Verify temp file is cleaned up after operation
          (should-not (file-exists-p captured-temp-file))))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-pdf-password-protect-no-password-in-command-normal ()
  "Normal: Password does not appear in shell command string."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let ((test-password "SuperSecret123!"))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (_prompt) test-password))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description command &rest _args)
                     ;; Password should NOT appear in command
                     (should-not (string-match-p test-password command))
                     ;; Command should reference password file
                     (should (string-match-p "--password-file=" command)))))
          (cj/dwim-shell-commands-pdf-password-protect)))
    (test-dwim-shell-security-teardown)))

;;; Normal Cases - PDF Password Unprotect

(ert-deftest test-dwim-pdf-password-unprotect-creates-temp-file-normal ()
  "Normal: PDF password unprotect creates temporary file with secure permissions."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let* ((captured-temp-file nil)
             (original-make-temp-file (symbol-function 'make-temp-file)))
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest args)
                     (setq captured-temp-file (apply original-make-temp-file args))
                     captured-temp-file))
                  ((symbol-function 'read-passwd)
                   (lambda (_prompt) "test-password"))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description command &rest _args)
                     (should (file-exists-p captured-temp-file))
                     (test-dwim-check-file-permissions captured-temp-file #o600)
                     (should (string-match-p captured-temp-file command))
                     (should-not (string-match-p "test-password" command)))))
          (cj/dwim-shell-commands-pdf-password-unprotect)
          (should-not (file-exists-p captured-temp-file))))
    (test-dwim-shell-security-teardown)))

;;; Normal Cases - Create Encrypted Archive

(ert-deftest test-dwim-create-encrypted-zip-uses-7z-normal ()
  "Normal: Create encrypted archive uses 7z, not zip."
  (skip-unless (executable-find "7z"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (_prompt) "test-password"))
                ((symbol-function 'read-string)
                 (lambda (_prompt &optional _default) "test-archive"))
                ((symbol-function 'dwim-shell-command-on-marked-files)
                 (lambda (_description command &rest args)
                   ;; Should use 7z, not zip
                   (should (string-match-p "7z a" command))
                   (should-not (string-match-p "zip -" command))
                   ;; Should use AES encryption
                   (should (string-match-p "-mhe=on" command))
                   ;; Verify utils parameter is 7z
                   (should (equal (plist-get args :utils) "7z")))))
        (cj/dwim-shell-commands-create-encrypted-zip))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-create-encrypted-zip-no-password-in-command-normal ()
  "Normal: Password does not appear in shell command string for archive creation."
  (skip-unless (executable-find "7z"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let ((test-password "VerySecret456!"))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (_prompt) test-password))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional _default) "test-archive"))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description command &rest _args)
                     ;; Password should NOT appear directly in command
                     (should-not (string-match-p test-password command))
                     ;; Should use cat to read from temp file
                     (should (string-match-p "cat" command)))))
          (cj/dwim-shell-commands-create-encrypted-zip)))
    (test-dwim-shell-security-teardown)))

;;; Normal Cases - Remove Archive Encryption

(ert-deftest test-dwim-remove-zip-encryption-uses-7z-normal ()
  "Normal: Remove archive encryption uses 7z for both extract and create."
  (skip-unless (executable-find "7z"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (_prompt) "test-password"))
                ((symbol-function 'dwim-shell-command-on-marked-files)
                 (lambda (_description command &rest args)
                   ;; Should use 7z for both extract and archive
                   (should (string-match-p "7z x" command))
                   (should (string-match-p "7z a" command))
                   ;; Verify utils parameter is 7z
                   (should (equal (plist-get args :utils) "7z")))))
        (cj/dwim-shell-commands-remove-zip-encryption))
    (test-dwim-shell-security-teardown)))

;;; Boundary Cases

(ert-deftest test-dwim-pdf-password-empty-password-boundary ()
  "Boundary: Empty password is accepted (though qpdf may reject it)."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let ((command-executed nil))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (_prompt) ""))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description _command &rest _args)
                     (setq command-executed t))))
          (cj/dwim-shell-commands-pdf-password-protect)
          ;; Function should accept empty password (tool may reject later)
          (should command-executed)))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-pdf-password-special-characters-boundary ()
  "Boundary: Password with special characters is properly handled."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let ((special-password "p@$$w0rd!#%^&*()"))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (_prompt) special-password))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description command &rest _args)
                     ;; Special characters should not appear in command
                     (should-not (string-match-p (regexp-quote special-password) command)))))
          (cj/dwim-shell-commands-pdf-password-protect)))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-archive-very-long-password-boundary ()
  "Boundary: Very long password (1000+ chars) is properly handled."
  (skip-unless (executable-find "7z"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let ((long-password (make-string 1000 ?x))
            (captured-temp-file nil))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (_prompt) long-password))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional _default) "test"))
                  ((symbol-function 'make-temp-file)
                   (lambda (&rest args)
                     (setq captured-temp-file (apply (symbol-function 'make-temp-file) args))
                     captured-temp-file))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description _command &rest _args)
                     ;; Verify password was written to temp file
                     (with-temp-buffer
                       (insert-file-contents captured-temp-file)
                       (should (equal (buffer-string) long-password))))))
          (cj/dwim-shell-commands-create-encrypted-zip)))
    (test-dwim-shell-security-teardown)))

;;; Error Cases

(ert-deftest test-dwim-pdf-password-temp-file-cleanup-on-error-error ()
  "Error: Temporary file is cleaned up even when command fails."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let* ((captured-temp-file nil)
             (original-make-temp-file (symbol-function 'make-temp-file)))
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest args)
                     (setq captured-temp-file (apply original-make-temp-file args))
                     captured-temp-file))
                  ((symbol-function 'read-passwd)
                   (lambda (_prompt) "test-password"))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description _command &rest _args)
                     ;; Simulate command failure
                     (error "Command failed"))))
          ;; Should error, but still clean up temp file
          (should-error (cj/dwim-shell-commands-pdf-password-protect))
          ;; Temp file should be cleaned up despite error
          (should-not (file-exists-p captured-temp-file))))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-archive-temp-file-cleanup-on-error-error ()
  "Error: Archive temp file cleaned up even when 7z command fails."
  (skip-unless (executable-find "7z"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (let* ((captured-temp-file nil)
             (original-make-temp-file (symbol-function 'make-temp-file)))
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest args)
                     (setq captured-temp-file (apply original-make-temp-file args))
                     captured-temp-file))
                  ((symbol-function 'read-passwd)
                   (lambda (_prompt) "test-password"))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional _default) "test"))
                  ((symbol-function 'dwim-shell-command-on-marked-files)
                   (lambda (_description _command &rest _args)
                     (error "7z command failed"))))
          (should-error (cj/dwim-shell-commands-create-encrypted-zip))
          (should-not (file-exists-p captured-temp-file))))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-pdf-password-temp-file-write-error-error ()
  "Error: Error when unable to write to temporary file."
  (skip-unless (executable-find "qpdf"))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (_prompt) "test-password"))
                ;; Mock make-temp-file to return a path that can't be written
                ((symbol-function 'make-temp-file)
                 (lambda (&rest _args) "/nonexistent/path/temp-file")))
        ;; Should error when trying to write to non-existent path
        (should-error (cj/dwim-shell-commands-pdf-password-protect)))
    (test-dwim-shell-security-teardown)))

(ert-deftest test-dwim-multiple-temp-file-cleanup-error ()
  "Error: Multiple operations don't leave temp files behind."
  (skip-unless (and (executable-find "qpdf") (executable-find "7z")))
  (test-dwim-shell-security-setup)
  (unwind-protect
      (progn
        ;; Track temp files before operations
        (let ((initial-temp-files (directory-files temporary-file-directory nil "^qpdf-pass-\\|^7z-pass-")))
          (cl-letf (((symbol-function 'read-passwd)
                     (lambda (_prompt) "password"))
                    ((symbol-function 'read-string)
                     (lambda (_prompt &optional _default) "archive"))
                    ((symbol-function 'dwim-shell-command-on-marked-files)
                     (lambda (&rest _args) nil)))
            ;; Run multiple operations
            (cj/dwim-shell-commands-pdf-password-protect)
            (cj/dwim-shell-commands-pdf-password-unprotect)
            (cj/dwim-shell-commands-create-encrypted-zip)
            (cj/dwim-shell-commands-remove-zip-encryption))
          ;; Check no new temp files remain
          (let ((final-temp-files (directory-files temporary-file-directory nil "^qpdf-pass-\\|^7z-pass-")))
            (should (equal (length final-temp-files) (length initial-temp-files))))))
    (test-dwim-shell-security-teardown)))

(provide 'test-dwim-shell-security)
;;; test-dwim-shell-security.el ends here
