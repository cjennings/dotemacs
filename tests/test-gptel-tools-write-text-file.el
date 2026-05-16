;;; test-gptel-tools-write-text-file.el --- Tests for write_text_file gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/write-text-file--run' and its helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "gptel-tools" user-emacs-directory))
  (setq load-prefer-newer t)
  (unless (featurep 'gptel)
    (defvar gptel-tools nil)
    (defun gptel-make-tool (&rest _args) nil)
    (defun gptel-get-tool (&rest _args) nil)
    (provide 'gptel)))

(require 'write_text_file)

;; ------------------------------------------------------- helpers

(defun test-gptel-tools-write-text-file--in-home (suffix fn)
  "Run FN with a fresh path under HOME using SUFFIX.  Clean up after."
  (let* ((name (format ".test-gptel-tools-write-text-file-%s-%s.tmp"
                       suffix (format-time-string "%s%N")))
         (path (expand-file-name name "~")))
    (unwind-protect
        (funcall fn path)
      (when (file-exists-p path) (delete-file path))
      (dolist (b (file-expand-wildcards (concat path "-*.bak")))
        (when (file-exists-p b) (delete-file b))))))

;; --------------------------------------------- validate-path

(ert-deftest test-gptel-tools-write-text-file-validate-path-normal ()
  "Normal: returns the expanded path for a HOME-relative input."
  (let ((result (cj/write-text-file--validate-path "foo.txt")))
    (should (string-prefix-p (expand-file-name "~") result))
    (should (string-suffix-p "/foo.txt" result))))

(ert-deftest test-gptel-tools-write-text-file-validate-path-error-outside-home ()
  "Error: a path outside HOME signals."
  (should-error (cj/write-text-file--validate-path "/etc/hostname")))

;; --------------------------------------------- backup-name

(ert-deftest test-gptel-tools-write-text-file-backup-name-shape ()
  "Backup names append a YYYY-MM-DD-HHMMSS suffix and .bak."
  (let ((name (cj/write-text-file--backup-name "/home/user/foo.txt")))
    (should (string-prefix-p "/home/user/foo.txt-" name))
    (should (string-suffix-p ".bak" name))
    (should (string-match-p "-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{6\\}\\.bak\\'"
                            name))))

;; --------------------------------------------- ensure-parent

(ert-deftest test-gptel-tools-write-text-file-ensure-parent-creates-missing ()
  "Normal: creates missing parent directories."
  (let* ((base (make-temp-file "test-gptel-tools-write-text-file-" t))
         (deep (expand-file-name "a/b/c/file.txt" base)))
    (unwind-protect
        (progn
          (cj/write-text-file--ensure-parent deep)
          (should (file-directory-p (file-name-directory deep))))
      (delete-directory base t))))

(ert-deftest test-gptel-tools-write-text-file-ensure-parent-error-unwritable ()
  "Error: an unwritable parent signals."
  (let* ((parent (make-temp-file "test-gptel-tools-write-text-file-ro-" t))
         (target (expand-file-name "child.txt" parent)))
    (unwind-protect
        (progn
          (set-file-modes parent #o500)
          (should-error (cj/write-text-file--ensure-parent target)))
      (set-file-modes parent #o700)
      (delete-directory parent t))))

;; --------------------------------------------- run

(ert-deftest test-gptel-tools-write-text-file-run-normal ()
  "Normal: writes new content and returns a status string."
  (test-gptel-tools-write-text-file--in-home
   "new"
   (lambda (path)
     (let ((result (cj/write-text-file--run
                    (file-name-nondirectory path) "hello\n" nil)))
       (should (string-match-p "Successfully wrote" result))
       (with-temp-buffer
         (insert-file-contents path)
         (should (equal (buffer-string) "hello\n")))))))

(ert-deftest test-gptel-tools-write-text-file-run-error-existing-no-overwrite ()
  "Error: existing file without overwrite signals."
  (test-gptel-tools-write-text-file--in-home
   "existing"
   (lambda (path)
     (with-temp-file path (insert "old content\n"))
     (should-error (cj/write-text-file--run
                    (file-name-nondirectory path) "new content\n" nil))
     ;; File preserved
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "old content\n"))))))

(ert-deftest test-gptel-tools-write-text-file-run-overwrite-creates-backup ()
  "Overwrite path makes a timestamped backup before writing."
  (test-gptel-tools-write-text-file--in-home
   "overwrite"
   (lambda (path)
     (with-temp-file path (insert "old content\n"))
     (cj/write-text-file--run
      (file-name-nondirectory path) "new content\n" t)
     ;; New content landed
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "new content\n")))
     ;; Backup exists with old content
     (let ((backups (file-expand-wildcards (concat path "-*.bak"))))
       (should (= 1 (length backups)))
       (with-temp-buffer
         (insert-file-contents (car backups))
         (should (equal (buffer-string) "old content\n")))))))

(ert-deftest test-gptel-tools-write-text-file-run-boundary-empty-content ()
  "Boundary: nil content writes an empty file."
  (test-gptel-tools-write-text-file--in-home
   "empty"
   (lambda (path)
     (cj/write-text-file--run (file-name-nondirectory path) nil nil)
     (should (file-exists-p path))
     (should (= 0 (file-attribute-size (file-attributes path)))))))

(ert-deftest test-gptel-tools-write-text-file-run-error-outside-home ()
  "Error: a path outside HOME signals."
  (should-error (cj/write-text-file--run "/etc/test-write.txt" "x" nil)))

(provide 'test-gptel-tools-write-text-file)
;;; test-gptel-tools-write-text-file.el ends here
