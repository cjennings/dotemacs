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

(ert-deftest test-gptel-tools-write-text-file-validate-path-boundary-absolute-home-path ()
  "Boundary: absolute HOME paths are accepted."
  (test-gptel-tools-write-text-file--in-home
   "absolute"
   (lambda (path)
     (should (equal (cj/write-text-file--validate-path path) path)))))

(ert-deftest test-gptel-tools-write-text-file-validate-path-error-existing-symlink-outside-home ()
  "Error: an existing symlink inside HOME pointing outside HOME is rejected."
  (let ((outside (make-temp-file "test-gptel-tools-write-text-file-outside-"))
        (link (expand-file-name
               (format ".test-gptel-tools-write-text-file-outside-link-%s.tmp"
                       (format-time-string "%s%N"))
               "~")))
    (unwind-protect
        (progn
          (make-symbolic-link outside link t)
          (should-error (cj/write-text-file--validate-path link)))
      (when (file-exists-p outside) (delete-file outside))
      (when (file-symlink-p link) (delete-file link)))))

(ert-deftest test-gptel-tools-write-text-file-validate-path-error-parent-symlink-outside-home ()
  "Error: a parent symlink inside HOME pointing outside HOME is rejected."
  (let ((outside-dir (make-temp-file "test-gptel-tools-write-text-file-outside-dir-" t))
        (link-dir (expand-file-name
                   (format ".test-gptel-tools-write-text-file-outside-dir-link-%s"
                           (format-time-string "%s%N"))
                   "~")))
    (unwind-protect
        (progn
          (make-symbolic-link outside-dir link-dir t)
          (should-error
           (cj/write-text-file--validate-path
            (expand-file-name "child.txt" link-dir))))
      (when (file-symlink-p link-dir) (delete-file link-dir))
      (when (file-exists-p outside-dir) (delete-directory outside-dir t)))))

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

(ert-deftest test-gptel-tools-write-text-file-ensure-parent-error-create-fails ()
  "Error: directory creation failures are wrapped with context."
  (cl-letf (((symbol-function 'make-directory)
             (lambda (&rest _args) (error "boom"))))
    (should-error
     (cj/write-text-file--ensure-parent
      (expand-file-name "missing/child.txt" temporary-file-directory)))))

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

(ert-deftest test-gptel-tools-write-text-file-run-large-user-accepts ()
  "Boundary: large writes proceed when the user accepts."
  (test-gptel-tools-write-text-file--in-home
   "large-accept"
   (lambda (path)
     (let ((cj/write-text-file--size-limit 3))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
         (cj/write-text-file--run (file-name-nondirectory path) "abcdef" nil)))
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "abcdef"))))))

(ert-deftest test-gptel-tools-write-text-file-run-large-user-declines ()
  "Error: large writes cancel cleanly when the user declines."
  (test-gptel-tools-write-text-file--in-home
   "large-decline"
   (lambda (path)
     (let ((cj/write-text-file--size-limit 3))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
         (should-error
          (cj/write-text-file--run (file-name-nondirectory path) "abcdef" nil))))
     (should-not (file-exists-p path)))))

(ert-deftest test-gptel-tools-write-text-file-run-error-overwrite-backup-failure-preserves-file ()
  "Error: backup failure prevents overwrite and preserves existing file."
  (test-gptel-tools-write-text-file--in-home
   "backup-fails"
   (lambda (path)
     (with-temp-file path (insert "old\n"))
     (cl-letf (((symbol-function 'copy-file)
                (lambda (&rest _args) (error "copy failed"))))
       (should-error
        (cj/write-text-file--run (file-name-nondirectory path) "new\n" t)))
     (with-temp-buffer
       (insert-file-contents path)
       (should (equal (buffer-string) "old\n"))))))

(ert-deftest test-gptel-tools-write-text-file-run-error-outside-home ()
  "Error: a path outside HOME signals."
  (should-error (cj/write-text-file--run "/etc/test-write.txt" "x" nil)))

(provide 'test-gptel-tools-write-text-file)
;;; test-gptel-tools-write-text-file.el ends here
