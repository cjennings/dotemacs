;;; test-custom-buffer-file-move-buffer-and-file.el --- Tests for cj/move-buffer-and-file -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--move-buffer-and-file function from custom-buffer-file.el
;;
;; This is the internal (non-interactive) implementation that moves both the
;; current buffer and its visited file to a new directory. It handles trailing
;; slashes, preserves file content, updates the visited-file-name, and clears
;; the modified flag. The interactive wrapper cj/move-buffer-and-file handles
;; user prompting and delegates to this implementation.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub ps-print package
(provide 'ps-print)

;; Now load the actual production module
(require 'custom-buffer-file)

;;; Setup and Teardown

(defun test-move-buffer-and-file-setup ()
  "Setup for move-buffer-and-file tests."
  (cj/create-test-base-dir))

(defun test-move-buffer-and-file-teardown ()
  "Teardown for move-buffer-and-file tests."
  ;; Kill all buffers visiting files in test directory
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (when (string-prefix-p cj/test-base-dir (buffer-file-name buf))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf))))
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-move-buffer-and-file-simple-move-should-succeed ()
  "Should move file and buffer to new directory successfully."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir))
             (content "Test content"))
        (with-temp-file source-file
          (insert content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-updates-buffer-file-name ()
  "Should update buffer-file-name to new location."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (string= (buffer-file-name) target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-preserves-content ()
  "Should preserve file content after move."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (content "Original content\nWith multiple lines\n"))
        (with-temp-file source-file
          (insert content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (string= (buffer-string) content))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-preserves-buffer-name ()
  "Should preserve buffer name after move."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "myfile.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (should (string= (buffer-name) "myfile.txt"))
        (cj/--move-buffer-and-file target-dir)
        (should (string= (buffer-name) "myfile.txt"))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-clears-modified-flag ()
  "Should clear buffer modified flag after move."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (insert "modification")
        (should (buffer-modified-p))
        (cj/--move-buffer-and-file target-dir)
        (should-not (buffer-modified-p))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-returns-t-on-success ()
  "Should return t on successful move."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (should (eq t (cj/--move-buffer-and-file target-dir)))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-deletes-source-file ()
  "Should delete source file after move."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-creates-target-file ()
  "Should create file in target directory."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Boundary Cases - Path Handling

(ert-deftest test-move-buffer-and-file-trailing-slash-should-strip ()
  "Should handle directory with trailing slash."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file (concat target-dir "/"))
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-trailing-backslash-should-strip ()
  "Should handle directory with trailing backslash."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file (concat target-dir "\\"))
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-no-trailing-slash-should-work ()
  "Should work with directory without trailing slash."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-deeply-nested-target ()
  "Should move to deeply nested target directory."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "a/b/c/d/target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-relative-path-should-work ()
  "Should resolve relative paths relative to file's directory."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        ;; Use "../target" to go up from source/ to target/
        (cj/--move-buffer-and-file "../target")
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Boundary Cases - Character Encoding

(ert-deftest test-move-buffer-and-file-unicode-filename ()
  "Should handle Unicode characters in filename."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test-café.txt" source-dir))
             (target-file (expand-file-name "test-café.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-unicode-directory ()
  "Should handle Unicode characters in directory name."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target-ñoño"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-emoji-in-filename ()
  "Should handle emoji in filename."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test-🎉-file.txt" source-dir))
             (target-file (expand-file-name "test-🎉-file.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-rtl-characters ()
  "Should handle RTL text in filename."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test-مرحبا.txt" source-dir))
             (target-file (expand-file-name "test-مرحبا.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-spaces-in-filename ()
  "Should handle spaces in filename."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test file with spaces.txt" source-dir))
             (target-file (expand-file-name "test file with spaces.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-special-chars-in-filename ()
  "Should handle special characters in filename."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test[file]-(1).txt" source-dir))
             (target-file (expand-file-name "test[file]-(1).txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Boundary Cases - File Naming

(ert-deftest test-move-buffer-and-file-hidden-file ()
  "Should handle hidden files (starting with dot)."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name ".hidden" source-dir))
             (target-file (expand-file-name ".hidden" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-no-extension ()
  "Should handle files without extensions."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "README" source-dir))
             (target-file (expand-file-name "README" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-multiple-dots-in-name ()
  "Should handle multiple dots in filename."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "my.file.name.test.txt" source-dir))
             (target-file (expand-file-name "my.file.name.test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-single-char-filename ()
  "Should handle single character filenames."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "x" source-dir))
             (target-file (expand-file-name "x" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-very-long-filename ()
  "Should handle very long filenames."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (long-name (concat (make-string 200 ?x) ".txt"))
             (source-file (expand-file-name long-name source-dir))
             (target-file (expand-file-name long-name target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-very-long-path ()
  "Should handle very long paths."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((long-dir (make-string 100 ?x))
             (source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory long-dir))
             (long-filename (concat (make-string 100 ?y) ".txt"))
             (source-file (expand-file-name long-filename source-dir))
             (target-file (expand-file-name long-filename target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Boundary Cases - File Content

(ert-deftest test-move-buffer-and-file-empty-file ()
  "Should move empty file successfully."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "empty.txt" source-dir))
             (target-file (expand-file-name "empty.txt" target-dir)))
        (with-temp-file source-file)
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (should (= 0 (buffer-size)))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-large-file ()
  "Should move large file successfully."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "large.txt" source-dir))
             (large-content (make-string 100000 ?x)))
        (with-temp-file source-file
          (insert large-content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (string= (buffer-string) large-content))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-binary-file ()
  "Should move binary-like content successfully."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "binary.dat" source-dir))
             (target-file (expand-file-name "binary.dat" target-dir))
             (binary-content (string 0 1 2 3 255 254 253)))
        (with-temp-file source-file
          (set-buffer-multibyte nil)
          (insert binary-content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-preserves-newlines ()
  "Should preserve different newline types."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "newlines.txt" source-dir))
             (content "Line 1\nLine 2\n\nLine 4\n"))
        (with-temp-file source-file
          (insert content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (string= (buffer-string) content))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-preserves-encoding ()
  "Should preserve UTF-8 encoded content."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "utf8.txt" source-dir))
             (content "Hello 世界 مرحبا Привет"))
        (with-temp-file source-file
          (insert content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir)
        (should (string= (buffer-string) content))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Boundary Cases - Buffer State

(ert-deftest test-move-buffer-and-file-with-unsaved-changes ()
  "Should handle buffer with unsaved changes."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir))
             (original "original"))
        (with-temp-file source-file
          (insert original))
        (find-file source-file)
        (insert " modified")
        (should (buffer-modified-p))
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (buffer-modified-p))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-with-multiple-windows ()
  "Should work when buffer is displayed in multiple windows."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (delete-other-windows)
        (split-window)
        (other-window 1)
        (switch-to-buffer (get-file-buffer source-file))
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (kill-buffer (current-buffer))
        (delete-other-windows))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-preserves-point-position ()
  "Should preserve point position in buffer."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (content "Line 1\nLine 2\nLine 3\n"))
        (with-temp-file source-file
          (insert content))
        (find-file source-file)
        (goto-char (point-min))
        (forward-line 1)
        (let ((original-point (point)))
          (cj/--move-buffer-and-file target-dir)
          (should (= (point) original-point)))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-preserves-mark ()
  "Should preserve mark in buffer."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (content "Line 1\nLine 2\nLine 3\n"))
        (with-temp-file source-file
          (insert content))
        (find-file source-file)
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 2)
        (let ((original-mark (mark)))
          (cj/--move-buffer-and-file target-dir)
          (should (= (mark) original-mark)))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Error Cases - Buffer Issues

(ert-deftest test-move-buffer-and-file-non-file-buffer-returns-nil ()
  "Should return nil when buffer not visiting a file."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let ((target-dir (cj/create-test-subdirectory "target")))
        (with-temp-buffer
          (rename-buffer "non-file-buffer" t)
          (let ((result (cj/--move-buffer-and-file target-dir)))
            (should-not result))))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-scratch-buffer-returns-nil ()
  "Should return nil for scratch buffer."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let ((target-dir (cj/create-test-subdirectory "target")))
        (with-current-buffer "*scratch*"
          (let ((result (cj/--move-buffer-and-file target-dir)))
            (should-not result))))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-killed-buffer-should-error ()
  "Should error when operating on killed buffer."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (buf nil))
        (with-temp-file source-file
          (insert "content"))
        (setq buf (find-file source-file))
        (kill-buffer buf)
        (should-error
         (with-current-buffer buf
           (cj/--move-buffer-and-file target-dir))))
    (test-move-buffer-and-file-teardown)))

;;; Error Cases - Directory Issues

(ert-deftest test-move-buffer-and-file-nonexistent-target-should-error ()
  "Should error when target directory doesn't exist."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (source-file (expand-file-name "test.txt" source-dir))
             (nonexistent-dir (expand-file-name "nonexistent" cj/test-base-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (should-error (cj/--move-buffer-and-file nonexistent-dir))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-target-is-file-not-dir-should-error ()
  "Should error when target is a file, not directory."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "notadir.txt" cj/test-base-dir)))
        (with-temp-file target-file
          (insert "I'm a file"))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (should-error (cj/--move-buffer-and-file target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-nil-directory-should-error ()
  "Should error when directory is nil."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (should-error (cj/--move-buffer-and-file nil))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-empty-string-directory-should-error ()
  "Should error when directory is empty string."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (should-error (cj/--move-buffer-and-file ""))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Error Cases - Permission Issues

(ert-deftest test-move-buffer-and-file-no-read-permission-source-should-error ()
  "Should error when source file is not readable."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (set-file-modes source-file #o000)
        (should-error (cj/--move-buffer-and-file target-dir))
        (set-file-modes source-file #o644)
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-no-write-permission-target-should-error ()
  "Should error when target directory is not writable."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (set-file-modes target-dir #o555)
        (find-file source-file)
        (should-error (cj/--move-buffer-and-file target-dir))
        (set-file-modes target-dir #o755)
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-no-delete-permission-source-should-error ()
  "Should error when source directory doesn't allow deletion."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (set-file-modes source-dir #o555)
        (should-error (cj/--move-buffer-and-file target-dir))
        (set-file-modes source-dir #o755)
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Error Cases - File Conflicts

(ert-deftest test-move-buffer-and-file-target-exists-should-overwrite ()
  "Should overwrite existing file when ok-if-exists is t."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir))
             (new-content "New content")
             (old-content "Old content"))
        (with-temp-file target-file
          (insert old-content))
        (with-temp-file source-file
          (insert new-content))
        (find-file source-file)
        (cj/--move-buffer-and-file target-dir t)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (revert-buffer t t)
        (should (string= (buffer-string) new-content))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-target-exists-should-error-if-not-ok ()
  "Should error when target exists and ok-if-exists is nil."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file target-file
          (insert "existing"))
        (with-temp-file source-file
          (insert "new"))
        (find-file source-file)
        (should-error (cj/--move-buffer-and-file target-dir nil))
        ;; Source should still exist since move failed
        (should (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-interactive-prompts-if-target-exists ()
  "Should prompt user when called interactively and target exists."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir))
             (prompted nil))
        (with-temp-file target-file
          (insert "existing"))
        (with-temp-file source-file
          (insert "new"))
        (find-file source-file)
        ;; Mock yes-or-no-p to capture that it was called
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt)
                     (setq prompted t)
                     t))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) target-dir)))
          (call-interactively #'cj/move-buffer-and-file)
          (should prompted))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-interactive-no-prompt-if-target-missing ()
  "Should not prompt when called interactively if target doesn't exist."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (prompted nil))
        (with-temp-file source-file
          (insert "new"))
        (find-file source-file)
        ;; Mock yes-or-no-p to capture if it was called
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt)
                     (setq prompted t)
                     t))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) target-dir)))
          (call-interactively #'cj/move-buffer-and-file)
          (should-not prompted))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-source-deleted-during-operation-should-error ()
  "Should error if source file is deleted during operation."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (delete-file source-file)
        (should-error (cj/--move-buffer-and-file target-dir))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

;;; Error Cases - Edge Cases

(ert-deftest test-move-buffer-and-file-symlink-source-should-handle ()
  "Should handle symbolic link as source."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (real-file (expand-file-name "real.txt" source-dir))
             (symlink (expand-file-name "link.txt" source-dir))
             (target-file (expand-file-name "link.txt" target-dir)))
        (with-temp-file real-file
          (insert "content"))
        (make-symbolic-link real-file symlink)
        (find-file symlink)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(ert-deftest test-move-buffer-and-file-read-only-buffer-should-still-work ()
  "Should work even if buffer is read-only."
  (test-move-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (source-file (expand-file-name "test.txt" source-dir))
             (target-file (expand-file-name "test.txt" target-dir)))
        (with-temp-file source-file
          (insert "content"))
        (find-file source-file)
        (read-only-mode 1)
        (cj/--move-buffer-and-file target-dir)
        (should (file-exists-p target-file))
        (should-not (file-exists-p source-file))
        (kill-buffer (current-buffer)))
    (test-move-buffer-and-file-teardown)))

(provide 'test-custom-buffer-file-move-buffer-and-file)
;;; test-custom-buffer-file-move-buffer-and-file.el ends here
