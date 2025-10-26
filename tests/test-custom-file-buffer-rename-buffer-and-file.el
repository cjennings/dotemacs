;;; test-custom-file-buffer-rename-buffer-and-file.el --- Tests for cj/--rename-buffer-and-file -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--rename-buffer-and-file function from custom-file-buffer.el
;;
;; This is the internal (non-interactive) implementation that renames both the
;; current buffer and its visited file. The interactive wrapper
;; cj/rename-buffer-and-file handles user prompting and delegates to this
;; implementation.

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
(require 'custom-file-buffer)

;;; Setup and Teardown

(defun test-rename-buffer-and-file-setup ()
  "Setup for rename-buffer-and-file tests."
  (cj/create-test-base-dir))

(defun test-rename-buffer-and-file-teardown ()
  "Teardown for rename-buffer-and-file tests."
  ;; Kill all buffers visiting files in test directory
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (when (string-prefix-p cj/test-base-dir (buffer-file-name buf))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf))))
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-rename-buffer-and-file-simple-rename ()
  "Should rename file in same directory."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (file-exists-p new-file))
        (should-not (file-exists-p old-file))
        (should (string= (buffer-name) "new.txt"))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-different-directory ()
  "Should rename to absolute path in different directory."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (old-file (expand-file-name "file.txt" source-dir))
             (new-file (expand-file-name "renamed.txt" target-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file new-file)
        (should (file-exists-p new-file))
        (should-not (file-exists-p old-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-different-extension ()
  "Should change file extension."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "file.txt" test-dir))
             (new-file (expand-file-name "file.md" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "file.md")
        (should (file-exists-p new-file))
        (should-not (file-exists-p old-file))
        (should (string= (buffer-name) "file.md"))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-preserves-content ()
  "Should preserve file content after rename."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (content "Important content\nWith multiple lines"))
        (with-temp-file old-file
          (insert content))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-string) content))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-updates-buffer-name ()
  "Should update buffer name to match new filename."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (should (string= (buffer-name) "old.txt"))
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-name) "new.txt"))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-updates-buffer-file-name ()
  "Should update buffer-file-name correctly."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-file-name) new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-clears-modified-flag ()
  "Should clear modified flag after rename."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (insert "modification")
        (should (buffer-modified-p))
        (cj/--rename-buffer-and-file "new.txt")
        (should-not (buffer-modified-p))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-returns-t-on-success ()
  "Should return t when successful."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (should (eq t (cj/--rename-buffer-and-file "new.txt")))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Boundary Cases - Naming

(ert-deftest test-rename-buffer-and-file-unicode-in-name ()
  "Should handle Unicode characters in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "café.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "café.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-emoji-in-name ()
  "Should handle emoji characters in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "test-🎉.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "test-🎉.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-rtl-text-in-name ()
  "Should handle RTL text in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "مرحبا.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "مرحبا.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-spaces-in-name ()
  "Should handle spaces in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "my new file.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "my new file.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-special-chars-in-name ()
  "Should handle special characters in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "[test]-(1).txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "[test]-(1).txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-very-long-name ()
  "Should handle very long filename."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (long-name (concat (make-string 200 ?x) ".txt"))
             (new-file (expand-file-name long-name test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file long-name)
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-single-char-name ()
  "Should handle single character name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "x" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "x")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-multiple-dots ()
  "Should handle multiple dots in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "my.file.name.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "my.file.name.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-no-extension ()
  "Should handle files without extension."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "README" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "README")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-hidden-file ()
  "Should handle hidden files (starting with dot)."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name ".hidden" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file ".hidden")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-trailing-whitespace ()
  "Should handle trailing/leading spaces in name."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "  spaced  " test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "  spaced  ")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-only-changes-case ()
  "Should handle case-only rename on case-sensitive filesystems."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "test.txt" test-dir))
             (new-file (expand-file-name "TEST.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        ;; On case-insensitive systems, need ok-if-exists
        (cj/--rename-buffer-and-file "TEST.txt" t)
        (should (string= (buffer-name) "TEST.txt"))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-adds-extension ()
  "Should handle adding extension to file."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "file" test-dir))
             (new-file (expand-file-name "file.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "file.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-removes-extension ()
  "Should handle removing extension from file."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "file.txt" test-dir))
             (new-file (expand-file-name "file" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "file")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-just-extension ()
  "Should handle name that is just extension."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name ".gitignore" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file ".gitignore")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Boundary Cases - Path Handling

(ert-deftest test-rename-buffer-and-file-relative-path ()
  "Should handle relative path."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (old-file (expand-file-name "file.txt" source-dir))
             (new-file (expand-file-name "renamed.txt" target-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "../target/renamed.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-absolute-path ()
  "Should handle absolute path."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (old-file (expand-file-name "file.txt" source-dir))
             (new-file (expand-file-name "renamed.txt" target-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file new-file)
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-parent-directory ()
  "Should handle parent directory reference."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((parent-dir (cj/create-test-subdirectory "parent"))
             (source-dir (cj/create-test-subdirectory "parent/source"))
             (old-file (expand-file-name "file.txt" source-dir))
             (new-file (expand-file-name "renamed.txt" parent-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "../renamed.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-deeply-nested-target ()
  "Should handle deeply nested target directory."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "a/b/c/d/target"))
             (old-file (expand-file-name "file.txt" source-dir))
             (new-file (expand-file-name "renamed.txt" target-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file new-file)
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-same-directory-basename-only ()
  "Should rename in same directory using just basename."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (file-exists-p new-file))
        (should-not (file-exists-p old-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-expand-tilde ()
  "Should expand tilde in path."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             ;; Use a path relative to home that we can create
             (home-test-dir (expand-file-name "temp-test-rename" "~"))
             (new-file (expand-file-name "renamed.txt" home-test-dir)))
        (make-directory home-test-dir t)
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (cj/--rename-buffer-and-file (concat "~/temp-test-rename/renamed.txt"))
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer))
        (delete-directory home-test-dir t))
    (test-rename-buffer-and-file-teardown)))

;;; Boundary Cases - File Content

(ert-deftest test-rename-buffer-and-file-empty-file ()
  "Should handle empty file."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir)))
        (with-temp-file old-file)
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (file-exists-p new-file))
        (should (= 0 (buffer-size)))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-large-file ()
  "Should handle large file."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (large-content (make-string 100000 ?x)))
        (with-temp-file old-file
          (insert large-content))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-string) large-content))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-binary-content ()
  "Should handle binary content."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.dat" test-dir))
             (new-file (expand-file-name "new.dat" test-dir))
             (binary-content (string 0 1 2 3 255 254 253)))
        (with-temp-file old-file
          (set-buffer-multibyte nil)
          (insert binary-content))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.dat")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-preserves-newlines ()
  "Should preserve different newline types."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (content "Line 1\nLine 2\n\nLine 4\n"))
        (with-temp-file old-file
          (insert content))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-string) content))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-preserves-encoding ()
  "Should preserve UTF-8 encoded content."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (content "Hello 世界 مرحبا Привет"))
        (with-temp-file old-file
          (insert content))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-string) content))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Boundary Cases - Buffer State

(ert-deftest test-rename-buffer-and-file-with-unsaved-changes ()
  "Should handle buffer with unsaved changes."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "original"))
        (find-file old-file)
        (insert " modified")
        (should (buffer-modified-p))
        (cj/--rename-buffer-and-file "new.txt")
        (should-not (buffer-modified-p))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-multiple-windows ()
  "Should work when buffer displayed in multiple windows."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (delete-other-windows)
        (split-window)
        (other-window 1)
        (switch-to-buffer (get-file-buffer old-file))
        (cj/--rename-buffer-and-file "new.txt")
        (should (string= (buffer-name) "new.txt"))
        (kill-buffer (current-buffer))
        (delete-other-windows))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-preserves-point ()
  "Should preserve point position."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (content "Line 1\nLine 2\nLine 3\n"))
        (with-temp-file old-file
          (insert content))
        (find-file old-file)
        (goto-char (point-min))
        (forward-line 1)
        (let ((original-point (point)))
          (cj/--rename-buffer-and-file "new.txt")
          (should (= (point) original-point)))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-preserves-mark ()
  "Should preserve mark."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (content "Line 1\nLine 2\nLine 3\n"))
        (with-temp-file old-file
          (insert content))
        (find-file old-file)
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 2)
        (let ((original-mark (mark)))
          (cj/--rename-buffer-and-file "new.txt")
          (should (= (mark) original-mark)))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-read-only-buffer ()
  "Should work even with read-only buffer."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (read-only-mode 1)
        (cj/--rename-buffer-and-file "new.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Error Cases - Buffer Issues

(ert-deftest test-rename-buffer-and-file-non-file-buffer-returns-nil ()
  "Should return nil when buffer not visiting file."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (with-temp-buffer
        (rename-buffer "non-file-buffer" t)
        (let ((result (cj/--rename-buffer-and-file "new.txt")))
          (should-not result)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-scratch-buffer-returns-nil ()
  "Should return nil for scratch buffer."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (with-current-buffer "*scratch*"
        (let ((result (cj/--rename-buffer-and-file "new.txt")))
          (should-not result)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-buffer-name-exists-should-error ()
  "Should error when buffer with new name exists."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (file1 (expand-file-name "file1.txt" test-dir))
             (file2 (expand-file-name "file2.txt" test-dir)))
        (with-temp-file file1
          (insert "content1"))
        (with-temp-file file2
          (insert "content2"))
        (find-file file1)
        (let ((buf1 (current-buffer)))
          (find-file file2)
          ;; Try to rename file2 to file1.txt (buffer exists)
          (should-error (cj/--rename-buffer-and-file "file1.txt"))
          (kill-buffer (current-buffer))
          (kill-buffer buf1)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-killed-buffer-should-error ()
  "Should error when operating on killed buffer."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (buf nil))
        (with-temp-file old-file
          (insert "content"))
        (setq buf (find-file old-file))
        (kill-buffer buf)
        (should-error
         (with-current-buffer buf
           (cj/--rename-buffer-and-file "new.txt"))))
    (test-rename-buffer-and-file-teardown)))

;;; Error Cases - File Conflicts

(ert-deftest test-rename-buffer-and-file-target-exists-should-error-if-not-ok ()
  "Should error when target exists and ok-if-exists is nil."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir)))
        (with-temp-file old-file
          (insert "old content"))
        (with-temp-file new-file
          (insert "existing content"))
        (find-file old-file)
        (should-error (cj/--rename-buffer-and-file "new.txt" nil))
        ;; Old file should still exist since rename failed
        (should (file-exists-p old-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-target-exists-should-overwrite-if-ok ()
  "Should overwrite when target exists and ok-if-exists is t."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir))
             (old-content "old content")
             (new-content "existing content"))
        (with-temp-file old-file
          (insert old-content))
        (with-temp-file new-file
          (insert new-content))
        (find-file old-file)
        (cj/--rename-buffer-and-file "new.txt" t)
        (should (file-exists-p new-file))
        (should-not (file-exists-p old-file))
        ;; Content should be from old file
        (revert-buffer t t)
        (should (string= (buffer-string) old-content))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-source-deleted-should-error ()
  "Should error if source file deleted during operation."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (delete-file old-file)
        (should-error (cj/--rename-buffer-and-file "new.txt"))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-same-name-is-noop ()
  "Should handle rename to same name as no-op."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "file.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        ;; Rename to same name with ok-if-exists
        (cj/--rename-buffer-and-file "file.txt" t)
        (should (file-exists-p old-file))
        (should (string= (buffer-name) "file.txt"))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Error Cases - Path Issues

(ert-deftest test-rename-buffer-and-file-nil-name-should-error ()
  "Should error when new-name is nil."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (should-error (cj/--rename-buffer-and-file nil))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-empty-name-should-error ()
  "Should error when new-name is empty string."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (should-error (cj/--rename-buffer-and-file ""))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-nonexistent-target-dir-should-error ()
  "Should error when target directory doesn't exist."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (nonexistent-path (expand-file-name "nonexistent/new.txt" cj/test-base-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (should-error (cj/--rename-buffer-and-file nonexistent-path))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-target-is-directory-should-error ()
  "Should error when new-name is existing directory."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (target-dir (cj/create-test-subdirectory "target"))
             (old-file (expand-file-name "old.txt" test-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (should-error (cj/--rename-buffer-and-file target-dir))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Error Cases - Permissions

(ert-deftest test-rename-buffer-and-file-no-write-permission-target ()
  "Should error when target directory not writable."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (old-file (expand-file-name "old.txt" source-dir))
             (new-file (expand-file-name "new.txt" target-dir)))
        (with-temp-file old-file
          (insert "content"))
        (set-file-modes target-dir #o555)
        (find-file old-file)
        (should-error (cj/--rename-buffer-and-file new-file))
        (set-file-modes target-dir #o755)
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-no-delete-permission-source-dir ()
  "Should error when source directory doesn't allow deletion."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((source-dir (cj/create-test-subdirectory "source"))
             (target-dir (cj/create-test-subdirectory "target"))
             (old-file (expand-file-name "old.txt" source-dir))
             (new-file (expand-file-name "new.txt" target-dir)))
        (with-temp-file old-file
          (insert "content"))
        (find-file old-file)
        (set-file-modes source-dir #o555)
        (should-error (cj/--rename-buffer-and-file new-file))
        (set-file-modes source-dir #o755)
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

;;; Error Cases - Edge Cases

(ert-deftest test-rename-buffer-and-file-symlink-source ()
  "Should handle symbolic link as source."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (real-file (expand-file-name "real.txt" test-dir))
             (symlink (expand-file-name "link.txt" test-dir))
             (new-file (expand-file-name "renamed.txt" test-dir)))
        (with-temp-file real-file
          (insert "content"))
        (make-symbolic-link real-file symlink)
        (find-file symlink)
        (cj/--rename-buffer-and-file "renamed.txt")
        (should (file-exists-p new-file))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(ert-deftest test-rename-buffer-and-file-interactive-prompts-on-conflict ()
  "Should prompt user when called interactively and file exists."
  (test-rename-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (old-file (expand-file-name "old.txt" test-dir))
             (new-file (expand-file-name "new.txt" test-dir))
             (prompted nil))
        (with-temp-file old-file
          (insert "old"))
        (with-temp-file new-file
          (insert "existing"))
        (find-file old-file)
        ;; Mock yes-or-no-p to capture that it was called
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt)
                     (setq prompted t)
                     t))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "new.txt")))
          (call-interactively #'cj/rename-buffer-and-file)
          (should prompted))
        (kill-buffer (current-buffer)))
    (test-rename-buffer-and-file-teardown)))

(provide 'test-custom-file-buffer-rename-buffer-and-file)
;;; test-custom-file-buffer-rename-buffer-and-file.el ends here
