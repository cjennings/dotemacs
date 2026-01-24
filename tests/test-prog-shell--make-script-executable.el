;;; test-prog-shell--make-script-executable.el --- Tests for cj/make-script-executable -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the cj/make-script-executable function in prog-shell.el

;;; Code:

(require 'ert)
(require 'prog-shell)

;; Helper to create temp file with content
(defun test--create-temp-script (content)
  "Create a temp file with CONTENT and return its path."
  (let ((temp-file (make-temp-file "test-script-")))
    (with-temp-file temp-file
      (insert content))
    ;; Remove execute permission to start fresh
    (set-file-modes temp-file #o644)
    temp-file))

;; Helper to check if file is executable
(defun test--file-executable-p (file)
  "Return t if FILE has any execute bit set."
  (not (zerop (logand (file-modes file) #o111))))

;;; Normal Cases

(ert-deftest test-make-script-executable-normal-bash-shebang ()
  "File with bash shebang becomes executable."
  (let ((temp-file (test--create-temp-script "#!/bin/bash\necho hello")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest test-make-script-executable-normal-python-shebang ()
  "File with python shebang becomes executable."
  (let ((temp-file (test--create-temp-script "#!/usr/bin/env python3\nprint('hi')")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest test-make-script-executable-normal-already-executable ()
  "Already executable file stays executable without error."
  (let ((temp-file (test--create-temp-script "#!/bin/bash\necho hello")))
    (unwind-protect
        (progn
          (set-file-modes temp-file #o755)
          (with-current-buffer (find-file-noselect temp-file)
            (should (test--file-executable-p temp-file))
            (cj/make-script-executable)  ; should not error
            (should (test--file-executable-p temp-file))
            (kill-buffer)))
      (delete-file temp-file))))

;;; Boundary Cases

(ert-deftest test-make-script-executable-boundary-no-shebang ()
  "File without shebang is NOT made executable."
  (let ((temp-file (test--create-temp-script "echo hello\n# no shebang")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should-not (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest test-make-script-executable-boundary-empty-file ()
  "Empty file is NOT made executable."
  (let ((temp-file (test--create-temp-script "")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should-not (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest test-make-script-executable-boundary-shebang-line-two ()
  "Shebang on line 2 (not line 1) does NOT make file executable."
  (let ((temp-file (test--create-temp-script "\n#!/bin/bash\necho hello")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should-not (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

(ert-deftest test-make-script-executable-boundary-hash-but-not-shebang ()
  "File starting with # but not #! is NOT made executable."
  (let ((temp-file (test--create-temp-script "# This is a comment\necho hello")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should-not (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

;;; Edge Cases

(ert-deftest test-make-script-executable-edge-no-buffer-file ()
  "Buffer not visiting a file does not error."
  (with-temp-buffer
    (insert "#!/bin/bash\necho hello")
    (should-not buffer-file-name)
    (cj/make-script-executable)))  ; should not error

(ert-deftest test-make-script-executable-edge-shebang-with-space ()
  "Shebang with space after #! still works."
  (let ((temp-file (test--create-temp-script "#! /bin/bash\necho hello")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (test--file-executable-p temp-file))
          (cj/make-script-executable)
          (should (test--file-executable-p temp-file))
          (kill-buffer))
      (delete-file temp-file))))

(provide 'test-prog-shell--make-script-executable)
;;; test-prog-shell--make-script-executable.el ends here
