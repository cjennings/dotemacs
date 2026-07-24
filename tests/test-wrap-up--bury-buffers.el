;;; test-wrap-up--bury-buffers.el --- Tests for cj/bury-buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for cj/bury-buffers, which buries the noisy
;; compile-and-shell buffers at the end of startup.
;;
;; Written to pin the buried set while a dead clause was removed.  The function
;; tested `(derived-mode-p 'elisp-compile-mode)', and no such mode exists in
;; Emacs -- the real one is `emacs-lisp-compilation-mode', which derives from
;; `compilation-mode' and so was already matched by the clause above it.  The
;; clause could never be true, and removing it must not change which buffers
;; get buried.  These tests are what makes that claim checkable.
;;
;; Test organization:
;; - Normal Cases: each buried mode is buried; byte-compilation output included
;; - Boundary Cases: an ordinary buffer is left alone; an empty buffer list
;; - Error Cases: a killed buffer in the list does not break the sweep
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'wrap-up)

;; Required explicitly so each test stands alone.  Without these the
;; comint-mode test passed only because ERT runs tests in alphabetical order
;; and an earlier test loaded `compile', which pulls in comint -- so renaming
;; or running that test by itself made it fail with void-function comint-mode.
(require 'comint)
(require 'bytecomp)

(defmacro test-wrap-up--with-mode-buffer (mode &rest body)
  "Create a buffer in MODE, bind it to `buf', run BODY, then kill it."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "*test-bury*")))
     (unwind-protect
         (progn
           (with-current-buffer buf (funcall ,mode))
           ,@body)
       (kill-buffer buf))))

;;; Normal Cases

(ert-deftest test-wrap-up-bury-buffers-buries-compilation ()
  "Normal: a compilation-mode buffer is buried."
  (test-wrap-up--with-mode-buffer #'compilation-mode
    (switch-to-buffer buf)
    (cj/bury-buffers)
    (should-not (eq buf (car (buffer-list))))))

(ert-deftest test-wrap-up-bury-buffers-buries-byte-compilation-output ()
  "Normal: the real byte-compilation mode is buried.
`emacs-lisp-compilation-mode' derives from `compilation-mode', which is
why the never-matching elisp-compile-mode clause was redundant."
  (should (eq 'compilation-mode
              (get 'emacs-lisp-compilation-mode 'derived-mode-parent)))
  (test-wrap-up--with-mode-buffer #'emacs-lisp-compilation-mode
    (switch-to-buffer buf)
    (cj/bury-buffers)
    (should-not (eq buf (car (buffer-list))))))

(ert-deftest test-wrap-up-bury-buffers-buries-comint ()
  "Normal: a comint-mode buffer is buried."
  (test-wrap-up--with-mode-buffer #'comint-mode
    (switch-to-buffer buf)
    (cj/bury-buffers)
    (should-not (eq buf (car (buffer-list))))))

;;; Boundary Cases

(ert-deftest test-wrap-up-bury-buffers-leaves-ordinary-buffer ()
  "Boundary: a fundamental-mode buffer is not buried."
  (test-wrap-up--with-mode-buffer #'fundamental-mode
    (switch-to-buffer buf)
    (cj/bury-buffers)
    (should (eq buf (car (buffer-list))))))

(ert-deftest test-wrap-up-bury-buffers-leaves-text-buffer ()
  "Boundary: an ordinary text-mode buffer is not buried."
  (test-wrap-up--with-mode-buffer #'text-mode
    (switch-to-buffer buf)
    (cj/bury-buffers)
    (should (eq buf (car (buffer-list))))))

;;; Error Cases

(ert-deftest test-wrap-up-bury-buffers-survives-dead-mode-name ()
  "Error: the sweep completes even though elisp-compile-mode does not exist.
The removed clause named a mode Emacs has never defined; this pins that
the function still runs cleanly with no such mode anywhere."
  (should-not (fboundp 'elisp-compile-mode))
  (should-not (get 'elisp-compile-mode 'derived-mode-parent))
  (cj/bury-buffers))

(provide 'test-wrap-up--bury-buffers)
;;; test-wrap-up--bury-buffers.el ends here
