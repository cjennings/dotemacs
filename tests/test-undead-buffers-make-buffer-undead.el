;;; test-undead-buffers-make-buffer-undead.el --- Tests for cj/make-buffer-undead -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/make-buffer-undead function from undead-buffers.el

;;; Code:

(require 'ert)
(require 'undead-buffers)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-make-buffer-undead-setup ()
  "Setup for make-buffer-undead tests."
  (cj/create-test-base-dir))

(defun test-make-buffer-undead-teardown ()
  "Teardown for make-buffer-undead tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-make-buffer-undead-valid-name-should-add-to-list ()
  "Adding a valid buffer name should add it to the undead buffer list."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list)))
        (unwind-protect
            (progn
              (cj/make-buffer-undead "*test-buffer*")
              (should (member "*test-buffer*" cj/undead-buffer-list)))
          (setq cj/undead-buffer-list orig)))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-existing-name-should-not-duplicate ()
  "Adding an existing buffer name should not create duplicates."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list)))
        (unwind-protect
            (progn
              (cj/make-buffer-undead "*test-dup*")
              (cj/make-buffer-undead "*test-dup*")
              (should (= 1 (cl-count "*test-dup*" cj/undead-buffer-list :test #'string=))))
          (setq cj/undead-buffer-list orig)))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-multiple-additions-should-preserve-order ()
  "Adding multiple buffer names should preserve order."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list)))
        (unwind-protect
            (progn
              (cj/make-buffer-undead "*first*")
              (cj/make-buffer-undead "*second*")
              (cj/make-buffer-undead "*third*")
              (let ((added-items (seq-drop cj/undead-buffer-list (length orig))))
                (should (equal added-items '("*first*" "*second*" "*third*")))))
          (setq cj/undead-buffer-list orig)))
    (test-make-buffer-undead-teardown)))

;;; Boundary Cases

(ert-deftest test-make-buffer-undead-whitespace-only-name-should-add ()
  "Adding a whitespace-only name should succeed."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list)))
        (unwind-protect
            (progn
              (cj/make-buffer-undead "   ")
              (should (member "   " cj/undead-buffer-list)))
          (setq cj/undead-buffer-list orig)))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-very-long-name-should-add ()
  "Adding a very long buffer name should succeed."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (long-name (make-string 1000 ?x)))
        (unwind-protect
            (progn
              (cj/make-buffer-undead long-name)
              (should (member long-name cj/undead-buffer-list)))
          (setq cj/undead-buffer-list orig)))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-unicode-name-should-add ()
  "Adding a buffer name with Unicode characters should succeed."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list)))
        (unwind-protect
            (progn
              (cj/make-buffer-undead "*test-🚀-buffer*")
              (should (member "*test-🚀-buffer*" cj/undead-buffer-list)))
          (setq cj/undead-buffer-list orig)))
    (test-make-buffer-undead-teardown)))

;;; Error Cases

(ert-deftest test-make-buffer-undead-empty-string-should-error ()
  "Passing an empty string should signal an error."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (should-error (cj/make-buffer-undead ""))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-nil-should-error ()
  "Passing nil should signal an error."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (should-error (cj/make-buffer-undead nil))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-number-should-error ()
  "Passing a number should signal an error."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (should-error (cj/make-buffer-undead 42))
    (test-make-buffer-undead-teardown)))

(ert-deftest test-make-buffer-undead-symbol-should-error ()
  "Passing a symbol should signal an error."
  (test-make-buffer-undead-setup)
  (unwind-protect
      (should-error (cj/make-buffer-undead 'some-symbol))
    (test-make-buffer-undead-teardown)))

(provide 'test-undead-buffers-make-buffer-undead)
;;; test-undead-buffers-make-buffer-undead.el ends here
