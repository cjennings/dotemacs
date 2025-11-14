;;; test-keyboard-macros.el --- ERT tests for keyboard-macros -*- lexical-binding: t; -*-

;; Author: Claude Code and cjennings
;; Keywords: tests, keyboard-macros

;;; Commentary:
;; ERT tests for keyboard-macros.el functions.
;; Tests are organized into normal, boundary, and error cases.

;;; Code:

(require 'ert)
(require 'keyboard-macros)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-keyboard-macros-setup ()
  "Set up test environment for keyboard-macros tests."
  (cj/create-test-base-dir)
  ;; Bind macros-file to test location
  (setq macros-file (expand-file-name "test-macros.el" cj/test-base-dir))
  ;; Reset state flags
  (setq cj/macros-loaded nil)
  (setq cj/macros-loading nil)
  ;; Clear any existing macro
  (setq last-kbd-macro nil))

(defun test-keyboard-macros-teardown ()
  "Clean up test environment after keyboard-macros tests."
  ;; Kill any buffers visiting the test macros file
  (when-let ((buf (get-file-buffer macros-file)))
    (kill-buffer buf))
  ;; Clean up test directory
  (cj/delete-test-base-dir)
  ;; Reset state
  (setq cj/macros-loaded nil)
  (setq cj/macros-loading nil)
  (setq last-kbd-macro nil))

;;; Normal Cases

(ert-deftest test-keyboard-macros-ensure-macros-loaded-first-time-normal ()
  "Normal: macros file is loaded on first call when file exists."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Create a macros file with a simple macro definition
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n")
          (insert "(fset 'test-macro [?h ?e ?l ?l ?o])\n"))
        ;; Verify initial state
        (should (not cj/macros-loaded))
        ;; Load macros
        (cj/ensure-macros-loaded)
        ;; Verify loaded
        (should cj/macros-loaded)
        (should (fboundp 'test-macro)))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-ensure-macros-loaded-idempotent-normal ()
  "Normal: subsequent calls don't reload when flag is already true."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Create a macros file
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n"))
        ;; First load
        (cj/ensure-macros-loaded)
        (should cj/macros-loaded)
        ;; Modify the file after loading
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n")
          (insert "(fset 'new-macro [?n ?e ?w])\n"))
        ;; Second call should not reload
        (cj/ensure-macros-loaded)
        ;; new-macro should not be defined because file wasn't reloaded
        (should (not (fboundp 'new-macro))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-ensure-macros-file-creates-new-normal ()
  "Normal: ensure-macros-file creates new file with lexical-binding header."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (should (not (file-exists-p macros-file)))
        (ensure-macros-file macros-file)
        (should (file-exists-p macros-file))
        (with-temp-buffer
          (insert-file-contents macros-file)
          (should (string-match-p "lexical-binding: t" (buffer-string)))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-ensure-macros-file-exists-normal ()
  "Normal: ensure-macros-file leaves existing file untouched."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n")
          (insert "(fset 'existing-macro [?t ?e ?s ?t])\n"))
        (let ((original-content (with-temp-buffer
                                  (insert-file-contents macros-file)
                                  (buffer-string))))
          (ensure-macros-file macros-file)
          (should (string= original-content
                          (with-temp-buffer
                            (insert-file-contents macros-file)
                            (buffer-string))))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-start-or-end-toggle-normal ()
  "Normal: starting and stopping macro recording toggles defining-kbd-macro."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Create empty macros file
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n"))
        ;; Start recording
        (should (not defining-kbd-macro))
        (cj/kbd-macro-start-or-end)
        (should defining-kbd-macro)
        ;; Stop recording
        (cj/kbd-macro-start-or-end)
        (should (not defining-kbd-macro)))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-valid-name-normal ()
  "Normal: saving a macro with valid name writes to file and returns name."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Set up a macro
        (setq last-kbd-macro [?t ?e ?s ?t])
        ;; Save it
        (let ((result (cj/save-maybe-edit-macro "test-macro")))
          (should (string= result "test-macro"))
          (should (file-exists-p macros-file))
          ;; Verify macro was written to file
          (with-temp-buffer
            (insert-file-contents macros-file)
            (should (string-match-p "test-macro" (buffer-string))))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-without-prefix-arg-normal ()
  "Normal: without prefix arg, returns to original buffer."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (let ((original-buffer (current-buffer))
              (current-prefix-arg nil))
          (cj/save-maybe-edit-macro "test-macro")
          ;; Should return to original buffer (or stay if it was the macros file)
          (should (or (eq (current-buffer) original-buffer)
                     (not (eq (current-buffer) (get-file-buffer macros-file)))))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-with-prefix-arg-normal ()
  "Normal: with prefix arg, opens macros file for editing."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (let ((current-prefix-arg t))
          (cj/save-maybe-edit-macro "test-macro")
          ;; Should be in the macros file buffer
          (should (eq (current-buffer) (get-file-buffer macros-file)))))
    (test-keyboard-macros-teardown)))

;;; Boundary Cases

(ert-deftest test-keyboard-macros-name-single-character-boundary ()
  "Boundary: macro name with single letter (minimum valid length)."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (let ((result (cj/save-maybe-edit-macro "a")))
          (should (string= result "a"))
          (should (file-exists-p macros-file))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-name-with-numbers-boundary ()
  "Boundary: macro name containing letters, numbers, and hyphens."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (let ((result (cj/save-maybe-edit-macro "macro-123-test")))
          (should (string= result "macro-123-test"))
          (should (file-exists-p macros-file))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-name-all-caps-boundary ()
  "Boundary: macro name with uppercase letters."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (let ((result (cj/save-maybe-edit-macro "TESTMACRO")))
          (should (string= result "TESTMACRO"))
          (should (file-exists-p macros-file))))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-empty-macro-file-boundary ()
  "Boundary: loading behavior when macros file exists but is empty."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Create empty file
        (with-temp-file macros-file
          (insert ""))
        (should (not cj/macros-loaded))
        ;; Should handle empty file gracefully
        (cj/ensure-macros-loaded)
        ;; Loading an empty file should still set the flag
        (should cj/macros-loaded))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-whitespace-only-name-boundary ()
  "Boundary: whitespace-only name (spaces, tabs) is rejected."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (should-error (cj/save-maybe-edit-macro "   \t  ")))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-concurrent-load-attempts-boundary ()
  "Boundary: cj/macros-loading lock prevents race conditions."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n"))
        ;; Simulate concurrent load by setting the lock
        (setq cj/macros-loading t)
        (cj/ensure-macros-loaded)
        ;; Should not load because lock is set
        (should (not cj/macros-loaded))
        ;; Release lock and try again
        (setq cj/macros-loading nil)
        (cj/ensure-macros-loaded)
        (should cj/macros-loaded))
    (test-keyboard-macros-teardown)))

;;; Error Cases

(ert-deftest test-keyboard-macros-save-empty-name-error ()
  "Error: empty string name triggers user-error."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (should-error (cj/save-maybe-edit-macro "") :type 'user-error))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-invalid-name-special-chars-error ()
  "Error: names with special characters trigger user-error."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (should-error (cj/save-maybe-edit-macro "test@macro") :type 'user-error)
        (should-error (cj/save-maybe-edit-macro "test!macro") :type 'user-error)
        (should-error (cj/save-maybe-edit-macro "test#macro") :type 'user-error))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-invalid-name-starts-with-number-error ()
  "Error: name starting with number triggers user-error."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (should-error (cj/save-maybe-edit-macro "123macro") :type 'user-error))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-invalid-name-has-spaces-error ()
  "Error: name with spaces triggers user-error."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        (should-error (cj/save-maybe-edit-macro "test macro") :type 'user-error))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-no-macro-defined-error ()
  "Error: saving when last-kbd-macro is nil triggers user-error."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro nil)
        (should-error (cj/save-maybe-edit-macro "test-macro") :type 'user-error))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-load-malformed-file-error ()
  "Error: error handling when macros file has syntax errors."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Create a malformed macros file
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n")
          (insert "(fset 'broken-macro [incomplete"))
        (should (not cj/macros-loaded))
        ;; Should handle error gracefully (prints message but doesn't crash)
        (cj/ensure-macros-loaded)
        ;; Should not be marked as loaded due to error
        (should (not cj/macros-loaded)))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-save-file-write-error-error ()
  "Error: error handling when unable to write to macros file."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        (setq last-kbd-macro [?t ?e ?s ?t])
        ;; Create the file and make it read-only
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n"))
        (set-file-modes macros-file #o444)
        ;; Should error when trying to save
        (condition-case err
            (progn
              (cj/save-maybe-edit-macro "test-macro")
              (should nil)) ;; Should not reach here
          (error
           ;; Expected to error
           (should t)))
        ;; Clean up permissions for teardown
        (set-file-modes macros-file #o644))
    (test-keyboard-macros-teardown)))

(ert-deftest test-keyboard-macros-load-file-read-error-error ()
  "Error: error handling when unable to read macros file."
  (test-keyboard-macros-setup)
  (unwind-protect
      (progn
        ;; Create file and remove read permissions
        (with-temp-file macros-file
          (insert ";;; -*- lexical-binding: t -*-\n"))
        (set-file-modes macros-file #o000)
        (should (not cj/macros-loaded))
        ;; Should handle error gracefully
        (cj/ensure-macros-loaded)
        ;; Should not be marked as loaded
        (should (not cj/macros-loaded))
        ;; Clean up permissions for teardown
        (set-file-modes macros-file #o644))
    (test-keyboard-macros-teardown)))

(provide 'test-keyboard-macros)
;;; test-keyboard-macros.el ends here
