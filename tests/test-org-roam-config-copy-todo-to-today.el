;;; test-org-roam-config-copy-todo-to-today.el --- Tests for org-roam TODO completion hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the org-after-todo-state-change-hook configuration that copies
;; completed tasks to daily org-roam nodes.
;;
;; The hook should trigger for ANY org-mode done state (DONE, CANCELLED, etc.),
;; not just "DONE". This is verified by checking membership in org-done-keywords.
;;
;; The critical behavior being tested is that the hook is registered
;; immediately when org-mode loads, NOT when org-roam loads (which happens
;; lazily). This ensures tasks can be copied to dailies even before the user
;; has invoked any org-roam commands.

;;; Code:

(require 'ert)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-org-roam-todo-hook-setup ()
  "Setup for org-roam todo hook tests."
  (cj/create-test-base-dir))

(defun test-org-roam-todo-hook-teardown ()
  "Teardown for org-roam todo hook tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-org-roam-hook-registered-after-org-loads ()
  "The hook should be registered after org loads."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (progn
        (require 'org)
        (require 'org-roam-config)
        (should (consp org-after-todo-state-change-hook)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-calls-copy-function-on-done ()
  "The hook lambda should call copy function when state is DONE."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))  ; Set done keywords for test
        (setq org-last-state nil)   ; No previous state (new task)
        (setq org-state "DONE")     ; Dynamic variable used by org-mode hooks
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-calls-copy-function-on-cancelled ()
  "The hook lambda should call copy function when state is CANCELLED."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))  ; Set done keywords for test
        (setq org-last-state nil)       ; No previous state (new task)
        (setq org-state "CANCELLED")    ; Dynamic variable used by org-mode hooks
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

;;; Boundary Cases

(ert-deftest test-org-roam-hook-registered-before-org-roam-loads ()
  "The hook should be registered even if org-roam has not loaded yet."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (progn
        (require 'org)
        (require 'org-roam-config)
        (should (consp org-after-todo-state-change-hook))
        (should-not (featurep 'org-roam)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-calls-copy-on-todo-to-done ()
  "The hook should copy when transitioning FROM TODO TO DONE."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))
        (setq org-last-state "TODO")  ; Previous state was TODO (non-done)
        (setq org-state "DONE")       ; New state is DONE
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-calls-copy-on-in-progress-to-done ()
  "The hook should copy when transitioning FROM IN-PROGRESS TO DONE."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))
        (setq org-last-state "IN-PROGRESS")  ; Previous state was IN-PROGRESS (non-done)
        (setq org-state "DONE")              ; New state is DONE
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-calls-copy-on-waiting-to-cancelled ()
  "The hook should copy when transitioning FROM WAITING TO CANCELLED."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))
        (setq org-last-state "WAITING")   ; Previous state was WAITING (non-done)
        (setq org-state "CANCELLED")      ; New state is CANCELLED
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-ignores-done-to-cancelled ()
  "The hook should NOT copy when transitioning FROM DONE TO CANCELLED (both done)."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))
        (setq org-last-state "DONE")      ; Previous state was DONE (already done)
        (setq org-state "CANCELLED")      ; New state is CANCELLED (also done)
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should-not copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-ignores-todo-state ()
  "The hook should not copy when transitioning TO TODO state (non-done)."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))  ; TODO is not in done keywords
        (setq org-state "TODO")  ; Transitioning TO TODO
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should-not copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(ert-deftest test-org-roam-hook-ignores-in-progress-state ()
  "The hook should not copy when transitioning TO IN-PROGRESS state (non-done)."
  (test-org-roam-todo-hook-setup)
  (unwind-protect
      (let ((copy-function-called nil))
        (require 'org)
        (require 'org-roam-config)
        (setq org-done-keywords '("DONE" "CANCELLED"))  ; IN-PROGRESS is not in done keywords
        (setq org-state "IN-PROGRESS")  ; Transitioning TO IN-PROGRESS
        (cl-letf (((symbol-function 'cj/org-roam-copy-todo-to-today)
                   (lambda () (setq copy-function-called t))))
          (run-hooks 'org-after-todo-state-change-hook)
          (should-not copy-function-called)))
    (test-org-roam-todo-hook-teardown)))

(provide 'test-org-roam-config-copy-todo-to-today)
;;; test-org-roam-config-copy-todo-to-today.el ends here
