;;; test-architecture-startup-contracts.el --- Startup architecture smoke tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight source-level checks for cross-module startup contracts.  These
;; deliberately avoid requiring package-heavy modules; the goal is to catch
;; accidental load-order and batch-startup regressions early without building a
;; full static analyzer.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst test-architecture--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for architecture contract tests.")

(defun test-architecture--module-files ()
  "Return all direct module source files."
  (directory-files (expand-file-name "modules" test-architecture--repo-root)
                   t "\\.el\\'"))

(defun test-architecture--file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun test-architecture--read-top-level-forms (file)
  "Read top-level forms from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun test-architecture--contains-timer-form-p (form)
  "Return non-nil when FORM contains a timer scheduling call."
  (cond
   ((atom form) nil)
   ((memq (car form) '(run-with-timer run-at-time run-with-idle-timer)) t)
   (t (or (test-architecture--contains-timer-form-p (car form))
          (test-architecture--contains-timer-form-p (cdr form))))))

(defun test-architecture--noninteractive-guard-p (form)
  "Return non-nil when FORM is guarded against batch/noninteractive startup."
  (and (consp form)
       (or (and (eq (car form) 'unless)
                (eq (cadr form) 'noninteractive))
           (and (eq (car form) 'when)
                (equal (cadr form) '(not noninteractive))))))

(defun test-architecture--definition-form-p (form)
  "Return non-nil when FORM defines code but does not execute its body now."
  (and (consp form)
       (memq (car form) '(defun defmacro defsubst cl-defun cl-defmacro))))

(defun test-architecture--unguarded-top-level-timer-forms (file)
  "Return top-level timer scheduling forms in FILE that are not batch-guarded."
  (let (violations)
    (dolist (form (test-architecture--read-top-level-forms file))
      (when (and (test-architecture--contains-timer-form-p form)
                 (not (test-architecture--definition-form-p form))
                 (not (test-architecture--noninteractive-guard-p form)))
        (push (prin1-to-string form) violations)))
    (nreverse violations)))

(ert-deftest test-architecture-custom-prefix-owned-by-keybindings ()
  "Only keybindings.el may globally own the exact C-; prefix."
  (let ((owner (expand-file-name "modules/keybindings.el"
                                 test-architecture--repo-root))
        offenders)
    (dolist (file (test-architecture--module-files))
      (let ((contents (test-architecture--file-string file)))
        (when (and (not (string= file owner))
                   (or (string-match-p "(keymap-global-set[[:space:]\n]+\"C-;\"" contents)
                       (string-match-p "(global-set-key[[:space:]\n]+(kbd[[:space:]\n]+\"C-;\"" contents)))
          (push (file-relative-name file test-architecture--repo-root) offenders))))
    (should (string-match-p "(keymap-global-set[[:space:]\n]+\"C-;\""
                            (test-architecture--file-string owner)))
    (should-not offenders)))

(ert-deftest test-architecture-top-level-timers-are-batch-guarded ()
  "Top-level timer scheduling must be guarded by noninteractive.

Function definitions may contain timer calls; this test only rejects timer
scheduling that can run while a module is being required in batch/test mode."
  (let (offenders)
    (dolist (file (test-architecture--module-files))
      (let ((violations (test-architecture--unguarded-top-level-timer-forms file)))
        (when violations
          (push (format "%s: %s"
                        (file-relative-name file test-architecture--repo-root)
                        (string-join violations " "))
                offenders))))
    (should-not offenders)))

(provide 'test-architecture-startup-contracts)
;;; test-architecture-startup-contracts.el ends here
