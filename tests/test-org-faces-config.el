;;; test-org-faces-config.el --- Tests for org-faces-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the custom agenda header-row faces exist and that the keyword and
;; priority maps wire each keyword / priority to its org-faces-* face.  org is
;; required first so the `with-eval-after-load' wiring in org-faces-config fires
;; on load.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-faces-config)

(ert-deftest test-org-faces-config-base-faces-exist ()
  "Normal: every base keyword and priority face is defined."
  (dolist (f '(org-faces-todo org-faces-project org-faces-doing org-faces-waiting
               org-faces-verify org-faces-stalled org-faces-delegated org-faces-failed
               org-faces-done org-faces-cancelled
               org-faces-priority-a org-faces-priority-b org-faces-priority-c org-faces-priority-d))
    (should (facep f))))

(ert-deftest test-org-faces-config-dim-faces-exist ()
  "Normal: every dim variant is defined (auto-dim remaps onto these)."
  (dolist (f '(org-faces-todo-dim org-faces-project-dim org-faces-doing-dim org-faces-waiting-dim
               org-faces-verify-dim org-faces-stalled-dim org-faces-delegated-dim org-faces-failed-dim
               org-faces-done-dim org-faces-cancelled-dim
               org-faces-priority-a-dim org-faces-priority-b-dim org-faces-priority-c-dim org-faces-priority-d-dim))
    (should (facep f))))

(ert-deftest test-org-faces-config-keyword-map ()
  "Normal: representative keywords map to their org-faces-* face."
  (should (eq (cdr (assoc "TODO" org-todo-keyword-faces)) 'org-faces-todo))
  (should (eq (cdr (assoc "VERIFY" org-todo-keyword-faces)) 'org-faces-verify))
  (should (eq (cdr (assoc "CANCELLED" org-todo-keyword-faces)) 'org-faces-cancelled))
  (should (eq (cdr (assoc "DELEGATED" org-todo-keyword-faces)) 'org-faces-delegated)))

(ert-deftest test-org-faces-config-keyword-coverage ()
  "Boundary: all ten keywords are mapped, each to a real face."
  (dolist (kw '("TODO" "PROJECT" "DOING" "WAITING" "VERIFY" "STALLED"
                "DELEGATED" "FAILED" "DONE" "CANCELLED"))
    (let ((face (cdr (assoc kw org-todo-keyword-faces))))
      (should face)
      (should (facep face)))))

(ert-deftest test-org-faces-config-priority-map ()
  "Normal: each priority A-D maps to its org-faces-priority-* face."
  (should (eq (cdr (assq ?A org-priority-faces)) 'org-faces-priority-a))
  (should (eq (cdr (assq ?B org-priority-faces)) 'org-faces-priority-b))
  (should (eq (cdr (assq ?C org-priority-faces)) 'org-faces-priority-c))
  (should (eq (cdr (assq ?D org-priority-faces)) 'org-faces-priority-d)))

(provide 'test-org-faces-config)
;;; test-org-faces-config.el ends here
