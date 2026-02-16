;;; test-org-agenda-config-skip-functions.el --- Tests for org-agenda skip functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the agenda skip functions in org-agenda-config.el:
;; - cj/org-skip-subtree-if-habit
;; - cj/org-skip-subtree-if-priority
;; - cj/org-skip-subtree-if-keyword
;; - cj/org-agenda-skip-subtree-if-not-overdue
;;
;; Uses dynamic timestamp generation (no hardcoded dates) via testutil-org.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

;; Suppress org-mode hooks that load packages unavailable in batch mode
(defmacro test-org-agenda--with-org-buffer (content &rest body)
  "Execute BODY in a temp org buffer with CONTENT, point at first heading.
Suppresses org-mode hooks to avoid loading packages not available in batch."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)
           (text-mode-hook nil))
       (org-mode))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; ---------- cj/org-skip-subtree-if-habit ----------

;;; Normal Cases

(ert-deftest test-org-agenda-config-skip-habit-normal-habit-entry-skips ()
  "Entry with STYLE=habit should return subtree-end (skip)."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Daily exercise\n"
              ":PROPERTIES:\n"
              ":STYLE: habit\n"
              ":END:\n")
    (should (integerp (cj/org-skip-subtree-if-habit)))))

(ert-deftest test-org-agenda-config-skip-habit-normal-non-habit-style-keeps ()
  "Entry with STYLE set to something other than habit should return nil (keep)."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Regular task\n"
              ":PROPERTIES:\n"
              ":STYLE: other\n"
              ":END:\n")
    (should (null (cj/org-skip-subtree-if-habit)))))

(ert-deftest test-org-agenda-config-skip-habit-normal-no-style-property-keeps ()
  "Entry with no STYLE property should return nil (keep)."
  (test-org-agenda--with-org-buffer "* TODO Task without style\n"
    (should (null (cj/org-skip-subtree-if-habit)))))

;;; Boundary Cases

(ert-deftest test-org-agenda-config-skip-habit-boundary-returns-subtree-end-position ()
  "Return value should be the position after the entire subtree."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Habit task\n"
              ":PROPERTIES:\n"
              ":STYLE: habit\n"
              ":END:\n"
              "** Sub-heading\n"
              "Some content\n"
              "* Next task\n")
    (let ((skip-pos (cj/org-skip-subtree-if-habit)))
      (should (integerp skip-pos))
      ;; Skip position should be past the sub-heading content
      (should (> skip-pos (point)))
      ;; But not past the next top-level heading
      (goto-char (point-min))
      (re-search-forward "^\\* Next task")
      (beginning-of-line)
      (should (<= skip-pos (point))))))

;;; ---------- cj/org-skip-subtree-if-priority ----------

;;; Normal Cases

(ert-deftest test-org-agenda-config-skip-priority-normal-matching-priority-skips ()
  "Entry with priority A should be skipped when filtering for A."
  (test-org-agenda--with-org-buffer "* TODO [#A] Important task\n"
    (should (integerp (cj/org-skip-subtree-if-priority ?A)))))

(ert-deftest test-org-agenda-config-skip-priority-normal-different-priority-keeps ()
  "Entry with priority B should not be skipped when filtering for A."
  (test-org-agenda--with-org-buffer "* TODO [#B] Normal task\n"
    (should (null (cj/org-skip-subtree-if-priority ?A)))))

(ert-deftest test-org-agenda-config-skip-priority-normal-no-priority-keeps ()
  "Entry with no priority cookie should not be skipped."
  (test-org-agenda--with-org-buffer "* TODO Plain task\n"
    (should (null (cj/org-skip-subtree-if-priority ?A)))))

(ert-deftest test-org-agenda-config-skip-priority-normal-filter-b-skips-b ()
  "Entry with priority B should be skipped when filtering for B."
  (test-org-agenda--with-org-buffer "* TODO [#B] Normal task\n"
    (should (integerp (cj/org-skip-subtree-if-priority ?B)))))

(ert-deftest test-org-agenda-config-skip-priority-normal-filter-c-skips-c ()
  "Entry with priority C should be skipped when filtering for C."
  (test-org-agenda--with-org-buffer "* TODO [#C] Low priority task\n"
    (should (integerp (cj/org-skip-subtree-if-priority ?C)))))

;;; ---------- cj/org-skip-subtree-if-keyword ----------

;;; Normal Cases

(ert-deftest test-org-agenda-config-skip-keyword-normal-matching-keyword-skips ()
  "Entry with TODO keyword in list should return subtree-end (skip)."
  (test-org-agenda--with-org-buffer "* TODO Some task\n"
    (should (integerp (cj/org-skip-subtree-if-keyword '("TODO"))))))

(ert-deftest test-org-agenda-config-skip-keyword-normal-done-in-list-skips ()
  "Entry with DONE keyword in list should return subtree-end (skip)."
  (test-org-agenda--with-org-buffer "* DONE Completed task\n"
    (should (integerp (cj/org-skip-subtree-if-keyword '("DONE"))))))

(ert-deftest test-org-agenda-config-skip-keyword-normal-keyword-not-in-list-keeps ()
  "Entry with keyword not in filter list should return nil (keep)."
  (test-org-agenda--with-org-buffer "* TODO Some task\n"
    (should (null (cj/org-skip-subtree-if-keyword '("DONE" "CANCELLED"))))))

(ert-deftest test-org-agenda-config-skip-keyword-normal-no-keyword-keeps ()
  "Entry with no TODO keyword should return nil (keep)."
  (test-org-agenda--with-org-buffer "* Just a heading\n"
    (should (null (cj/org-skip-subtree-if-keyword '("TODO" "DONE"))))))

;;; Boundary Cases

(ert-deftest test-org-agenda-config-skip-keyword-boundary-multiple-keywords-in-list ()
  "Filter list with multiple keywords should match any of them."
  (test-org-agenda--with-org-buffer "* DONE Finished task\n"
    (should (integerp (cj/org-skip-subtree-if-keyword '("TODO" "DONE" "CANCELLED"))))))

;;; ---------- cj/org-agenda-skip-subtree-if-not-overdue ----------

;;; Normal Cases

(ert-deftest test-org-agenda-config-skip-overdue-normal-past-scheduled-keeps ()
  "Entry scheduled in the past with TODO keyword is overdue — keep it."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Overdue task\n"
              "SCHEDULED: " (test-org-timestamp-days-ago 7) "\n")
    (should (null (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-normal-future-scheduled-skips ()
  "Entry scheduled in the future is not overdue — skip it."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Future task\n"
              "SCHEDULED: " (test-org-timestamp-days-ahead 7) "\n")
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-normal-past-deadline-keeps ()
  "Entry with past deadline and TODO keyword is overdue — keep it."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Missed deadline\n"
              "DEADLINE: " (test-org-timestamp-days-ago 3) "\n")
    (should (null (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-normal-done-task-skips ()
  "Done task should be skipped even if overdue."
  (test-org-agenda--with-org-buffer
      (concat "* DONE Completed task\n"
              "SCHEDULED: " (test-org-timestamp-days-ago 7) "\n")
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-normal-habit-skips ()
  "Habit should be skipped even if overdue."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Daily habit\n"
              "SCHEDULED: " (test-org-timestamp-days-ago 7) "\n"
              ":PROPERTIES:\n"
              ":STYLE: habit\n"
              ":END:\n")
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-normal-no-todo-keyword-skips ()
  "Entry without a TODO keyword should be skipped."
  (test-org-agenda--with-org-buffer
      (concat "* Just a heading\n"
              "SCHEDULED: " (test-org-timestamp-days-ago 7) "\n")
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

;;; Boundary Cases

(ert-deftest test-org-agenda-config-skip-overdue-boundary-today-scheduled-skips ()
  "Entry scheduled today is NOT overdue (not strictly before today) — skip."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Today task\n"
              "SCHEDULED: " (test-org-timestamp-today) "\n")
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-boundary-no-date-skips ()
  "Entry with TODO but no scheduled/deadline date — not overdue, skip."
  (test-org-agenda--with-org-buffer "* TODO Undated task\n"
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

(ert-deftest test-org-agenda-config-skip-overdue-boundary-future-deadline-skips ()
  "Entry with future deadline is not overdue — skip."
  (test-org-agenda--with-org-buffer
      (concat "* TODO Future deadline\n"
              "DEADLINE: " (test-org-timestamp-days-ahead 14) "\n")
    (should (integerp (cj/org-agenda-skip-subtree-if-not-overdue)))))

(provide 'test-org-agenda-config-skip-functions)
;;; test-org-agenda-config-skip-functions.el ends here
