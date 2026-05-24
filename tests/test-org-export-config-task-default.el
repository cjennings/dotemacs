;;; test-org-export-config-task-default.el --- Org export task-default test -*- lexical-binding: t; -*-

;;; Commentary:
;; org-export-config.el used to set `org-export-with-tasks' twice in a row
;; ('("TODO") then nil), leaving the final behavior ("export no tasks")
;; contradicted by the adjacent comment.  Requiring the module then ox fires
;; the deferred :config in batch, so this test pins the single intended
;; default: tasks are not exported.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-export-config)
(require 'ox)

(ert-deftest test-org-export-config-tasks-default-is-nil ()
  "Normal: after ox config loads, tasks are excluded from export by default."
  (should (null org-export-with-tasks)))

(provide 'test-org-export-config-task-default)
;;; test-org-export-config-task-default.el ends here
