;;; test-org-config-org-log-done.el --- Lock org-log-done to one home -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-log-done' had two setters: `cj/org-todo-settings' in org-config.el set
;; it nil and org-roam-config.el set it to 'time, so the effective value was
;; load-order-dependent.  `cj/org-todo-settings' is now the single home and
;; sets 'time, which records a CLOSED timestamp on TODO->DONE — the behavior
;; the dated-completion workflow depends on.  This test calls the settings
;; function in isolation (no org-roam-config required) so it would fail if the
;; nil value or the org-roam duplicate ever came back.

;;; Code:

(require 'ert)
(require 'org)        ;; declares org-log-done special so the let below is dynamic
(require 'org-config)

(ert-deftest test-org-config-org-log-done-set-to-time ()
  "Normal: cj/org-todo-settings sets org-log-done to 'time."
  (let ((org-log-done nil))
    (cj/org-todo-settings)
    (should (eq org-log-done 'time))))

(provide 'test-org-config-org-log-done)
;;; test-org-config-org-log-done.el ends here
