;;; test-org-config-noop-state-log.el --- Suppress no-op state-change logs -*- lexical-binding: t; -*-

;;; Commentary:
;; org's state-change logging writes "- State X from X" lines for no-op
;; transitions (identical from/to state), which carry no information.  With
;; `org-log-into-drawer' nil those lines also land inline, where they wedge
;; between a heading and its planning line and break org's parser.  Two fixes
;; in `cj/org-todo-settings': log state changes into :LOGBOOK: drawers
;; (`org-log-into-drawer' t), and suppress no-op state logs at the
;; `org-add-log-setup' choke point via `cj/org--suppress-noop-state-log'.

;;; Code:

(require 'ert)
(require 'org)          ;; declares the org-log-* vars special
(require 'org-config)

(ert-deftest test-org-config-log-into-drawer-enabled ()
  "Normal: cj/org-todo-settings enables org-log-into-drawer (LOGBOOK)."
  (let ((org-log-into-drawer nil))
    (cj/org-todo-settings)
    (should (eq org-log-into-drawer t))))

(ert-deftest test-org-config-noop-state-log-p-identical-is-noop ()
  "Normal: identical from/to state on a 'state purpose is a no-op."
  (should (cj/org--noop-state-log-p 'state "TODO" "TODO")))

(ert-deftest test-org-config-noop-state-log-p-real-change-not-noop ()
  "Normal: a genuine state change is not a no-op."
  (should-not (cj/org--noop-state-log-p 'state "DONE" "TODO")))

(ert-deftest test-org-config-noop-state-log-p-nil-prev-not-noop ()
  "Boundary: a nil previous state (initial log) is not a no-op."
  (should-not (cj/org--noop-state-log-p 'state "TODO" nil)))

(ert-deftest test-org-config-noop-state-log-p-non-state-purpose-not-noop ()
  "Boundary: identical strings under a non-state purpose are not suppressed."
  (should-not (cj/org--noop-state-log-p 'note "x" "x")))

(ert-deftest test-org-config-noop-state-log-p-nil-both-not-noop ()
  "Error: a nil/nil state pair is not a suppressible no-op."
  (should-not (cj/org--noop-state-log-p 'state nil nil)))

(ert-deftest test-org-config-suppress-advice-skips-noop ()
  "Normal: the advice does NOT call through for a no-op state transition."
  (let ((called nil))
    (cj/org--suppress-noop-state-log
     (lambda (&rest _) (setq called t)) 'state "TODO" "TODO")
    (should-not called)))

(ert-deftest test-org-config-suppress-advice-passes-real-change ()
  "Normal: the advice calls through for a genuine state change."
  (let ((called nil))
    (cj/org--suppress-noop-state-log
     (lambda (&rest _) (setq called t)) 'state "DONE" "TODO")
    (should called)))

(provide 'test-org-config-noop-state-log)
;;; test-org-config-noop-state-log.el ends here
