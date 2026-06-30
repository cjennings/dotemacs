;;; test-calendar-sync--syncing-p.el --- Tests for the in-flight sync guard  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `calendar-sync--syncing-p' (the per-calendar in-flight check
;; that lets the dispatcher skip an overlapping timer tick) and for the
;; load-state sanitize that clears a stale `syncing' status in a fresh process.

;;; Code:

(require 'ert)
(require 'calendar-sync)

(defun test-cs-syncing--reset ()
  "Clear the module's per-calendar state hash."
  (clrhash calendar-sync--calendar-states))

;;; calendar-sync--syncing-p

(ert-deftest test-calendar-sync--syncing-p-normal-true-when-syncing ()
  "Normal: a calendar whose status is `syncing' reads as in-flight."
  (test-cs-syncing--reset)
  (calendar-sync--set-calendar-state "google" '(:status syncing))
  (should (calendar-sync--syncing-p "google")))

(ert-deftest test-calendar-sync--syncing-p-boundary-nil-when-no-state ()
  "Boundary: a calendar with no recorded state is not in-flight."
  (test-cs-syncing--reset)
  (should-not (calendar-sync--syncing-p "never-seen")))

(ert-deftest test-calendar-sync--syncing-p-error-nil-for-terminal-status ()
  "Error: a terminal status (ok / error) is not in-flight."
  (test-cs-syncing--reset)
  (calendar-sync--set-calendar-state "google" '(:status ok))
  (should-not (calendar-sync--syncing-p "google"))
  (calendar-sync--set-calendar-state "proton" '(:status error))
  (should-not (calendar-sync--syncing-p "proton")))

;;; Dispatcher guard: an in-flight calendar skips both leaf syncers

(ert-deftest test-calendar-sync--sync-calendar-skips-when-in-flight ()
  "Normal: `calendar-sync--sync-calendar' does not launch a second sync for a
calendar already marked syncing, so an overlapping timer tick is a no-op."
  (test-cs-syncing--reset)
  (let ((api-calls '()) (ics-calls '()))
    (cl-letf (((symbol-function 'calendar-sync--sync-calendar-api)
               (lambda (cal) (push cal api-calls)))
              ((symbol-function 'calendar-sync--sync-calendar-ics)
               (lambda (cal) (push cal ics-calls))))
      (calendar-sync--set-calendar-state "proton" '(:status syncing))
      (calendar-sync--sync-calendar '(:name "proton" :url "https://x/y.ics"
                                            :file "/tmp/c.org"))
      (should (null api-calls))
      (should (null ics-calls)))))

(ert-deftest test-calendar-sync--sync-calendar-dispatches-when-idle ()
  "Boundary: an idle calendar (no in-flight status) still dispatches normally."
  (test-cs-syncing--reset)
  (let ((ics-calls '()))
    (cl-letf (((symbol-function 'calendar-sync--sync-calendar-ics)
               (lambda (cal) (push cal ics-calls))))
      (calendar-sync--sync-calendar '(:name "proton" :url "https://x/y.ics"
                                            :file "/tmp/c.org"))
      (should (= 1 (length ics-calls))))))

;;; load-state sanitize: a persisted `syncing' status is cleared on load

(ert-deftest test-calendar-sync--load-state-clears-stale-syncing ()
  "Error: a `syncing' status persisted before a crash is reset on load, so the
in-flight guard cannot skip that calendar forever in the new session."
  (test-cs-syncing--reset)
  (let* ((dir (make-temp-file "cs-state-" t))
         (calendar-sync--state-file (expand-file-name "state.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file calendar-sync--state-file
            (prin1 '((timezone-offset . nil)
                     (calendar-states . (("google" . (:status syncing)))))
                   (current-buffer)))
          (calendar-sync--load-state)
          (should-not (calendar-sync--syncing-p "google")))
      (delete-directory dir t))))

(provide 'test-calendar-sync--syncing-p)
;;; test-calendar-sync--syncing-p.el ends here
