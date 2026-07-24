;;; test-calendar-sync--syncing-p.el --- Tests for the in-flight sync guard  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `calendar-sync--syncing-p' (the per-calendar in-flight check
;; that lets the dispatcher skip an overlapping timer tick) and for the
;; load-state sanitize that clears a stale `syncing' status in a fresh process.
;;
;; Every test runs inside `test-cs-syncing--with-fresh-state', which let-binds
;; a private state hash.  These tests previously cleared the module's global
;; hash on entry and left whatever they wrote in it on exit, which leaked:
;; `...-sync-calendar-skips-when-in-flight' marks "proton" as syncing to
;; exercise the guard, and `test-calendar-sync--sync-dispatch-normal-ics-fetcher'
;; in the sibling dispatch file dispatches a calendar also named "proton".
;; ERT runs them in that order, so the leftover in-flight status made the
;; dispatch a no-op and the sibling failed -- but only when the calendar-sync
;; files ran in one process.  `make test' runs each file separately and the
;; editor hook skipped this family for being over its file cap, so nothing
;; caught it.  Let-binding is what the sibling files already do
;; (test-calendar-sync.el, test-calendar-sync-async-worker.el); this file was
;; the odd one out.

;;; Code:

(require 'ert)
(require 'calendar-sync)

(defmacro test-cs-syncing--with-fresh-state (&rest body)
  "Run BODY with a private, empty per-calendar state hash.
Let-bound rather than cleared in place, so nothing this test writes can
reach a later test."
  (declare (indent 0))
  `(let ((calendar-sync--calendar-states (make-hash-table :test 'equal)))
     ,@body))

;;; calendar-sync--syncing-p

(ert-deftest test-calendar-sync--syncing-p-normal-true-when-syncing ()
  "Normal: a calendar whose status is `syncing' reads as in-flight."
  (test-cs-syncing--with-fresh-state
    (calendar-sync--set-calendar-state "google" '(:status syncing))
    (should (calendar-sync--syncing-p "google"))))

(ert-deftest test-calendar-sync--syncing-p-boundary-nil-when-no-state ()
  "Boundary: a calendar with no recorded state is not in-flight."
  (test-cs-syncing--with-fresh-state
    (should-not (calendar-sync--syncing-p "never-seen"))))

(ert-deftest test-calendar-sync--syncing-p-error-nil-for-terminal-status ()
  "Error: a terminal status (ok / error) is not in-flight."
  (test-cs-syncing--with-fresh-state
    (calendar-sync--set-calendar-state "google" '(:status ok))
    (should-not (calendar-sync--syncing-p "google"))
    (calendar-sync--set-calendar-state "proton" '(:status error))
    (should-not (calendar-sync--syncing-p "proton"))))

;;; Dispatcher guard: an in-flight calendar skips both leaf syncers

(ert-deftest test-calendar-sync--sync-calendar-skips-when-in-flight ()
  "Normal: `calendar-sync--sync-calendar' does not launch a second sync for a
calendar already marked syncing, so an overlapping timer tick is a no-op."
  (test-cs-syncing--with-fresh-state
    (let ((api-calls '()) (ics-calls '()))
      (cl-letf (((symbol-function 'calendar-sync--sync-calendar-api)
                 (lambda (cal) (push cal api-calls)))
                ((symbol-function 'calendar-sync--sync-calendar-ics)
                 (lambda (cal) (push cal ics-calls))))
        (calendar-sync--set-calendar-state "proton" '(:status syncing))
        (calendar-sync--sync-calendar '(:name "proton" :url "https://x/y.ics"
                                              :file "/tmp/c.org"))
        (should (null api-calls))
        (should (null ics-calls))))))

(ert-deftest test-calendar-sync--sync-calendar-dispatches-when-idle ()
  "Boundary: an idle calendar (no in-flight status) still dispatches normally."
  (test-cs-syncing--with-fresh-state
    (let ((ics-calls '()))
      (cl-letf (((symbol-function 'calendar-sync--sync-calendar-ics)
                 (lambda (cal) (push cal ics-calls))))
        (calendar-sync--sync-calendar '(:name "proton" :url "https://x/y.ics"
                                              :file "/tmp/c.org"))
        (should (= 1 (length ics-calls)))))))

;;; Isolation guard

(ert-deftest test-calendar-sync--syncing-state-does-not-leak ()
  "Error: state written inside the macro is gone once it returns.
Pins the isolation itself.  Without it a test marking a calendar syncing
leaves that status set for every later test in the same process, which is
exactly what broke the sibling dispatch test."
  (test-cs-syncing--with-fresh-state
    (calendar-sync--set-calendar-state "leak-probe" '(:status syncing))
    (should (calendar-sync--syncing-p "leak-probe")))
  (should-not (calendar-sync--syncing-p "leak-probe")))

;;; load-state sanitize: a persisted `syncing' status is cleared on load

(ert-deftest test-calendar-sync--load-state-clears-stale-syncing ()
  "Error: a `syncing' status persisted before a crash is reset on load, so the
in-flight guard cannot skip that calendar forever in the new session."
  (test-cs-syncing--with-fresh-state
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
        (delete-directory dir t)))))

(provide 'test-calendar-sync--syncing-p)
;;; test-calendar-sync--syncing-p.el ends here
