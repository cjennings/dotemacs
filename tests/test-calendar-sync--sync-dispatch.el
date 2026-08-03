;;; test-calendar-sync--sync-dispatch.el --- Tests for fetcher dispatch  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `calendar-sync--sync-calendar' dispatch.  It routes a
;; calendar plist to the API helper when :fetcher is \\='api, and to the .ics
;; path otherwise (\\='ics, nil, or any other value).  The two leaf syncers are
;; stubbed so no external process runs.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'calendar-sync)

(defmacro test-sync-dispatch--with-stubs (&rest body)
  "Run BODY with both leaf syncers stubbed to record their calls.
Binds `api-calls' and `ics-calls' to lists of the calendars each received."
  (declare (indent 0))
  `(let ((api-calls '())
         (ics-calls '()))
     (cl-letf (((symbol-function 'calendar-sync--sync-calendar-api)
                (lambda (cal) (push cal api-calls)))
               ((symbol-function 'calendar-sync--sync-calendar-ics)
                (lambda (cal) (push cal ics-calls))))
       ,@body)))

;;; Normal

(ert-deftest test-calendar-sync--sync-dispatch-normal-api-fetcher ()
  "Normal: :fetcher \\='api routes to the API syncer only."
  (test-sync-dispatch--with-stubs
    (let ((cal '(:name "google" :fetcher api :account "work"
                       :calendar-id "primary" :file "/tmp/gcal.org")))
      (calendar-sync--sync-calendar cal)
      (should (equal (list cal) api-calls))
      (should (null ics-calls)))))

(ert-deftest test-calendar-sync--sync-dispatch-normal-ics-fetcher ()
  "Normal: :fetcher \\='ics routes to the .ics syncer only."
  (test-sync-dispatch--with-stubs
    (let ((cal '(:name "proton" :fetcher ics :url "https://x/y.ics"
                       :file "/tmp/pcal.org")))
      (calendar-sync--sync-calendar cal)
      (should (equal (list cal) ics-calls))
      (should (null api-calls)))))

;;; Boundary

(ert-deftest test-calendar-sync--sync-dispatch-boundary-missing-fetcher-defaults-ics ()
  "Boundary: a calendar with no :fetcher key defaults to the .ics path.
This is what keeps existing Proton/.ics config working unchanged."
  (test-sync-dispatch--with-stubs
    (let ((cal '(:name "legacy" :url "https://x/y.ics" :file "/tmp/c.org")))
      (calendar-sync--sync-calendar cal)
      (should (equal (list cal) ics-calls))
      (should (null api-calls)))))

(ert-deftest test-calendar-sync--sync-dispatch-boundary-nil-fetcher-defaults-ics ()
  "Boundary: an explicit :fetcher nil also defaults to the .ics path."
  (test-sync-dispatch--with-stubs
    (let ((cal '(:name "legacy" :fetcher nil :url "https://x/y.ics"
                       :file "/tmp/c.org")))
      (calendar-sync--sync-calendar cal)
      (should (equal (list cal) ics-calls))
      (should (null api-calls)))))

;;; Error

(ert-deftest test-calendar-sync--sync-dispatch-error-unknown-fetcher-defaults-ics ()
  "Error: an unrecognized :fetcher value falls back to the .ics path.
Only \\='api is special-cased; anything else takes the safe default rather
than crashing."
  (test-sync-dispatch--with-stubs
    (let ((cal '(:name "weird" :fetcher carrier-pigeon :url "https://x/y.ics"
                       :file "/tmp/c.org")))
      (calendar-sync--sync-calendar cal)
      (should (equal (list cal) ics-calls))
      (should (null api-calls)))))

(ert-deftest test-calendar-sync--sync-dispatch-error-leaf-signal-is-contained ()
  "Error: a syncer that signals marks the calendar failed instead of propagating.

Resolving a `:secret-host' feed reads authinfo.gpg, and a cold gpg-agent makes
that signal a `file-error' before any process starts — so the failure arrives
synchronously, where the async callbacks that normally record a failure never
run."
  (let ((failed '())
        (calendar-sync--calendar-states (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'calendar-sync--sync-calendar-ics)
               (lambda (_) (signal 'file-error '("Decryption failed"))))
              ((symbol-function 'calendar-sync--mark-sync-failed)
               (lambda (name reason) (push (cons name reason) failed))))
      (calendar-sync--sync-calendar
       '(:name "google" :url "https://x/y.ics" :file "/tmp/c.org"))
      (should (equal "google" (car (car failed)))))))

(ert-deftest test-calendar-sync--sync-all-continues-past-a-failing-calendar ()
  "Error: one calendar's synchronous failure does not stop the ones after it.

This is the whole cost of leaving the signal uncontained: on a machine whose
feeds resolve through authinfo, the first calendar's decryption error aborted
the entire run, so calendars that would have synced fine never got the chance."
  (let ((synced '())
        (calendar-sync--calendar-states (make-hash-table :test 'equal))
        (calendar-sync-calendars
         '((:name "bad" :url "https://x/a.ics" :file "/tmp/a.org")
           (:name "good" :url "https://x/b.ics" :file "/tmp/b.org"))))
    (cl-letf (((symbol-function 'calendar-sync--sync-calendar-ics)
               (lambda (cal)
                 (if (equal (plist-get cal :name) "bad")
                     (signal 'file-error '("Decryption failed"))
                   (push (plist-get cal :name) synced))))
              ((symbol-function 'calendar-sync--mark-sync-failed)
               (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (calendar-sync--sync-all-calendars)
      (should (equal '("good") synced)))))

(provide 'test-calendar-sync--sync-dispatch)
;;; test-calendar-sync--sync-dispatch.el ends here
