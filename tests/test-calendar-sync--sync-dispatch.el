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

(provide 'test-calendar-sync--sync-dispatch)
;;; test-calendar-sync--sync-dispatch.el ends here
