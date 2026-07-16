;;; test-calendar-sync--format-timestamp.el --- Tests for org timestamp formatting -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--format-timestamp.
;; Converts parsed start/end times to org-mode timestamp strings.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--format-timestamp-normal-with-time ()
  "Timed event produces '<YYYY-MM-DD Day HH:MM-HH:MM>'."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) '(2026 3 15 15 30))))
    (should (string-match-p "^<2026-03-15 .* 14:00-15:30>$" result))))

(ert-deftest test-calendar-sync--format-timestamp-normal-all-day ()
  "All-day event (nil hours) produces '<YYYY-MM-DD Day>'."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 16 nil nil))))
    (should (string-match-p "^<2026-03-15 .*>$" result))
    (should-not (string-match-p "[0-9]:[0-9]" result))))

(ert-deftest test-calendar-sync--format-timestamp-normal-includes-weekday ()
  "Timestamp includes abbreviated weekday name."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) '(2026 3 15 15 0))))
    ;; 2026-03-15 is a Sunday
    (should (string-match-p "Sun" result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--format-timestamp-boundary-midnight ()
  "Midnight start and end are formatted as 00:00."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 1 1 0 0) '(2026 1 1 1 0))))
    (should (string-match-p "00:00-01:00" result))))

(ert-deftest test-calendar-sync--format-timestamp-boundary-nil-end ()
  "Nil end with timed start still produces time range."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) nil)))
    ;; No end time means no time range
    (should (string-match-p "^<2026-03-15" result))))

;;; Error Cases

(ert-deftest test-calendar-sync--format-timestamp-error-start-no-time-end-has-time ()
  "All-day start with timed end produces date-only (no time range)."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 15 15 0))))
    ;; start-hour is nil so time-str should be nil
    (should-not (string-match-p "[0-9][0-9]:[0-9][0-9]-" result))))

;;; Multi-day spans (org range syntax)

(ert-deftest test-calendar-sync--format-timestamp-multi-day-timed-spans-dates ()
  "A timed event ending on a later date renders as an org range.
The end DATE used to be discarded and only its time kept, so a four-day
conference produced the same timestamp as a same-day meeting and the agenda
showed it on day one only."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) '(2026 3 18 15 30))))
    (should (equal result "<2026-03-15 Sun 14:00>--<2026-03-18 Wed 15:30>"))))

(ert-deftest test-calendar-sync--format-timestamp-multi-day-all-day-excludes-dtend ()
  "An all-day span ends the day before DTEND, which is non-inclusive.
RFC 5545 3.6.1: DTEND is \"the non-inclusive end of the event\", so an
all-day event running Mar 15-17 carries DTEND 2026-03-18.  Rendering DTEND
verbatim would add a phantom fourth day."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 18 nil nil))))
    (should (equal result "<2026-03-15 Sun>--<2026-03-17 Tue>"))))

(ert-deftest test-calendar-sync--format-timestamp-single-all-day-stays-single ()
  "A one-day all-day event stays a single stamp, never a degenerate range.
Its DTEND is the next day (non-inclusive), so a naive range would turn every
single all-day event into a two-day one -- a worse regression than the
collapse this range support fixes."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 16 nil nil))))
    (should (equal result "<2026-03-15 Sun>"))
    (should-not (string-match-p "--" result))))

(ert-deftest test-calendar-sync--format-timestamp-same-day-timed-stays-compact ()
  "A same-day timed event keeps the compact HH:MM-HH:MM form, not a range.
Guards the common case against the range branch."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) '(2026 3 15 15 30))))
    (should (equal result "<2026-03-15 Sun 14:00-15:30>"))
    (should-not (string-match-p "--" result))))

(ert-deftest test-calendar-sync--format-timestamp-multi-day-all-day-two-days ()
  "Boundary: the shortest real all-day span (two days) renders as a range.
DTEND 2026-03-17 means the event covers Mar 15-16; one day fewer and it
collapses to the single-stamp case above."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 17 nil nil))))
    (should (equal result "<2026-03-15 Sun>--<2026-03-16 Mon>"))))

(ert-deftest test-calendar-sync--format-timestamp-multi-day-span-crosses-month ()
  "Boundary: an all-day span crossing a month boundary decrements correctly.
DTEND 2026-04-01 means the event's last day is 2026-03-31, which exercises
the borrow in `calendar-sync--add-days'."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 30 nil nil) '(2026 4 1 nil nil))))
    (should (equal result "<2026-03-30 Mon>--<2026-03-31 Tue>"))))

(provide 'test-calendar-sync--format-timestamp)
;;; test-calendar-sync--format-timestamp.el ends here
