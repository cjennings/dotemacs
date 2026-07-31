;;; test-agenda-query--bounds.el --- Tests for timestamp bounds -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--agenda-query-timestamp-bounds', which turns an org-element
;; timestamp into epoch bounds for the JSON query.
;;
;; The contract under test:
;; - :start is the epoch of the timestamp's start.
;; - :end is the epoch of an explicit range end, or nil when the source has no
;;   range.  A null end is information the consumer cannot re-derive, so it is
;;   never filled in with a guess.
;; - :all-day is t when the timestamp carries no hour.
;; - :effective-end is what the window predicate uses: the explicit end, or for
;;   an all-day entry the end of its last day, or for a timed point event the
;;   start itself.  This is the "treat the day as its extent" rule, kept out of
;;   :end so the reported shape stays faithful to the source.
;;
;; Helpers carry a file-unique prefix on purpose: the editor hook loads every
;; agenda-query test file into ONE process, so a shared helper name here would
;; silently redefine its namesake in a sibling file.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)
(require 'org-agenda)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'agenda-query)

(defun test-aq-bounds--epoch (sec min hour day month year)
  "Return the epoch second for the local time SEC MIN HOUR DAY MONTH YEAR."
  (time-convert (encode-time (list sec min hour day month year nil -1 nil))
                'integer))

(defun test-aq-bounds--of (raw)
  "Parse timestamp string RAW and return its bounds plist."
  (cj/--agenda-query-timestamp-bounds (org-timestamp-from-string raw)))

;;; Normal Cases

(ert-deftest test-agenda-query-bounds-normal-timed-range ()
  "Normal: a same-day timed range reports both ends and is not all-day."
  (let ((b (test-aq-bounds--of "<2026-07-31 Fri 23:00-23:30>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 23 31 7 2026)))
    (should (equal (plist-get b :end)
                   (test-aq-bounds--epoch 0 30 23 31 7 2026)))
    (should-not (plist-get b :all-day))
    (should (equal (plist-get b :effective-end) (plist-get b :end)))))

(ert-deftest test-agenda-query-bounds-normal-timed-point ()
  "Normal: a timed point event has no end, and its extent is the instant."
  (let ((b (test-aq-bounds--of "<2026-07-31 Fri 14:00>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 14 31 7 2026)))
    (should-not (plist-get b :end))
    (should-not (plist-get b :all-day))
    (should (equal (plist-get b :effective-end) (plist-get b :start)))))

;;; Boundary Cases

(ert-deftest test-agenda-query-bounds-boundary-all-day-extent ()
  "Boundary: an all-day entry reports no end but extends over its whole day."
  (let ((b (test-aq-bounds--of "<2026-07-31 Fri>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 0 31 7 2026)))
    (should-not (plist-get b :end))
    (should (plist-get b :all-day))
    ;; One second short of the next midnight -- the day is the extent.
    (should (equal (plist-get b :effective-end)
                   (1- (test-aq-bounds--epoch 0 0 0 1 8 2026))))))

(ert-deftest test-agenda-query-bounds-boundary-multi-day-all-day ()
  "Boundary: a multi-day all-day range ends at the close of its last day."
  (let ((b (test-aq-bounds--of "<2026-07-31 Fri>--<2026-08-02 Sun>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 0 31 7 2026)))
    (should (plist-get b :all-day))
    (should (equal (plist-get b :end)
                   (1- (test-aq-bounds--epoch 0 0 0 3 8 2026))))
    (should (equal (plist-get b :effective-end) (plist-get b :end)))))

(ert-deftest test-agenda-query-bounds-boundary-midnight-start ()
  "Boundary: an explicit 00:00 is a timed event, not an all-day one."
  (let ((b (test-aq-bounds--of "<2026-07-31 Fri 00:00>")))
    (should-not (plist-get b :all-day))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 0 31 7 2026)))))

(ert-deftest test-agenda-query-bounds-boundary-dst-spring-forward ()
  "Boundary: a timestamp on a DST changeover day still resolves to one epoch.
US DST began 2026-03-08.  The interface is epoch seconds precisely so the
consumer never has to know the source timestamps are naive local time."
  (let ((b (test-aq-bounds--of "<2026-03-08 Sun 13:00>")))
    (should (integerp (plist-get b :start)))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 13 8 3 2026)))))

(ert-deftest test-agenda-query-bounds-boundary-dst-day-is-23-hours ()
  "Boundary: the day-extent rule follows real clock time, not a fixed 86400.
2026-03-08 loses an hour, so its all-day extent is one second short of 23
hours.  Adding a constant day would overshoot into the next day."
  (let ((b (test-aq-bounds--of "<2026-03-08 Sun>")))
    (should (equal (plist-get b :effective-end)
                   (1- (test-aq-bounds--epoch 0 0 0 9 3 2026))))
    (should (= (- (plist-get b :effective-end) (plist-get b :start))
               (1- (* 23 3600))))))

(ert-deftest test-agenda-query-bounds-boundary-same-day-range-crosses-midnight ()
  "Boundary: a range whose end precedes its start is read as crossing midnight.

Org records <2026-07-31 Fri 23:00-01:00> with day-end EQUAL to day-start, so
reading it literally puts the end 22 hours before the start.  A negative
duration is meaningless to a renderer, and the row would also vanish from the
very window it belongs to, since its effective end would sit before the
window opens."
  (let ((b (test-aq-bounds--of "<2026-07-31 Fri 23:00-01:00>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 23 31 7 2026)))
    (should (equal (plist-get b :end)
                   (test-aq-bounds--epoch 0 0 1 1 8 2026)))
    (should (> (plist-get b :end) (plist-get b :start)))
    (should (= (- (plist-get b :end) (plist-get b :start)) (* 2 3600)))))

(ert-deftest test-agenda-query-bounds-boundary-reversed-multi-day-range ()
  "Boundary: a genuinely reversed multi-day range reports no end at all.

The midnight roll is scoped to same-day ranges, which is the shape org uses
for <23:00-01:00>.  Rolling a reversed multi-day range would shift a wrong
date by one day and leave it still wrong, so instead the entry is reported as
a point -- a consumer can draw that, where a negative-duration bar is
meaningless."
  (let ((b (test-aq-bounds--of "<2026-08-02 Sun 09:00>--<2026-07-31 Fri 08:00>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 9 2 8 2026)))
    (should-not (plist-get b :end))
    (should (equal (plist-get b :effective-end) (plist-get b :start)))))

(ert-deftest test-agenda-query-bounds-boundary-reversed-all-day-range ()
  "Boundary: a reversed ALL-DAY range also reports no end.

The same malformed shape as the timed case, and the same consequence if it
slips through: the negative extent would sit before the start, so the entry
would vanish from the very day it opens on.  A typo or a bad ICS import
produces this."
  (let ((b (test-aq-bounds--of "<2026-08-05 Wed>--<2026-08-01 Sat>")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 0 5 8 2026)))
    (should-not (plist-get b :end))
    ;; Falls back to its own day, so it still appears on 5 August.
    (should (> (plist-get b :effective-end) (plist-get b :start)))))

;;; Error Cases

(ert-deftest test-agenda-query-bounds-error-inactive-still-computes ()
  "Error: bounds are purely arithmetic -- filtering inactive stamps is a
separate concern, so an inactive timestamp still yields usable bounds."
  (let ((b (test-aq-bounds--of "[2026-07-31 Fri 09:00]")))
    (should (equal (plist-get b :start)
                   (test-aq-bounds--epoch 0 0 9 31 7 2026)))))

(ert-deftest test-agenda-query-bounds-error-nil-timestamp ()
  "Error: a nil timestamp yields nil rather than signaling."
  (should-not (cj/--agenda-query-timestamp-bounds nil)))

(provide 'test-agenda-query--bounds)
;;; test-agenda-query--bounds.el ends here
