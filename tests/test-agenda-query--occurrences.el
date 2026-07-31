;;; test-agenda-query--occurrences.el --- Tests for window + repeats -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--agenda-query-occurrences' and the repeater cookie helper.
;;
;; Two behaviors under test:
;;
;; 1. The window predicate is intersection, not containment.  An event running
;;    across the window's start edge is in the window -- on a live surface it is
;;    the thing currently happening.
;;
;; 2. A repeating entry contributes one row per occurrence inside the window,
;;    expanded through org's own arithmetic.  The base timestamp of a long-lived
;;    repeater sits far in the past, so returning it raw would put a task that is
;;    genuinely due today outside the window entirely.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)
(require 'org-agenda)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'agenda-query)

(defun test-agenda-query-occ--epoch (min hour day month year)
  "Return the epoch for local MIN HOUR DAY MONTH YEAR."
  (time-convert (encode-time (list 0 min hour day month year nil -1 nil))
                'integer))

(defun test-agenda-query-occ--of (raw start end)
  "Return occurrences of timestamp string RAW within START..END."
  (cj/--agenda-query-occurrences (org-timestamp-from-string raw) start end))

(defun test-agenda-query-occ--starts (raw start end)
  "Return just the start epochs of RAW's occurrences within START..END."
  (mapcar #'car (test-agenda-query-occ--of raw start end)))

;;; ---------- repeater cookie ----------

(ert-deftest test-agenda-query-cookie-normal-all-three-styles ()
  "Normal: each repeater style round-trips to its raw cookie."
  (should (equal "+1d" (cj/--agenda-query-repeater-cookie
                        (org-timestamp-from-string "<2026-07-01 Wed +1d>"))))
  (should (equal "++2w" (cj/--agenda-query-repeater-cookie
                         (org-timestamp-from-string "<2026-07-01 Wed ++2w>"))))
  (should (equal ".+3m" (cj/--agenda-query-repeater-cookie
                         (org-timestamp-from-string "<2026-07-01 Wed .+3m>")))))

(ert-deftest test-agenda-query-cookie-boundary-none ()
  "Boundary: a plain timestamp has no cookie, and nil is not a cookie."
  (should-not (cj/--agenda-query-repeater-cookie
               (org-timestamp-from-string "<2026-07-01 Wed>")))
  (should-not (cj/--agenda-query-repeater-cookie nil)))

;;; ---------- window predicate ----------

(ert-deftest test-agenda-query-occ-normal-inside-window ()
  "Normal: an event wholly inside the window is returned once, with its end."
  (let* ((win-start (test-agenda-query-occ--epoch 0 8 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 0 18 31 7 2026))
         (occ (test-agenda-query-occ--of "<2026-07-31 Fri 09:00-10:00>"
                                         win-start win-end)))
    (should (= 1 (length occ)))
    (should (equal (caar occ) (test-agenda-query-occ--epoch 0 9 31 7 2026)))
    (should (equal (cdar occ) (test-agenda-query-occ--epoch 0 10 31 7 2026)))))

(ert-deftest test-agenda-query-occ-boundary-overlaps-start-edge ()
  "Boundary: an event that began before the window but is still running is in.
This is the 23:00-01:00 case -- the whole reason the predicate is intersects
rather than starts-inside."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 1 8 2026))
         (win-end (test-agenda-query-occ--epoch 0 23 1 8 2026)))
    (should (= 1 (length (test-agenda-query-occ--of
                          "<2026-07-31 Fri 23:00>--<2026-08-01 Sat 01:00>"
                          win-start win-end))))))

(ert-deftest test-agenda-query-occ-boundary-touches-edge-exactly ()
  "Boundary: an event ending exactly at the window start is still included."
  (let* ((win-start (test-agenda-query-occ--epoch 0 10 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 0 18 31 7 2026)))
    (should (= 1 (length (test-agenda-query-occ--of
                          "<2026-07-31 Fri 09:00-10:00>" win-start win-end))))))

(ert-deftest test-agenda-query-occ-boundary-all-day-covers-window ()
  "Boundary: an all-day entry covers any window inside its day."
  (let* ((win-start (test-agenda-query-occ--epoch 0 13 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 30 13 31 7 2026))
         (occ (test-agenda-query-occ--of "<2026-07-31 Fri>" win-start win-end)))
    (should (= 1 (length occ)))
    ;; No range in the source, so no end is invented.
    (should-not (cdar occ))))

(ert-deftest test-agenda-query-occ-boundary-outside-window ()
  "Boundary: an event finishing before the window opens is excluded."
  (let* ((win-start (test-agenda-query-occ--epoch 0 12 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 0 18 31 7 2026)))
    (should-not (test-agenda-query-occ--of "<2026-07-31 Fri 09:00-10:00>"
                                           win-start win-end))))

;;; ---------- repeat expansion ----------

(ert-deftest test-agenda-query-occ-normal-daily-repeat-reaches-today ()
  "Normal: a daily repeater based a month back yields today's occurrence.
Returning the raw base date would put a task genuinely due today far outside
the window -- this is the failure the expansion exists to prevent."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 59 23 31 7 2026))
         (starts (test-agenda-query-occ--starts "<2026-07-01 Wed 09:00 +1d>"
                                                win-start win-end)))
    (should (equal starts (list (test-agenda-query-occ--epoch 0 9 31 7 2026))))))

(ert-deftest test-agenda-query-occ-normal-repeat-preserves-duration ()
  "Normal: an expanded occurrence keeps the base timestamp's duration."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 59 23 31 7 2026))
         (occ (car (test-agenda-query-occ--of "<2026-07-01 Wed 09:00-10:30 +1d>"
                                              win-start win-end))))
    (should occ)
    (should (equal (car occ) (test-agenda-query-occ--epoch 0 9 31 7 2026)))
    (should (equal (cdr occ) (test-agenda-query-occ--epoch 30 10 31 7 2026)))))

(ert-deftest test-agenda-query-occ-boundary-one-row-per-occurrence ()
  "Boundary: a window wider than the interval yields a row per occurrence.
A count is derivable from rows; rows are not derivable from a count, so the
row form is what the query returns."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 27 7 2026))
         (win-end (test-agenda-query-occ--epoch 59 23 31 7 2026))
         (starts (test-agenda-query-occ--starts "<2026-07-01 Wed 09:00 +1d>"
                                                win-start win-end)))
    (should (= 5 (length starts)))
    (should (equal starts
                   (list (test-agenda-query-occ--epoch 0 9 27 7 2026)
                         (test-agenda-query-occ--epoch 0 9 28 7 2026)
                         (test-agenda-query-occ--epoch 0 9 29 7 2026)
                         (test-agenda-query-occ--epoch 0 9 30 7 2026)
                         (test-agenda-query-occ--epoch 0 9 31 7 2026))))))

(ert-deftest test-agenda-query-occ-boundary-weekly-lands-on-its-day ()
  "Boundary: a weekly repeater yields only the days it actually falls on.
2026-07-01 is a Wednesday, so within Mon 27 -- Fri 31 July only Wed 29 counts."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 27 7 2026))
         (win-end (test-agenda-query-occ--epoch 59 23 31 7 2026))
         (starts (test-agenda-query-occ--starts "<2026-07-01 Wed 09:00 +1w>"
                                                win-start win-end)))
    (should (equal starts (list (test-agenda-query-occ--epoch 0 9 29 7 2026))))))

(ert-deftest test-agenda-query-occ-boundary-restart-style-expands ()
  "Boundary: a .+ repeater expands from its base like any other style.

Org rewrites a restart repeater's base timestamp when the task is completed,
so for an open task the base IS the last repeat and base-relative expansion is
what the agenda shows.  All three styles agree here."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 59 23 31 7 2026))
         (expected (list (test-agenda-query-occ--epoch 0 9 31 7 2026))))
    (dolist (raw '("<2026-07-01 Wed 09:00 +1d>"
                   "<2026-07-01 Wed 09:00 ++1d>"
                   "<2026-07-01 Wed 09:00 .+1d>"))
      (should (equal expected
                     (test-agenda-query-occ--starts raw win-start win-end))))))

(ert-deftest test-agenda-query-occ-boundary-repeat-not-yet-started ()
  "Boundary: a repeater whose base is after the window yields nothing."
  (let* ((win-start (test-agenda-query-occ--epoch 0 0 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 59 23 31 7 2026)))
    (should-not (test-agenda-query-occ--starts "<2026-12-01 Tue 09:00 +1d>"
                                               win-start win-end))))

(ert-deftest test-agenda-query-occ-boundary-zero-value-repeater ()
  "Boundary: org treats a zero-value repeater as void, so it behaves as a
plain timestamp -- absent when its base is outside the window, and present
exactly once when the base is inside it.

Both halves are asserted deliberately.  The absent half alone would also pass
against an implementation with no repeater support at all, which makes it no
evidence about zero-value handling."
  (should-not (test-agenda-query-occ--starts
               "<2026-07-01 Wed 09:00 +0d>"
               (test-agenda-query-occ--epoch 0 0 31 7 2026)
               (test-agenda-query-occ--epoch 59 23 31 7 2026)))
  (should (equal (list (test-agenda-query-occ--epoch 0 9 1 7 2026))
                 (test-agenda-query-occ--starts
                  "<2026-07-01 Wed 09:00 +0d>"
                  (test-agenda-query-occ--epoch 0 0 1 7 2026)
                  (test-agenda-query-occ--epoch 59 23 1 7 2026)))))

(ert-deftest test-agenda-query-occ-boundary-repeat-in-progress-at-window-start ()
  "Boundary: a repeating event that began before the window and is still
running is returned.

The non-repeating path honors intersects-not-contains; the repeating path has
to as well.  Scanning only days inside the window misses this, because the
occurrence's own day starts earlier: a nightly 22:00-02:00 job is the thing
actually happening at 03:00, and a renderer that drops it shows an empty slot
during the event."
  (let ((raw "<2026-07-01 Wed 22:00 +1d>--<2026-07-02 Thu 02:00>"))
    (should (= 1 (length (test-agenda-query-occ--of
                          raw
                          (test-agenda-query-occ--epoch 0 0 31 7 2026)
                          (test-agenda-query-occ--epoch 0 6 31 7 2026)))))
    ;; The same occurrence seen from inside its own evening.
    (should (= 1 (length (test-agenda-query-occ--of
                          raw
                          (test-agenda-query-occ--epoch 0 21 31 7 2026)
                          (test-agenda-query-occ--epoch 0 23 31 7 2026)))))))

(ert-deftest test-agenda-query-occ-boundary-repeat-extent-follows-dst ()
  "Boundary: an all-day repeat's extent is recomputed per occurrence day.

US DST ends 2026-11-01, making that day 25 hours long.  Carrying the base
day's length forward as a fixed number of seconds leaves the occurrence
ending an hour early, so a window late on that day sees nothing while the
identical non-repeating entry is found."
  (let ((win-start (test-agenda-query-occ--epoch 30 23 1 11 2026))
        (win-end (test-agenda-query-occ--epoch 59 23 1 11 2026)))
    (should (= 1 (length (test-agenda-query-occ--of "<2026-07-01 Wed +1d>"
                                                    win-start win-end))))
    ;; The non-repeating control: same day, same window, must agree.
    (should (= 1 (length (test-agenda-query-occ--of "<2026-11-01 Sun>"
                                                    win-start win-end))))))

;;; ---------- error cases ----------

(ert-deftest test-agenda-query-occ-error-inverted-window ()
  "Error: a window whose start is after its end matches nothing.
Without the guard the intersection test passes for any long-running event,
which would silently return rows for a nonsense request."
  (let* ((win-start (test-agenda-query-occ--epoch 0 18 31 7 2026))
         (win-end (test-agenda-query-occ--epoch 0 8 31 7 2026)))
    (should-not (test-agenda-query-occ--of "<2026-07-31 Fri 09:00-10:00>"
                                           win-start win-end))
    (should-not (test-agenda-query-occ--of "<2026-07-01 Wed 09:00 +1d>"
                                           win-start win-end))))

(ert-deftest test-agenda-query-occ-error-nil-timestamp ()
  "Error: a nil timestamp yields no occurrences rather than signaling."
  (should-not (cj/--agenda-query-occurrences nil 0 100)))

(provide 'test-agenda-query--occurrences)
;;; test-agenda-query--occurrences.el ends here
