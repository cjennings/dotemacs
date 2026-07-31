;;; agenda-query.el --- Agenda window as JSON for external renderers -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: eager module, lazy dependencies.
;; Eager reason: the entry point must exist for a headless emacsclient --eval.
;; Org is required at call time, so loading the BYTE-COMPILED module costs no
;; startup; loading it from source pays the eval-when-compile requires below.
;; Top-level side effects: none once compiled.
;; Runtime requires: org, org-element, org-agenda, calendar -- all deferred.
;; Direct test load: yes.
;;
;; Answers "what is on the agenda between these two instants" as JSON, so a
;; renderer running outside Emacs can draw it.  Both entry points read
;; `org-agenda-files' and leave every buffer unmodified.
;;
;; Two output profiles over one query.  `cj/agenda-window-json' is canonical:
;; epoch seconds, descriptive field names, and a null end where the source has
;; no range.  `cj/agenda-render-json' adds what the wallpaper renderer reads --
;; s and e in epoch MILLISECONDS, t for the title, and an end every row can
;; actually be drawn with.  `cj/agenda-render-cache-update' writes that profile
;; for today to `cj/agenda-render-cache-file'.
;;
;; Epoch seconds at the boundary is the load-bearing interface choice.  It takes
;; timezone out of the contract entirely: the consumer does its own zoneinfo
;; conversion, and nothing downstream needs to know org's timestamps are naive
;; local time.  DST changeovers stop being a special case for the same reason.
;;
;; The window predicate is intersection, not containment: an event is in the
;; window when it is running at any point during it, so a 23:00-01:00 event
;; belongs to both days it touches.  An all-day entry's extent is its whole day.

;;; Code:

;; Compile-time only, so the byte-compiler sees org's functions while a plain
;; load of this file still pulls in nothing.
(eval-when-compile
  (require 'org)
  (require 'org-element)
  (require 'org-agenda)
  (require 'calendar))

(declare-function org-element--property "org-element-ast")
(declare-function org-element-map "org-element")
(declare-function org-element-parse-buffer "org-element")
(declare-function org-element-type "org-element-ast")
(declare-function org-entry-get "org")
(declare-function org-agenda--timestamp-to-absolute "org-agenda")
(declare-function org-agenda-files "org")
(declare-function org-get-agenda-file-buffer "org")
(declare-function calendar-gregorian-from-absolute "calendar")
(declare-function calendar-absolute-from-gregorian "calendar")

(defconst cj/agenda-query-max-window-seconds (* 366 24 60 60)
  "Widest window `cj/agenda-window-json' will answer, in seconds.

A guard against a units mistake rather than a policy limit.  The consumers are
Python and JavaScript, and `Date.now' returns MILLISECONDS -- passing that
unconverted asks for a window tens of thousands of years wide, which builds
millions of rows and wedges the Emacs daemon Craig is working in.  Failing
loudly costs a renderer one bad frame; the alternative costs him his editor.")

(defconst cj/agenda-query-epoch-floor -2208988800
  "Earliest epoch second `cj/agenda-window-json' accepts (1900-01-01).")

(defconst cj/agenda-query-epoch-ceiling 7258118400
  "Latest epoch second `cj/agenda-window-json' accepts (2200-01-01).

Width alone does not catch the units mistake.  Passing `Date.now()' for BOTH
bounds an hour apart is a plausible-looking 41-day window made of milliseconds,
which sails past the width cap and answers with timestamps in the year 58549.
Bounding the magnitude catches that shape, while still spanning any date an
agenda could legitimately hold.")

(defun cj/--agenda-query-load-org ()
  "Load org at call time.
Kept off the top level so requiring this module costs nothing at startup --
the entry point runs headless, long after Emacs is up.  Idempotent."
  (require 'org)
  (require 'org-element)
  (require 'org-agenda)
  (require 'calendar))

;;; ---------- time helpers ----------

(defun cj/--agenda-query-epoch (sec min hour day month year)
  "Return the epoch second for local time SEC MIN HOUR DAY MONTH YEAR.
Out-of-range fields normalize, so day 32 of July is the 1st of August."
  (time-convert (encode-time (list sec min hour day month year nil -1 nil))
                'integer))

(defun cj/--agenda-query-day-close (day month year)
  "Return the last epoch second of the local day DAY MONTH YEAR.
Reached through the following midnight rather than by adding 86400, so a DST
changeover day is 23 or 25 hours long as it actually is."
  (1- (cj/--agenda-query-epoch 0 0 0 (1+ day) month year)))

(defun cj/--agenda-query-epoch-to-absolute (epoch)
  "Return the absolute day number containing EPOCH, in local time."
  (let ((d (decode-time epoch)))
    (calendar-absolute-from-gregorian (list (nth 4 d) (nth 3 d) (nth 5 d)))))

(defun cj/--agenda-query-day-close-of (absolute)
  "Return the last epoch second of the ABSOLUTE day number."
  (let ((g (calendar-gregorian-from-absolute absolute)))
    (cj/--agenda-query-day-close (nth 1 g) (nth 0 g) (nth 2 g))))

(defun cj/--agenda-query-at-time-on (absolute hour minute)
  "Return the epoch of HOUR:MINUTE local on the ABSOLUTE day number."
  (let ((g (calendar-gregorian-from-absolute absolute)))
    (cj/--agenda-query-epoch 0 minute hour (nth 1 g) (nth 0 g) (nth 2 g))))

;;; ---------- timestamp bounds ----------

(defun cj/--agenda-query-timestamp-bounds (timestamp)
  "Return a bounds plist for org-element TIMESTAMP, or nil when it is nil.

Keys are :start, :end, :all-day and :effective-end, all epoch seconds except
:all-day.  :end is nil when the source carries no range at all; where the
source does carry one, an all-day range's end is the close of its last day,
since a day is its own extent.  :effective-end is what the window predicate
uses: the explicit end, or an all-day entry's day close, or a timed point
event's own instant.

One deliberate divergence from org's literal parse: org records
<2026-07-31 Fri 23:00-01:00> with its end on the SAME day, which would put the
end 22 hours before the start.  A negative duration is useless to any
consumer, so an end that precedes its start is read as crossing midnight."
  (when timestamp
    (let* ((y0 (org-element-property :year-start timestamp))
           (m0 (org-element-property :month-start timestamp))
           (d0 (org-element-property :day-start timestamp))
           (h0 (org-element-property :hour-start timestamp))
           (mi0 (org-element-property :minute-start timestamp))
           (y1 (org-element-property :year-end timestamp))
           (m1 (org-element-property :month-end timestamp))
           (d1 (org-element-property :day-end timestamp))
           (h1 (org-element-property :hour-end timestamp))
           (mi1 (org-element-property :minute-end timestamp))
           (all-day (null h0))
           (start (cj/--agenda-query-epoch 0 (or mi0 0) (or h0 0) d0 m0 y0))
           (ranged (or (not (equal (list y0 m0 d0) (list y1 m1 d1)))
                       (and h1 (not (equal (list h0 mi0) (list h1 mi1))))))
           (same-day (equal (list y0 m0 d0) (list y1 m1 d1)))
           (end (when ranged
                  (if all-day
                      ;; A reversed all-day range (a typo, or a bad ICS
                      ;; import) would otherwise report an end days before
                      ;; its start, and the negative extent would drop the
                      ;; entry from the very window it opens in.
                      (let ((e (cj/--agenda-query-day-close d1 m1 y1)))
                        (and (>= e start) e))
                    (let ((e (cj/--agenda-query-epoch
                              0 (or mi1 0) (or h1 0) d1 m1 y1)))
                      ;; Roll only a SAME-DAY range: that is the shape org
                      ;; records for <23:00-01:00>.  Rolling a genuinely
                      ;; reversed multi-day range would just shift a wrong
                      ;; date by a day and leave it wrong.
                      (when (and same-day (< e start))
                        (setq e (cj/--agenda-query-epoch
                                 0 (or mi1 0) (or h1 0) (1+ d1) m1 y1)))
                      ;; A range that still ends before it starts is
                      ;; malformed.  Report the entry as a point rather than
                      ;; hand a consumer a negative-duration bar.
                      (and (>= e start) e))))))
      (list :start start
            :end end
            :all-day (and all-day t)
            :effective-end (or end
                               (if all-day
                                   (cj/--agenda-query-day-close d0 m0 y0)
                                 start))))))

(defun cj/--agenda-query-repeater-cookie (timestamp)
  "Return TIMESTAMP's raw repeater cookie as a string, or nil when it has none."
  (when timestamp
    (let ((type (org-element-property :repeater-type timestamp))
          (value (org-element-property :repeater-value timestamp))
          (unit (org-element-property :repeater-unit timestamp)))
      (when (and type value unit)
        (concat (pcase type
                  ('cumulate "+")
                  ('catch-up "++")
                  ('restart ".+")
                  (_ "+"))
                (number-to-string value)
                (pcase unit
                  ('hour "h") ('day "d") ('week "w")
                  ('month "m") ('year "y") (_ "")))))))

;;; ---------- window intersection and repeat expansion ----------

(defun cj/--agenda-query-intersects-p (start effective-end win-start win-end)
  "Return non-nil when START..EFFECTIVE-END overlaps WIN-START..WIN-END."
  (and (>= effective-end win-start)
       (<= start win-end)))

(defun cj/--agenda-query-occurrence-on (day base bounds)
  "Return the (START . END) cons and effective end for an occurrence on DAY.

DAY is an absolute day number, BASE the base occurrence's own absolute day,
and BOUNDS its `cj/--agenda-query-timestamp-bounds' plist.  Returns a plist
of :start, :end and :effective-end.

Every field is rebuilt from calendar dates rather than by adding the base's
duration in seconds.  A fixed offset is wrong across a DST boundary: an
all-day occurrence on a 25-hour day would end an hour early, and on a 23-hour
day it would spill into the next."
  (let* ((start (plist-get bounds :start))
         (end (plist-get bounds :end))
         (all-day (plist-get bounds :all-day))
         (decoded (decode-time start))
         (span (if end
                   (- (cj/--agenda-query-epoch-to-absolute end) base)
                 0))
         (occ-start (cj/--agenda-query-at-time-on
                     day (nth 2 decoded) (nth 1 decoded)))
         (occ-end (when end
                    (if all-day
                        (cj/--agenda-query-day-close-of (+ day span))
                      (let ((de (decode-time end)))
                        (cj/--agenda-query-at-time-on
                         (+ day span) (nth 2 de) (nth 1 de)))))))
    (list :start occ-start
          :end occ-end
          :effective-end (or occ-end
                             (if all-day
                                 (cj/--agenda-query-day-close-of day)
                               occ-start)))))

(defun cj/--agenda-query-repeat-occurrences (timestamp bounds win-start win-end)
  "Return every occurrence of repeating TIMESTAMP inside the window.

BOUNDS is its `cj/--agenda-query-timestamp-bounds' plist.  Each candidate day
is offered to `org-agenda--timestamp-to-absolute', which is org's own repeater
arithmetic -- so an occurrence lands exactly where Craig's agenda shows it
rather than where a reimplementation would put it.  A day is an occurrence
when org maps it to itself.

All three repeater styles expand from the base timestamp, including `.+':
org rewrites a restart repeater's base when the task is completed, so for an
open task the base already IS the last repeat.

The scan starts before the window by the event's own day span, so an
occurrence that began earlier and is still running is found -- the same
intersects-not-contains rule the non-repeating path follows.

Repeats are resolved at day granularity, matching org's agenda: an hourly
repeater therefore contributes one row per day rather than one per hour.

Returns one cons per occurrence, oldest first.  One row per occurrence is
deliberate -- a count is derivable from rows, rows are not derivable from a
count."
  (cj/--agenda-query-load-org)
  (let* ((raw (org-element-property :raw-value timestamp))
         (start (plist-get bounds :start))
         (end (plist-get bounds :end))
         (base-day (cj/--agenda-query-epoch-to-absolute start))
         (span (if end
                   (- (cj/--agenda-query-epoch-to-absolute end) base-day)
                 0))
         (first-day (- (cj/--agenda-query-epoch-to-absolute win-start)
                       (1+ (max span 0))))
         (last-day (cj/--agenda-query-epoch-to-absolute win-end))
         (results '()))
    (dotimes (offset (max 0 (1+ (- last-day first-day))))
      (let* ((day (+ first-day offset))
             (hit (when (>= day base-day)
                    (catch :skip
                      (org-agenda--timestamp-to-absolute raw day 'future)))))
        (when (and (integerp hit) (= hit day))
          (let ((occ (cj/--agenda-query-occurrence-on day base-day bounds)))
            (when (cj/--agenda-query-intersects-p
                   (plist-get occ :start) (plist-get occ :effective-end)
                   win-start win-end)
              (push (cons (plist-get occ :start) (plist-get occ :end))
                    results))))))
    (nreverse results)))

(defun cj/--agenda-query-occurrences (timestamp win-start win-end)
  "Return TIMESTAMP's (START . END) epoch conses inside the window.
END is nil for an occurrence the source gave no range.  A non-repeating
timestamp yields at most one cons; a repeating one yields every occurrence."
  (let ((bounds (cj/--agenda-query-timestamp-bounds timestamp)))
    (when (and bounds (<= win-start win-end))
      (if (cj/--agenda-query-repeater-cookie timestamp)
          (cj/--agenda-query-repeat-occurrences timestamp bounds win-start win-end)
        (when (cj/--agenda-query-intersects-p (plist-get bounds :start)
                                              (plist-get bounds :effective-end)
                                              win-start win-end)
          (list (cons (plist-get bounds :start) (plist-get bounds :end))))))))

;;; ---------- collecting events from a buffer ----------

(defun cj/--agenda-query-ancestor-headline (element)
  "Return the nearest headline ancestor of ELEMENT, or nil when there is none."
  (let ((node element))
    (while (and node (not (eq (org-element-type node) 'headline)))
      (setq node (org-element-property :parent node)))
    node))

(defun cj/--agenda-query-visible-p (headline)
  "Return non-nil unless HEADLINE sits in an archived or commented subtree.

Org's agenda skips both, and this query is only useful to the extent it
agrees with the agenda Craig actually sees.  Ancestors count: archiving or
commenting a parent takes its whole subtree off the agenda."
  (let ((node headline)
        (visible t))
    (while (and node visible)
      (when (eq (org-element-type node) 'headline)
        (when (or (org-element-property :archivedp node)
                  (org-element-property :commentedp node))
          (setq visible nil)))
      (setq node (org-element-property :parent node)))
    visible))

(defun cj/--agenda-query-active-p (timestamp)
  "Return non-nil when TIMESTAMP is active -- the kind org agendas show.
Inactive stamps and diary sexps are excluded: the first never reaches an
agenda, and the second cannot be reduced to a plain instant."
  (memq (org-element-property :type timestamp) '(active active-range)))

(defun cj/--agenda-query-title (headline)
  "Return HEADLINE's title as display text.

`:raw-value' already drops the keyword, priority cookie and tags, but keeps
org link syntax verbatim -- a captured web item arrives as
\"[[https://…][Tracking your habits]]\".  Org's own agenda shows the
description, and a renderer has no business parsing org markup, so the link
is reduced here."
  (let ((raw (or (org-element-property :raw-value headline) "")))
    (if (fboundp 'org-link-display-format)
        (org-link-display-format raw)
      raw)))

(defun cj/--agenda-query-event (headline file type timestamp occurrence)
  "Build one JSON-ready event alist.
HEADLINE supplies the title and completion state, FILE the source path, TYPE
the string \"scheduled\", \"deadline\" or \"timestamp\", TIMESTAMP the repeater
cookie, and OCCURRENCE the (START . END) epochs this row renders."
  (let* ((begin (org-element-property :begin headline))
         (keyword (org-element-property :todo-keyword headline))
         (location (org-entry-get begin "LOCATION"))
         (organizer (org-entry-get begin "ORGANIZER"))
         (repeater (cj/--agenda-query-repeater-cookie timestamp))
         (bounds (cj/--agenda-query-timestamp-bounds timestamp)))
    ;; Keys are symbols because `json-serialize' requires that of an alist.
    (list (cons 'title (cj/--agenda-query-title headline))
          (cons 'start (car occurrence))
          (cons 'end (or (cdr occurrence) :null))
          (cons 'all-day (if (plist-get bounds :all-day) t :false))
          (cons 'type type)
          (cons 'file file)
          (cons 'keyword (or keyword :null))
          (cons 'done (if (eq (org-element-property :todo-type headline) 'done)
                          t :false))
          (cons 'repeater (or repeater :null))
          (cons 'location (or location :null))
          (cons 'organizer (or organizer :null)))))

(defun cj/--agenda-query-collect (headline file type timestamp win-start win-end)
  "Return every event row TIMESTAMP contributes, or nil."
  (when (and timestamp
             (cj/--agenda-query-active-p timestamp)
             (cj/--agenda-query-visible-p headline))
    (mapcar (lambda (occurrence)
              (cj/--agenda-query-event headline file type timestamp occurrence))
            (cj/--agenda-query-occurrences timestamp win-start win-end))))

(defun cj/--agenda-query-buffer-events (buffer file win-start win-end)
  "Return the event rows BUFFER contributes for the window, tagged with FILE.

Reads three kinds of timestamp, because org stores them two different ways.
SCHEDULED and DEADLINE are properties on the entry's `planning' element, NOT
children in the parse tree -- so the obvious implementation, mapping over
\\='timestamp, silently returns neither.  It does not error; it just omits the
two entry kinds an agenda is mostly made of.  Do not simplify this back into a
single `org-element-map' over \\='timestamp.  Body timestamps are collected
separately, skipping any whose parent is a planning element so nothing is
counted twice."
  (cj/--agenda-query-load-org)
  (with-current-buffer buffer
    (let ((tree (org-element-parse-buffer))
          (events '()))
      ;; SCHEDULED and DEADLINE, read off the planning element.
      (dolist (planning (org-element-map tree 'planning #'identity))
        (let ((headline (cj/--agenda-query-ancestor-headline planning)))
          (when headline
            (dolist (spec (list (cons "scheduled" :scheduled)
                                (cons "deadline" :deadline)))
              (setq events
                    (nconc events
                           (cj/--agenda-query-collect
                            headline file (car spec)
                            (org-element-property (cdr spec) planning)
                            win-start win-end)))))))
      ;; Plain active timestamps in entry bodies.
      (dolist (timestamp (org-element-map tree 'timestamp #'identity))
        (unless (eq (org-element-type (org-element-property :parent timestamp))
                    'planning)
          (let ((headline (cj/--agenda-query-ancestor-headline timestamp)))
            (when headline
              (setq events
                    (nconc events
                           (cj/--agenda-query-collect
                            headline file "timestamp" timestamp
                            win-start win-end)))))))
      events)))

;;; ---------- output ----------

(defun cj/--agenda-query-sort (events)
  "Return EVENTS ordered by start, then title, so output is deterministic."
  (sort (copy-sequence events)
        (lambda (a b)
          (let ((sa (alist-get 'start a))
                (sb (alist-get 'start b)))
            (if (= sa sb)
                (string< (alist-get 'title a) (alist-get 'title b))
              (< sa sb))))))

(defun cj/--agenda-query-drawable-end (event)
  "Return EVENT's end as something a renderer can draw, in epoch seconds.

The canonical row reports a null end when the source has no range, which is
faithful but not drawable.  Here an all-day entry's extent is its whole day and
a timed point event's is the instant itself, so every row has a width -- even
if that width is zero.  Derived from the row, so this stays a pure function of
canonical output."
  (let ((start (alist-get 'start event))
        (end (alist-get 'end event)))
    (cond
     ((integerp end) end)
     ((eq t (alist-get 'all-day event))
      (let ((d (decode-time start)))
        (cj/--agenda-query-day-close (nth 3 d) (nth 4 d) (nth 5 d))))
     (t start))))

(defun cj/--agenda-query-render-row (event)
  "Return EVENT in the renderer's contract, adding s, e and t.

The renderer reads s and e as epoch MILLISECONDS and t as the title; it takes
everything else and drops it before drawing.  The canonical fields are kept
alongside rather than replaced, so one file serves both the renderer and any
consumer reading the documented shape.  Milliseconds live only in s and e --
`start' and `end' stay seconds, and the two never mix within a key."
  (append (list (cons 's (* 1000 (alist-get 'start event)))
                (cons 'e (* 1000 (cj/--agenda-query-drawable-end event)))
                (cons 't (alist-get 'title event)))
          event))

(defun cj/--agenda-query-write-atomically (path text)
  "Write TEXT to PATH through a temp file and a rename, returning PATH.

The rename is why this matters: the renderer reads on a timer, so a partially
written file would be a parse error on a live surface.  Replacing the file in
one step also makes it its own cache -- when Emacs is down the reader still
gets the last good answer instead of a truncated one.

The temp file is created 0600 by `make-temp-file', so the mode is reset after
the rename; otherwise a reader running as anyone else could not open it.  The
mask is 666, not `default-file-modes' alone -- that is 777 minus the umask, so
using it directly would publish the JSON world-EXECUTABLE."
  (let* ((path (expand-file-name path))
         (dir (file-name-directory path))
         (temp (make-temp-file (expand-file-name ".agenda-query-" dir))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8))
            (write-region text nil temp nil 'silent))
          (rename-file temp path t)
          (set-file-modes path (logand #o666 (default-file-modes)))
          (setq temp nil))
      (when (and temp (file-exists-p temp))
        (delete-file temp)))
    path))

(defun cj/agenda-window-json (start-epoch end-epoch &optional out-path)
  "Return the agenda between START-EPOCH and END-EPOCH as a JSON string.

Both bounds are epoch SECONDS, inclusive.  An event is included when it is
running at any point in the window, so an event that started before
START-EPOCH counts while it is still in progress.

Each element of the returned array carries: title (keyword, priority cookie
and tags already stripped), start, end (null when the source has no range),
all-day, type (\"scheduled\", \"deadline\" or \"timestamp\"), file, keyword,
done, repeater (the raw cookie, or null), location and organizer.  Absent
values are JSON null rather than omitted, so the shape is stable to parse.

A repeating entry contributes one row per occurrence inside the window.

With OUT-PATH, also write the JSON there atomically, leaving the previous
contents intact if anything fails.  Reads `org-agenda-files' and modifies no
buffer, so it is safe to call on a timer or over emacsclient --eval.

Signals when either bound falls outside 1900-2200, or when the window is wider
than `cj/agenda-query-max-window-seconds'.  Both checks exist to surface a
milliseconds-for-seconds mistake, which otherwise answers with dates in the
year 58549 or builds millions of rows."
  (let ((json (json-serialize
               (vconcat (cj/--agenda-query-events start-epoch end-epoch)))))
    (when out-path
      (cj/--agenda-query-write-atomically out-path json))
    json))

(defun cj/--agenda-query-events (start-epoch end-epoch)
  "Return the sorted event rows between START-EPOCH and END-EPOCH.
Shared by both output profiles; the callers do their own writing."
  (unless (numberp start-epoch)
    (signal 'wrong-type-argument (list 'numberp start-epoch)))
  (unless (numberp end-epoch)
    (signal 'wrong-type-argument (list 'numberp end-epoch)))
  (cj/--agenda-query-load-org)
  (let* ((win-start (floor start-epoch))
         (win-end (floor end-epoch))
         (events '()))
    (dolist (bound (list win-start win-end))
      (unless (and (>= bound cj/agenda-query-epoch-floor)
                   (<= bound cj/agenda-query-epoch-ceiling))
        (user-error
         "Agenda bound %s is outside 1900-2200; expected epoch SECONDS (milliseconds?)"
         bound)))
    (when (> (- win-end win-start) cj/agenda-query-max-window-seconds)
      (user-error
       "Agenda window spans %d days; bounds are epoch SECONDS (milliseconds?)"
       (/ (- win-end win-start) 86400)))
    (when (<= win-start win-end)
      (dolist (file (org-agenda-files))
        (when (file-readable-p file)
          (let ((buffer (org-get-agenda-file-buffer file)))
            (when buffer
              (setq events
                    (nconc events
                           (cj/--agenda-query-buffer-events
                            buffer file win-start win-end))))))))
    (cj/--agenda-query-sort events)))

(defconst cj/agenda-render-cache-file
  (expand-file-name
   "settings/agenda.json"
   (let ((xdg (getenv "XDG_CACHE_HOME")))
     ;; An empty XDG_CACHE_HOME is set-but-useless; `or' would take it and
     ;; resolve the whole path relative to whatever the cwd happens to be.
     (if (and xdg (not (string-empty-p xdg)))
         xdg
       (expand-file-name ".cache" (or (getenv "HOME") "~")))))
  "Where the wallpaper renderer reads the day from.

A stable path is load-bearing on both sides.  Because the file is only ever
replaced by a rename, the renderer keeps drawing the last good answer while
Emacs is down, which makes this file its own cache.")

(defun cj/agenda-render-json (start-epoch end-epoch &optional out-path)
  "Return the agenda between START-EPOCH and END-EPOCH in the renderer's shape.

Bounds are epoch SECONDS, as everywhere else here.  Each row carries the
canonical fields plus the three the renderer reads: s and e as epoch
MILLISECONDS, and t as the title.  The renderer works in milliseconds
throughout, and converting once here beats converting in three places there.

Every row has a drawable e, even where the canonical end is null: an all-day
entry spans its day and a point event has zero width.  See
`cj/--agenda-query-drawable-end'."
  (let ((json (json-serialize
               (vconcat (mapcar #'cj/--agenda-query-render-row
                                (cj/--agenda-query-events start-epoch
                                                          end-epoch))))))
    (when out-path
      (cj/--agenda-query-write-atomically out-path json))
    json))

(defun cj/agenda-render-cache-update ()
  "Write the agenda around today to `cj/agenda-render-cache-file'.

The window is three whole local days: yesterday's midnight through tomorrow's
day close.  A consumer drawing a rolling window centred on now needs entries
from either side of midnight, and a single calendar day leaves it with nothing
to draw for the part of its span that falls outside today -- half the surface,
late in the evening.  Three days covers any rolling span up to a full day
either way, and the consumer filters to what it actually draws.

Day boundaries are computed rather than assumed, so the span is 71, 72 or 73
hours across a DST changeover rather than a flat 72.  Returns the path.

Safe to call repeatedly and from a timer: it only reads org files, creates the
cache directory if needed, and replaces the file by rename, so a reader on its
own schedule never sees a partial write."
  (interactive)
  (let* ((d (decode-time (time-convert nil 'integer)))
         (day (nth 3 d)) (month (nth 4 d)) (year (nth 5 d))
         ;; Out-of-range day fields normalize, so day 0 is last month's last
         ;; day and day+1 rolls the month or year without special cases.
         (start (cj/--agenda-query-epoch 0 0 0 (1- day) month year))
         (end (cj/--agenda-query-day-close (1+ day) month year)))
    (make-directory (file-name-directory cj/agenda-render-cache-file) t)
    (cj/agenda-render-json start end cj/agenda-render-cache-file)
    (when (called-interactively-p 'interactive)
      (message "Agenda render cache written to %s" cj/agenda-render-cache-file))
    cj/agenda-render-cache-file))

(provide 'agenda-query)
;;; agenda-query.el ends here
