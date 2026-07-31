;;; test-agenda-query.el --- Tests for the agenda JSON query -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--agenda-query-buffer-events', `cj/agenda-window-json' and the
;; atomic writer.
;;
;; The load-bearing test here is the SCHEDULED/DEADLINE one.  Org stores
;; planning timestamps as properties on a `planning' element rather than as
;; children in the parse tree, so the obvious implementation -- mapping over
;; \='timestamp -- returns neither, silently.  A fixture without a SCHEDULED
;; entry would pass against that broken implementation, so every fixture that
;; matters carries one.
;;
;; Helpers carry a file-unique prefix on purpose: the editor hook loads every
;; agenda-query test file into ONE process, so a shared helper name here would
;; silently redefine its namesake in a sibling file.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'seq)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'agenda-query)

(defun test-aq-json--epoch (min hour day month year)
  "Return the epoch for local MIN HOUR DAY MONTH YEAR."
  (time-convert (encode-time (list 0 min hour day month year nil -1 nil))
                'integer))

(defun test-aq-json--day-window ()
  "Return the window covering all of 2026-07-31 as a cons."
  (cons (test-aq-json--epoch 0 0 31 7 2026)
        (test-aq-json--epoch 59 23 31 7 2026)))

(defmacro test-aq-json--with-org (text &rest body)
  "Parse TEXT in an org-mode buffer and run BODY with `events' bound.
`events' holds the rows the buffer contributes for all of 2026-07-31."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (let* ((window (test-aq-json--day-window))
            (events (cj/--agenda-query-buffer-events
                     (current-buffer) "/tmp/fixture.org"
                     (car window) (cdr window))))
       ,@body)))

(defun test-aq-json--field (event key)
  "Return EVENT's KEY, a symbol."
  (alist-get key event))

(defun test-aq-json--of-type (events type)
  "Return the EVENTS whose type is TYPE."
  (seq-filter (lambda (e) (equal type (test-aq-json--field e 'type))) events))

(defmacro test-aq-json--with-agenda-file (text &rest body)
  "Write TEXT to a temp agenda file and run BODY with `file' and `window' bound.
Kills the visited buffer afterwards so the run leaves no state behind."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "agenda-query-" t))
          (file (expand-file-name "todo.org" dir))
          (window (test-aq-json--day-window)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (let ((org-agenda-files (list file)))
             ,@body))
       (dolist (buffer (buffer-list))
         (when (equal (buffer-file-name buffer) file) (kill-buffer buffer)))
       (delete-directory dir t))))

;;; ---------- the planning-property trap ----------

(ert-deftest test-agenda-query-normal-scheduled-and-deadline-are-found ()
  "Normal: SCHEDULED and DEADLINE both reach the output.

These are properties on the planning element, not children in the parse tree.
An implementation that maps over \='timestamp returns the body stamp only and
drops these two silently -- so this asserts they are present, not merely that
the count is right."
  (test-aq-json--with-org
      "* TODO Standup\nSCHEDULED: <2026-07-31 Fri 09:00> DEADLINE: <2026-07-31 Fri 17:00>\nBody <2026-07-31 Fri 14:00>\n"
    (should (= 1 (length (test-aq-json--of-type events "scheduled"))))
    (should (= 1 (length (test-aq-json--of-type events "deadline"))))
    (should (= 1 (length (test-aq-json--of-type events "timestamp"))))
    (should (equal (test-aq-json--epoch 0 9 31 7 2026)
                   (test-aq-json--field
                    (car (test-aq-json--of-type events "scheduled")) 'start)))
    (should (equal (test-aq-json--epoch 0 17 31 7 2026)
                   (test-aq-json--field
                    (car (test-aq-json--of-type events "deadline")) 'start)))))

(ert-deftest test-agenda-query-normal-scheduled-alone-is-found ()
  "Normal: an entry whose only timestamp is SCHEDULED still produces a row.
The positive control for the trap: with no body timestamp to mask it, a broken
implementation returns nothing at all here."
  (test-aq-json--with-org
      "* TODO Water plants\nSCHEDULED: <2026-07-31 Fri>\n"
    (should (= 1 (length events)))
    (should (equal "scheduled" (test-aq-json--field (car events) 'type)))))

;;; ---------- field content ----------

(ert-deftest test-agenda-query-normal-title-is-stripped ()
  "Normal: the title carries no keyword, priority cookie or tags."
  (test-aq-json--with-org
      "* TODO [#A] Standup with Nerses :work:urgent:\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (equal "Standup with Nerses"
                   (test-aq-json--field (car events) 'title)))))

(ert-deftest test-agenda-query-normal-title-renders-org-links ()
  "Normal: an org link in the title becomes its description text.

Craig's captured web items carry link syntax in the headline, so this is not
hypothetical -- his live agenda has one today.  A wallpaper showing
\"[[https://…][Tracking your habits]]\" is a bug the consumer cannot fix
without parsing org markup."
  (test-aq-json--with-org
      "* TODO [[https://orgmode.org/x][Tracking your habits]]\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (equal "Tracking your habits"
                   (test-aq-json--field (car events) 'title))))
  ;; A bare link has no description, so its target is the best available text.
  (test-aq-json--with-org
      "* TODO [[https://orgmode.org/x]]\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (equal "https://orgmode.org/x"
                   (test-aq-json--field (car events) 'title)))))

(ert-deftest test-agenda-query-normal-location-and-organizer ()
  "Normal: LOCATION and ORGANIZER properties reach the row."
  (test-aq-json--with-org
      "* TODO Standup\nSCHEDULED: <2026-07-31 Fri 09:00>\n:PROPERTIES:\n:LOCATION: Room 3\n:ORGANIZER: Nerses\n:END:\n"
    (should (equal "Room 3" (test-aq-json--field (car events) 'location)))
    (should (equal "Nerses" (test-aq-json--field (car events) 'organizer)))))

(ert-deftest test-agenda-query-normal-completion-state ()
  "Normal: a closed entry reports done true and keeps its raw keyword.
Without this a DONE task carrying today's SCHEDULED renders as an upcoming
event, and the error compounds over a working day."
  (test-aq-json--with-org
      "* DONE Standup\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (eq t (test-aq-json--field (car events) 'done)))
    (should (equal "DONE" (test-aq-json--field (car events) 'keyword)))))

(ert-deftest test-agenda-query-boundary-open-task-is-not-done ()
  "Boundary: an open task reports done false, not null or missing."
  (test-aq-json--with-org
      "* TODO Standup\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (eq :false (test-aq-json--field (car events) 'done)))
    (should (equal "TODO" (test-aq-json--field (car events) 'keyword)))))

(ert-deftest test-agenda-query-boundary-plain-headline-has-null-keyword ()
  "Boundary: an entry with no keyword reports null rather than an empty string."
  (test-aq-json--with-org
      "* Lunch\n<2026-07-31 Fri 12:00-13:00>\n"
    (should (eq :null (test-aq-json--field (car events) 'keyword)))
    (should (eq :false (test-aq-json--field (car events) 'done)))))

(ert-deftest test-agenda-query-boundary-repeater-cookie-in-row ()
  "Boundary: a repeating entry carries its raw cookie so the consumer can mark it."
  (test-aq-json--with-org
      "* TODO Water plants\nSCHEDULED: <2026-07-01 Wed 09:00 +1d>\n"
    (should (= 1 (length events)))
    (should (equal "+1d" (test-aq-json--field (car events) 'repeater)))
    (should (equal (test-aq-json--epoch 0 9 31 7 2026)
                   (test-aq-json--field (car events) 'start)))))

(ert-deftest test-agenda-query-boundary-absent-fields-are-null ()
  "Boundary: absent optional values are null, so the shape stays stable."
  (test-aq-json--with-org
      "* TODO Standup\nSCHEDULED: <2026-07-31 Fri>\n"
    (let ((event (car events)))
      (should (eq :null (test-aq-json--field event 'end)))
      (should (eq :null (test-aq-json--field event 'location)))
      (should (eq :null (test-aq-json--field event 'organizer)))
      (should (eq :null (test-aq-json--field event 'repeater)))
      (should (eq t (test-aq-json--field event 'all-day))))))

;;; ---------- selection ----------

(ert-deftest test-agenda-query-boundary-inactive-excluded ()
  "Boundary: an inactive timestamp never reaches an agenda, so it is excluded."
  (test-aq-json--with-org
      "* Note\nLogged [2026-07-31 Fri 10:00]\n"
    (should-not events)))

(ert-deftest test-agenda-query-boundary-nested-headlines-not-double-counted ()
  "Boundary: a child's timestamp belongs to the child alone.
Collecting per-headline by mapping a headline's whole subtree would attribute
each child stamp to every ancestor too."
  (test-aq-json--with-org
      "* Parent\nSCHEDULED: <2026-07-31 Fri 09:00>\n** Child\nSCHEDULED: <2026-07-31 Fri 11:00>\n"
    (should (= 2 (length events)))
    (should (equal '("Parent" "Child")
                   (mapcar (lambda (e) (test-aq-json--field e 'title)) events)))))

(ert-deftest test-agenda-query-boundary-archived-and-commented-excluded ()
  "Boundary: archived and commented subtrees are skipped, as org's agenda does.
A query that disagrees with the agenda Craig sees is worse than one returning
less, and both of these are off his agenda."
  (test-aq-json--with-org
      (concat "* Archived thing :ARCHIVE:\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
              "* COMMENT Commented\nSCHEDULED: <2026-07-31 Fri 10:00>\n"
              "* Live\nSCHEDULED: <2026-07-31 Fri 11:00>\n")
    (should (equal '("Live")
                   (mapcar (lambda (e) (test-aq-json--field e 'title)) events)))))

(ert-deftest test-agenda-query-boundary-archive-applies-to-children ()
  "Boundary: archiving a parent takes its whole subtree off the agenda."
  (test-aq-json--with-org
      (concat "* Parent :ARCHIVE:\n** Child\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
              "* Live\nSCHEDULED: <2026-07-31 Fri 11:00>\n")
    (should (equal '("Live")
                   (mapcar (lambda (e) (test-aq-json--field e 'title)) events)))))

(ert-deftest test-agenda-query-boundary-outside-window-excluded ()
  "Boundary: an entry on another day contributes nothing."
  (test-aq-json--with-org
      "* TODO Standup\nSCHEDULED: <2026-08-15 Sat 09:00>\n"
    (should-not events)))

(ert-deftest test-agenda-query-boundary-empty-buffer ()
  "Boundary: an empty buffer yields no rows."
  (test-aq-json--with-org "" (should-not events)))

;;; ---------- error cases ----------

(ert-deftest test-agenda-query-error-diary-sexp-is-skipped ()
  "Error: a diary sexp timestamp cannot expand to an instant, and is skipped
without signaling -- one unusable entry must not fail the whole query."
  (test-aq-json--with-org
      "* Floating\n<%%(diary-float t 3 3)>\n* TODO Standup\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (= 1 (length events)))
    (should (equal "Standup" (test-aq-json--field (car events) 'title)))))

(ert-deftest test-agenda-query-error-headline-without-timestamp ()
  "Error: an entry with no timestamp at all contributes nothing."
  (test-aq-json--with-org "* TODO Someday\nJust prose.\n"
    (should-not events)))

;;; ---------- JSON output ----------

(ert-deftest test-agenda-query-normal-json-round-trips ()
  "Normal: the output parses as JSON and preserves the fields.
Booleans arrive as real JSON booleans and absent values as null, so the
consumer can test on them rather than string-matching."
  (test-aq-json--with-agenda-file
      "* DONE Standup\nSCHEDULED: <2026-07-31 Fri 09:00-09:30>\n"
    (let* ((json (cj/agenda-window-json (car window) (cdr window)))
           (parsed (json-parse-string json :object-type 'alist)))
      (should (= 1 (length parsed)))
      (let ((event (aref parsed 0)))
        (should (equal "Standup" (alist-get 'title event)))
        (should (eq t (alist-get 'done event)))
        (should (eq :false (alist-get 'all-day event)))
        (should (equal file (alist-get 'file event)))
        (should (eq :null (alist-get 'repeater event)))
        (should (equal (test-aq-json--epoch 30 9 31 7 2026)
                       (alist-get 'end event)))))))

(ert-deftest test-agenda-query-boundary-json-empty-is-array ()
  "Boundary: an empty result is an empty JSON array, never null.
The consumer parses the same shape whether or not anything is scheduled."
  (let ((org-agenda-files nil))
    (should (equal "[]" (cj/agenda-window-json 0 100)))))

(ert-deftest test-agenda-query-boundary-json-sorted-by-start ()
  "Boundary: rows come back in start order regardless of file order."
  (test-aq-json--with-agenda-file
      "* Late\nSCHEDULED: <2026-07-31 Fri 17:00>\n* Early\nSCHEDULED: <2026-07-31 Fri 08:00>\n"
    (let ((parsed (json-parse-string
                   (cj/agenda-window-json (car window) (cdr window))
                   :object-type 'alist)))
      (should (equal '("Early" "Late")
                     (mapcar (lambda (e) (alist-get 'title e))
                             (append parsed nil)))))))

(ert-deftest test-agenda-query-error-json-rejects-non-numeric-bounds ()
  "Error: a non-numeric bound signals rather than returning a wrong answer."
  (should-error (cj/agenda-window-json "now" 100) :type 'wrong-type-argument)
  (should-error (cj/agenda-window-json 0 nil) :type 'wrong-type-argument))

(ert-deftest test-agenda-query-error-rejects-absurdly-wide-window ()
  "Error: a window wider than the cap signals instead of trying to answer.

This is the milliseconds-for-seconds mistake, which a JavaScript consumer
makes by passing `Date.now()' straight through.  Answering it means building
tens of millions of rows inside the daemon Craig is working in; a renderer
losing one frame is much the cheaper failure."
  (let ((org-agenda-files nil)
        (now 1785474000))
    (should-error (cj/agenda-window-json now (* now 1000)) :type 'user-error)
    ;; A window at the cap is still answered.
    (should (equal "[]" (cj/agenda-window-json
                         now (+ now cj/agenda-query-max-window-seconds))))))

(ert-deftest test-agenda-query-error-rejects-millisecond-bounds ()
  "Error: BOTH bounds in milliseconds is rejected on magnitude.

This is the likelier shape of the units mistake and the width cap cannot see
it: `Date.now()' and `Date.now() + 3600000' look like a 41-day window, so it
passes the width check and answers with timestamps in the year 58549.  A
wrong answer that parses is worse than an error."
  (let ((org-agenda-files nil)
        (ms 1785474000000))
    (should-error (cj/agenda-window-json ms (+ ms 3600000)) :type 'user-error)
    ;; The same instants in seconds are a perfectly ordinary request.
    (should (equal "[]" (cj/agenda-window-json 1785474000 1785477600)))))

;;; ---------- atomic write ----------

(ert-deftest test-agenda-query-normal-writes-file-atomically ()
  "Normal: OUT-PATH receives the JSON and no temp file is left behind."
  (let* ((dir (make-temp-file "agenda-query-out-" t))
         (out (expand-file-name "agenda.json" dir))
         (org-agenda-files nil))
    (unwind-protect
        (progn
          (should (equal "[]" (cj/agenda-window-json 0 100 out)))
          (should (file-exists-p out))
          (with-temp-buffer
            (insert-file-contents out)
            (should (equal "[]" (buffer-string))))
          ;; The rename consumed the temp file; only the target remains.
          (should (equal '("agenda.json")
                         (directory-files
                          dir nil directory-files-no-dot-files-regexp))))
      (delete-directory dir t))))

(ert-deftest test-agenda-query-boundary-write-is-readable-by-others ()
  "Boundary: the written file is not left at the temp file's private 0600.

`make-temp-file' creates 0600, and the rename carries that mode onto the
target.  The whole point of OUT-PATH is that another process reads it, so a
private mode would work only while the reader runs as Craig."
  (let* ((dir (make-temp-file "agenda-query-out-" t))
         (out (expand-file-name "agenda.json" dir)))
    (unwind-protect
        (progn
          (cj/--agenda-query-write-atomically out "[]")
          ;; Readable beyond the owner, following the session umask...
          (should (= (logand (file-modes out) #o044)
                     (logand #o044 (default-file-modes))))
          ;; ...but never executable.  `default-file-modes' is 777 minus the
          ;; umask, so using it unmasked publishes the JSON as 0755.
          ;; No assertion that the mode differs from 0600: under umask 077
          ;; that IS the correct answer, and asserting otherwise would fail
          ;; for a reason that has nothing to do with this code.
          (should (zerop (logand (file-modes out) #o111))))
      (delete-directory dir t))))

(ert-deftest test-agenda-query-boundary-write-replaces-existing ()
  "Boundary: an existing file is replaced wholesale, not appended to."
  (let* ((dir (make-temp-file "agenda-query-out-" t))
         (out (expand-file-name "agenda.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file out (insert "stale content that is much longer"))
          (cj/--agenda-query-write-atomically out "[]")
          (with-temp-buffer
            (insert-file-contents out)
            (should (equal "[]" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest test-agenda-query-error-write-to-missing-dir-preserves-target ()
  "Error: a write into a nonexistent directory signals and litters nothing."
  (let* ((dir (make-temp-file "agenda-query-out-" t))
         (missing (expand-file-name "nope/agenda.json" dir)))
    (unwind-protect
        (progn
          (should-error (cj/--agenda-query-write-atomically missing "[]"))
          (should-not (file-exists-p missing))
          (should-not (directory-files
                       dir nil directory-files-no-dot-files-regexp)))
      (delete-directory dir t))))

(provide 'test-agenda-query)
;;; test-agenda-query.el ends here
