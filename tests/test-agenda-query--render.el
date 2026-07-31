;;; test-agenda-query--render.el --- Tests for the renderer profile -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/agenda-render-json' and the row transform behind it.
;;
;; The renderer reads three keys — s and e as epoch MILLISECONDS, t as the
;; title — and drops everything else before drawing.  Two things therefore have
;; to hold, and both are asserted rather than assumed:
;;
;; 1. s and e really are milliseconds.  A seconds-for-milliseconds slip puts
;;    every bar in 1970 and the surface renders empty, which looks exactly like
;;    "Emacs isn't writing the file".
;; 2. Every row has a drawable e.  The canonical profile reports null where the
;;    source has no range, and null is not a width.
;;
;; Helpers carry a file-unique prefix: the editor hook loads every
;; agenda-query test file into ONE process.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'agenda-query)
;; Supplies `cj/org-todo-keywords', the vocabulary the batch writer sets and
;; the editor's org config reads.
(require 'user-constants)

(defun test-aq-render--epoch (min hour day month year)
  "Return the epoch for local MIN HOUR DAY MONTH YEAR."
  (time-convert (encode-time (list 0 min hour day month year nil -1 nil))
                'integer))

(defmacro test-aq-render--with-agenda-file (text &rest body)
  "Write TEXT to a temp agenda file and run BODY with `rows' bound.
`rows' is the parsed render-profile output for all of 2026-07-31."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "agenda-render-" t))
          (file (expand-file-name "todo.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (let* ((org-agenda-files (list file))
                  (rows (append (json-parse-string
                                 (cj/agenda-render-json
                                  (test-aq-render--epoch 0 0 31 7 2026)
                                  (test-aq-render--epoch 59 23 31 7 2026))
                                 :object-type 'alist)
                                nil)))
             ,@body))
       (dolist (buffer (buffer-list))
         (when (equal (buffer-file-name buffer) file) (kill-buffer buffer)))
       (delete-directory dir t))))

;;; Normal Cases

(ert-deftest test-agenda-render-normal-emits-milliseconds ()
  "Normal: s and e are epoch milliseconds, not seconds.

The renderer works in milliseconds throughout.  Sending seconds puts every bar
in January 1970, which draws as an empty surface — indistinguishable from the
file never being written."
  (test-aq-render--with-agenda-file
      "* Standup\nSCHEDULED: <2026-07-31 Fri 09:00-09:30>\n"
    (let ((row (car rows)))
      (should (= (alist-get 's row)
                 (* 1000 (test-aq-render--epoch 0 9 31 7 2026))))
      (should (= (alist-get 'e row)
                 (* 1000 (test-aq-render--epoch 30 9 31 7 2026))))
      ;; Sanity on the magnitude: milliseconds since 1970 are 13 digits now.
      (should (= 13 (length (number-to-string (alist-get 's row))))))))

(ert-deftest test-agenda-render-normal-title-is-t ()
  "Normal: the title is carried as t, the key the renderer reads."
  (test-aq-render--with-agenda-file
      "* TODO [#A] Standup with Nerses :work:\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
    (should (equal "Standup with Nerses" (alist-get 't (car rows))))))

(ert-deftest test-agenda-render-normal-canonical-fields-survive ()
  "Normal: the canonical fields ride along, since the consumer drops its own.
One file serves both the renderer and anything reading the documented shape."
  (test-aq-render--with-agenda-file
      "* DONE Standup\nSCHEDULED: <2026-07-31 Fri 09:00-09:30>\n"
    (let ((row (car rows)))
      (should (equal "Standup" (alist-get 'title row)))
      (should (eq t (alist-get 'done row)))
      (should (equal "scheduled" (alist-get 'type row)))
      ;; start stays SECONDS while s is milliseconds; the units never mix
      ;; within a key.
      (should (= (alist-get 'start row) (/ (alist-get 's row) 1000))))))

;;; Boundary Cases

(ert-deftest test-agenda-render-boundary-all-day-spans-its-day ()
  "Boundary: an all-day entry gets a drawable e covering its whole day.
Canonically its end is null, and null is not something a renderer can draw."
  (test-aq-render--with-agenda-file
      "* TODO Water plants\nSCHEDULED: <2026-07-31 Fri>\n"
    (let ((row (car rows)))
      (should (eq :null (alist-get 'end row)))
      (should (= (alist-get 's row)
                 (* 1000 (test-aq-render--epoch 0 0 31 7 2026))))
      (should (= (alist-get 'e row)
                 (* 1000 (1- (test-aq-render--epoch 0 0 1 8 2026))))))))

(ert-deftest test-agenda-render-boundary-point-event-has-zero-width ()
  "Boundary: a timed point event ends where it starts rather than at null.
Zero width is a marker the renderer can decide how to draw; null is a crash or
a silently dropped row."
  (test-aq-render--with-agenda-file
      "* Reminder\n<2026-07-31 Fri 14:00>\n"
    (let ((row (car rows)))
      (should (eq :null (alist-get 'end row)))
      (should (= (alist-get 'e row) (alist-get 's row))))))

(ert-deftest test-agenda-render-boundary-every-row-is-drawable ()
  "Boundary: across a mixed day, every row has integer s and e with e >= s.
This is the invariant the surface depends on, so it is asserted over the whole
set rather than one row at a time."
  (test-aq-render--with-agenda-file
      (concat "* TODO All day\nSCHEDULED: <2026-07-31 Fri>\n"
              "* Point\n<2026-07-31 Fri 14:00>\n"
              "* Ranged\n<2026-07-31 Fri 15:00-16:30>\n"
              "* TODO Deadline day\nDEADLINE: <2026-07-31 Fri 17:00>\n")
    (should (= 4 (length rows)))
    (dolist (row rows)
      (should (integerp (alist-get 's row)))
      (should (integerp (alist-get 'e row)))
      (should (>= (alist-get 'e row) (alist-get 's row)))
      (should (stringp (alist-get 't row))))))

(ert-deftest test-agenda-render-boundary-empty-is-array ()
  "Boundary: an empty day is an empty array, not null.
The renderer parses the same shape whether or not anything is scheduled."
  (let ((org-agenda-files nil))
    (should (equal "[]" (cj/agenda-render-json 1785474000 1785477600)))))

;;; ---------- the batch reader has to know the keyword vocabulary ----------

(ert-deftest test-agenda-render-normal-custom-keyword-is-parsed ()
  "Normal: a config keyword is recognised, so it leaves the title.

The batch writer runs without the editor's init, so it has to set
`org-todo-keywords' itself.  When it does not, org stops parsing the headline
as a task at all: DOING and the priority cookie stay glued to the front of the
title and the row reads as having no keyword.  That is a wrong answer that
still parses as JSON, which is the worst kind."
  (let ((org-todo-keywords cj/org-todo-keywords))
    (test-aq-render--with-agenda-file
        "* DOING [#A] Justin Johns advisor projects\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
      (let ((row (car rows)))
        (should (equal "Justin Johns advisor projects" (alist-get 't row)))
        (should (equal "DOING" (alist-get 'keyword row)))))))

(ert-deftest test-agenda-render-boundary-unknown-keyword-stays-in-title ()
  "Boundary: with stock keywords the same headline degrades visibly.

Pinning the failure mode, not endorsing it.  This is what the surface showed
before the batch writer learned the vocabulary, and it is why the test above
exists."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (test-aq-render--with-agenda-file
        "* DOING [#A] Justin Johns advisor projects\nSCHEDULED: <2026-07-31 Fri 09:00>\n"
      (should (string-prefix-p "DOING" (alist-get 't (car rows)))))))

;;; ---------- the cache writer ----------

(ert-deftest test-agenda-render-normal-cache-update-writes-today ()
  "Normal: the cache writer covers three whole local days and creates its dir.

Yesterday's midnight through tomorrow's day close.  A consumer drawing a
rolling window centred on now needs both sides of midnight: with a single
calendar day, the part of its span outside today has nothing to draw, which
late in the evening is half the surface.

This is the function that actually ships the feature, so it gets its own
coverage rather than riding on the tests for the profile beneath it.  The
window assertion is the point: a writer that quietly covered the last hour,
or the next 24 from now, would still produce a plausible-looking file."
  (let* ((dir (make-temp-file "agenda-cache-" t))
         ;; A path two levels deep, so directory creation is exercised.
         (cj/agenda-render-cache-file
          (expand-file-name "settings/agenda.json" dir))
         (org-agenda-files nil)
         (windows '()))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/agenda-render-json)
                   (lambda (start end &optional out)
                     (push (cons start end) windows)
                     (when out (cj/--agenda-query-write-atomically out "[]"))
                     "[]")))
          (should (equal cj/agenda-render-cache-file
                         (cj/agenda-render-cache-update)))
          (should (file-exists-p cj/agenda-render-cache-file))
          (let* ((window (car windows))
                 (start (decode-time (car window)))
                 (now (decode-time)))
            ;; Opens at local midnight...
            (should (= 0 (nth 2 start)))
            (should (= 0 (nth 1 start)))
            (should (= 0 (nth 0 start)))
            ;; ...on yesterday, and closes at the end of tomorrow.
            (should (= (car window)
                       (cj/--agenda-query-epoch
                        0 0 0 (1- (nth 3 now)) (nth 4 now) (nth 5 now))))
            (should (= (cdr window)
                       (cj/--agenda-query-day-close
                        (1+ (nth 3 now)) (nth 4 now) (nth 5 now))))
            ;; Three whole days, give or take an hour at a DST changeover.
            (let ((hours (/ (- (cdr window) (car window)) 3600.0)))
              (should (>= hours 70.9))
              (should (<= hours 73.1)))))
      (delete-directory dir t))))

(ert-deftest test-agenda-render-boundary-cache-update-is-idempotent ()
  "Boundary: writing twice leaves one file and no temp litter."
  (let* ((dir (make-temp-file "agenda-cache-" t))
         (cj/agenda-render-cache-file (expand-file-name "agenda.json" dir))
         (org-agenda-files nil))
    (unwind-protect
        (progn
          (cj/agenda-render-cache-update)
          (cj/agenda-render-cache-update)
          (should (equal '("agenda.json")
                         (directory-files
                          dir nil directory-files-no-dot-files-regexp))))
      (delete-directory dir t))))

;;; Error Cases

(ert-deftest test-agenda-render-error-cache-update-unwritable-parent ()
  "Error: an unwritable cache location signals rather than failing silently.
A silent failure here looks identical to an empty agenda on the surface."
  (let* ((dir (make-temp-file "agenda-cache-ro-" t))
         (cj/agenda-render-cache-file
          (expand-file-name "settings/agenda.json" dir))
         (org-agenda-files nil))
    (unwind-protect
        (progn
          (set-file-modes dir #o500)
          (should-error (cj/agenda-render-cache-update)))
      (set-file-modes dir #o700)
      (delete-directory dir t))))

(ert-deftest test-agenda-render-error-bounds-are-still-seconds ()
  "Error: the render profile takes SECONDS in, even though it emits
milliseconds.  The direction of conversion is one-way and the input guards
still apply, so a caller passing milliseconds is told rather than answered."
  (let ((org-agenda-files nil)
        (ms 1785474000000))
    (should-error (cj/agenda-render-json ms (+ ms 3600000)) :type 'user-error)))

;;; Back to Normal Cases -- the profile's own writer

(ert-deftest test-agenda-render-normal-writes-out-path ()
  "Normal: with an out-path the render profile writes it atomically."
  (let* ((dir (make-temp-file "agenda-render-out-" t))
         (out (expand-file-name "agenda.json" dir))
         (org-agenda-files nil))
    (unwind-protect
        (progn
          (cj/agenda-render-json 1785474000 1785477600 out)
          (should (file-exists-p out))
          (with-temp-buffer
            (insert-file-contents out)
            (should (equal "[]" (buffer-string))))
          (should (zerop (logand (file-modes out) #o111))))
      (delete-directory dir t))))

(provide 'test-agenda-query--render)
;;; test-agenda-query--render.el ends here
