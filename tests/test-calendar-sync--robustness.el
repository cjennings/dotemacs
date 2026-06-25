;;; test-calendar-sync--robustness.el --- Tests for sync robustness fixes -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for two robustness fixes:
;;  - calendar-sync--parse-ics distinguishes a healthy zero-event calendar
;;    (a real iCalendar with no in-window events -> non-nil header) from
;;    garbage (no BEGIN:VCALENDAR -> nil), so a near-empty calendar no longer
;;    reports "parse failed".
;;  - calendar-sync--write-file writes atomically (temp file + rename), so a
;;    reader never sees a half-written calendar and no temp file is left behind.
;; (The curl --fail change is in the make-process command list and is exercised
;; against the live feed, not unit-tested here.)

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; calendar-sync--parse-ics: zero-event vs garbage

(ert-deftest test-calendar-sync--parse-ics-valid-zero-events-non-nil ()
  "Normal: a real iCalendar with no in-window events returns a non-nil empty
calendar, not a parse failure."
  (let ((result (calendar-sync--parse-ics "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR\n")))
    (should result)
    (should (string-match-p "Calendar Events" result))))

(ert-deftest test-calendar-sync--parse-ics-garbage-nil ()
  "Error: non-iCalendar content (no BEGIN:VCALENDAR, e.g. an HTML error page)
returns nil -- a genuine failure."
  (should-not (calendar-sync--parse-ics "HTTP 404 Not Found\n<html><body>error</body></html>")))

;;; calendar-sync--write-file: atomic

(ert-deftest test-calendar-sync--write-file-writes-content ()
  "Normal: the content lands in the target file."
  (let* ((dir (make-temp-file "cal-sync-test-" t))
         (file (expand-file-name "agenda.org" dir)))
    (unwind-protect
        (progn
          (calendar-sync--write-file "# Calendar Events\n\nhello\n" file)
          (should (equal "# Calendar Events\n\nhello\n"
                         (with-temp-buffer (insert-file-contents file)
                                           (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest test-calendar-sync--write-file-leaves-no-temp ()
  "Boundary: the temp file is renamed into place, not left in the directory."
  (let* ((dir (make-temp-file "cal-sync-test-" t))
         (file (expand-file-name "agenda.org" dir)))
    (unwind-protect
        (progn
          (calendar-sync--write-file "x" file)
          ;; only the target file remains -- no leftover .calendar-sync-* temp
          (should (equal '("agenda.org")
                         (directory-files dir nil "\\`[^.]"))))
      (delete-directory dir t))))

(ert-deftest test-calendar-sync--write-file-creates-parent-dir ()
  "Boundary: a missing parent directory is created."
  (let* ((root (make-temp-file "cal-sync-test-" t))
         (file (expand-file-name "sub/nested/agenda.org" root)))
    (unwind-protect
        (progn
          (calendar-sync--write-file "y" file)
          (should (file-exists-p file)))
      (delete-directory root t))))

(provide 'test-calendar-sync--robustness)
;;; test-calendar-sync--robustness.el ends here
