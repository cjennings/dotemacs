;;; test-calendar-sync-no-config-startup.el --- No-config startup tests for calendar-sync  -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for loading calendar-sync without configured calendars.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar calendar-sync-private-config-file)
(defvar calendar-sync-calendars)
(defvar calendar-sync-auto-start)

(ert-deftest test-calendar-sync-no-config-startup-does-not-start-sync ()
  "Loading calendar-sync without calendars should not start timers or fetches."
  (let ((calendar-sync-private-config-file
         (expand-file-name "missing-calendar-sync.local.el" temporary-file-directory))
        (calendar-sync-calendars nil)
        (calendar-sync-auto-start t)
        (timer-called nil)
        (process-called nil)
        messages)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq timer-called t)
                 'test-calendar-sync-timer))
              ((symbol-function 'make-process)
               (lambda (&rest _args)
                 (setq process-called t)
                 nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (load (expand-file-name "modules/calendar-sync.el" user-emacs-directory)
            nil t))
    (should (boundp 'calendar-sync-calendars))
    (should-not calendar-sync-calendars)
    (should-not timer-called)
    (should-not process-called)
    (should-not (cl-some (lambda (msg)
                           (string-match-p "calendar-sync: Syncing" msg))
                         messages))))

(ert-deftest test-calendar-sync-no-config-status-reports-missing-config ()
  "Status should report missing calendar config without erroring."
  (let ((calendar-sync-private-config-file
         (expand-file-name "missing-calendar-sync.local.el" temporary-file-directory))
        (calendar-sync-calendars nil)
        (calendar-sync-auto-start t)
        messages)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _args) nil))
              ((symbol-function 'make-process) (lambda (&rest _args) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (load (expand-file-name "modules/calendar-sync.el" user-emacs-directory)
            nil t)
      (calendar-sync-status))
    (should (member "calendar-sync: No calendars configured (set calendar-sync-calendars)"
                    messages))))

(ert-deftest test-calendar-sync-no-config-loads-private-config-when-present ()
  "Loading calendar-sync should read an ignored private config file when present."
  (let ((config-file (make-temp-file "calendar-sync-local-" nil ".el"))
        (calendar-sync-calendars nil)
        (calendar-sync-auto-start t)
        (timer-called nil)
        (process-called nil))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "(setq calendar-sync-auto-start nil)\n")
            (insert "(setq calendar-sync-calendars\n")
            (insert "      '((:name \"local\" :url \"https://example.test/calendar.ics\" :file \"/tmp/local.org\")))\n"))
          (let ((calendar-sync-private-config-file config-file))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (setq timer-called t)
                         'test-calendar-sync-timer))
                      ((symbol-function 'make-process)
                       (lambda (&rest _args)
                         (setq process-called t)
                         nil))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (load (expand-file-name "modules/calendar-sync.el" user-emacs-directory)
                    nil t))
            (should (equal (calendar-sync--calendar-names) '("local")))
            (should-not calendar-sync-auto-start)
            (should-not timer-called)
            (should-not process-called)))
      (delete-file config-file))))

(ert-deftest test-calendar-sync-no-config-private-config-does-not-auto-start-in-batch ()
  "Private config should not auto-start sync while Emacs is noninteractive."
  (let ((config-file (make-temp-file "calendar-sync-local-" nil ".el"))
        (calendar-sync-calendars nil)
        (calendar-sync-auto-start t)
        (timer-called nil)
        (process-called nil))
    (unwind-protect
        (progn
          (with-temp-file config-file
            (insert "(setq calendar-sync-calendars\n")
            (insert "      '((:name \"local\" :url \"https://example.test/calendar.ics\" :file \"/tmp/local.org\")))\n"))
          (let ((calendar-sync-private-config-file config-file))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest _args)
                         (setq timer-called t)
                         'test-calendar-sync-timer))
                      ((symbol-function 'make-process)
                       (lambda (&rest _args)
                         (setq process-called t)
                         nil))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (load (expand-file-name "modules/calendar-sync.el" user-emacs-directory)
                    nil t))
            (should (equal (calendar-sync--calendar-names) '("local")))
            (should calendar-sync-auto-start)
            (should noninteractive)
            (should-not timer-called)
            (should-not process-called)))
      (delete-file config-file))))

(provide 'test-calendar-sync-no-config-startup)
;;; test-calendar-sync-no-config-startup.el ends here
