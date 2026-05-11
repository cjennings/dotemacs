;;; test-calendar-sync-async-worker.el --- Tests for async calendar conversion  -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for keeping calendar sync parse/write work off the main
;; Emacs thread.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

(ert-deftest test-calendar-sync--worker-command-loads-module-without-init ()
  "The conversion worker should run batch Emacs without user init."
  (let* ((calendar-sync--module-file "/tmp/calendar-sync.el")
         (calendar-sync-past-months 2)
         (calendar-sync-future-months 6)
         (calendar-sync-user-emails '("me@example.test"))
         (command (calendar-sync--worker-command "/tmp/input.ics" "/tmp/output.org")))
    (should (member "--batch" command))
    (should (member "--no-site-file" command))
    (should (member "--no-site-lisp" command))
    (should (member "-L" command))
    (should (member "/tmp/" command))
    (should (member "-l" command))
    (should (member "/tmp/calendar-sync.el" command))
    (should (cl-some (lambda (arg)
                       (and (stringp arg)
                            (string-match-p "calendar-sync-auto-start nil" arg)))
                     command))
    (should (cl-some (lambda (arg)
                       (and (stringp arg)
                            (string-match-p "calendar-sync--batch-convert-file" arg)
                            (string-match-p "/tmp/input\\.ics" arg)
                            (string-match-p "/tmp/output\\.org" arg)
                            (string-match-p "'(\"me@example\\.test\")" arg)))
                     command))))

(ert-deftest test-calendar-sync--worker-command-loads-sibling-modules-without-init ()
  "The worker command should load calendar-sync and sibling modules without init."
  (let* ((calendar-sync--module-file
          (expand-file-name "modules/calendar-sync.el" user-emacs-directory))
         (command (append
                   (calendar-sync--worker-command "/tmp/input.ics" "/tmp/output.org")
                   (list "--eval" "(princ \"loaded\")"))))
    ;; Replace the conversion eval with a harmless smoke expression so this
    ;; test exercises load-path setup without requiring temp ICS input.
    (setq command
          (cl-loop for arg in command
                   collect (if (and (stringp arg)
                                    (string-match-p "calendar-sync--batch-convert-file" arg))
                               "(princ \"\")"
                             arg)))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process
                              (car command) nil t nil (cdr command))))
        (should (= 0 exit-code))
        (should (string-match-p "loaded" (buffer-string)))))))

(ert-deftest test-calendar-sync--batch-convert-file-writes-org-output ()
  "The worker entry point should convert an ICS file and write Org output."
  (let* ((input-file (make-temp-file "calendar-sync-worker-" nil ".ics"))
         (output-file (make-temp-file "calendar-sync-worker-" nil ".org"))
         (event (test-calendar-sync-make-vevent
                 "Worker Meeting"
                 (test-calendar-sync-time-tomorrow-at 9 0)
                 (test-calendar-sync-time-tomorrow-at 10 0)))
         (ics (test-calendar-sync-make-ics event)))
    (unwind-protect
        (progn
          (with-temp-file input-file
            (insert ics))
          (delete-file output-file)
          (calendar-sync--batch-convert-file input-file output-file 3 12 '("me@example.test"))
          (should (file-exists-p output-file))
          (with-temp-buffer
            (insert-file-contents output-file)
            (should (string-match-p "\\* Worker Meeting" (buffer-string)))))
      (when (file-exists-p input-file)
        (delete-file input-file))
      (when (file-exists-p output-file)
        (delete-file output-file)))))

(ert-deftest test-calendar-sync--parse-ics-does-not-require-cj-log-silently ()
  "Worker parsing should not fail when the rest of the config is not loaded."
  (let ((original-log-function
         (when (fboundp 'cj/log-silently)
           (symbol-function 'cj/log-silently))))
    (unwind-protect
        (progn
          (when (fboundp 'cj/log-silently)
            (fmakunbound 'cj/log-silently))
          (should-not (calendar-sync--parse-ics "not valid ics")))
      (when original-log-function
        (fset 'cj/log-silently original-log-function)))))

(ert-deftest test-calendar-sync--sync-calendar-uses-worker-for-parse-and-write ()
  "Sync should fetch to a file and hand parse/write work to a worker process."
  (let ((calendar '(:name "work"
                    :url "https://example.test/work.ics"
                    :file "/tmp/work.org"))
        (calendar-sync--calendar-states (make-hash-table :test 'equal))
        (fetched-url nil)
        (worker-input nil)
        (worker-output nil)
        (saved-state nil))
    (cl-letf (((symbol-function 'calendar-sync--fetch-ics-file)
               (lambda (url callback)
                 (setq fetched-url url)
                 (funcall callback "/tmp/work.ics")))
              ((symbol-function 'calendar-sync--convert-ics-file-async)
               (lambda (ics-file output-file callback)
                 (setq worker-input ics-file
                       worker-output output-file)
                 (funcall callback t "")))
              ((symbol-function 'calendar-sync--parse-ics)
               (lambda (&rest _args)
                 (ert-fail "sync-calendar parsed ICS on the main thread")))
              ((symbol-function 'calendar-sync--write-file)
               (lambda (&rest _args)
                 (ert-fail "sync-calendar wrote the Org file on the main thread")))
              ((symbol-function 'calendar-sync--save-state)
               (lambda ()
                 (setq saved-state t)))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (calendar-sync--sync-calendar calendar))
    (should (string= "https://example.test/work.ics" fetched-url))
    (should (string= "/tmp/work.ics" worker-input))
    (should (string= "/tmp/work.org" worker-output))
    (should saved-state)
    (should (eq 'ok (plist-get (calendar-sync--get-calendar-state "work") :status)))))

(ert-deftest test-calendar-sync--sync-calendar-records-worker-failure ()
  "Worker conversion failures should be reflected in calendar state."
  (let ((calendar '(:name "work"
                    :url "https://example.test/work.ics"
                    :file "/tmp/work.org"))
        (calendar-sync--calendar-states (make-hash-table :test 'equal))
        (saved-state nil))
    (cl-letf (((symbol-function 'calendar-sync--fetch-ics-file)
               (lambda (_url callback)
                 (funcall callback "/tmp/work.ics")))
              ((symbol-function 'calendar-sync--convert-ics-file-async)
               (lambda (_ics-file _output-file callback)
                 (funcall callback nil "parse failed")))
              ((symbol-function 'calendar-sync--save-state)
               (lambda ()
                 (setq saved-state t)))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (calendar-sync--sync-calendar calendar))
    (let ((state (calendar-sync--get-calendar-state "work")))
      (should saved-state)
      (should (eq 'error (plist-get state :status)))
      (should (string-match-p "parse failed" (plist-get state :last-error))))))

(ert-deftest test-calendar-sync--sync-calendar-handles-empty-worker-error ()
  "Worker failures without stderr should still produce a useful state error."
  (let ((calendar '(:name "work"
                    :url "https://example.test/work.ics"
                    :file "/tmp/work.org"))
        (calendar-sync--calendar-states (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'calendar-sync--fetch-ics-file)
               (lambda (_url callback)
                 (funcall callback "/tmp/work.ics")))
              ((symbol-function 'calendar-sync--convert-ics-file-async)
               (lambda (_ics-file _output-file callback)
                 (funcall callback nil nil)))
              ((symbol-function 'calendar-sync--save-state) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (calendar-sync--sync-calendar calendar))
    (let ((state (calendar-sync--get-calendar-state "work")))
      (should (eq 'error (plist-get state :status)))
      (should (string= "Conversion failed" (plist-get state :last-error))))))

(provide 'test-calendar-sync-async-worker)
;;; test-calendar-sync-async-worker.el ends here
