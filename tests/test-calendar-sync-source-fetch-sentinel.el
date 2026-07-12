;;; test-calendar-sync-source-fetch-sentinel.el --- Tests for the fetch sentinel -*- lexical-binding: t; -*-

;;; Commentary:
;; calendar-sync--fetch-sentinel-finish is the extracted tail of the async
;; .ics fetch sentinel.  The async-worker tests stub the whole fetch, so its
;; success, failure, and temp-file-cleanup branches were never exercised.
;; These tests drive the helper directly with fake success/failure inputs,
;; no live curl process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'calendar-sync)

(ert-deftest test-calendar-sync-fetch-sentinel-success-passes-temp-file ()
  "Normal: on success the callback gets the temp file and it is not deleted."
  (let ((temp-file (make-temp-file "calendar-sync-sentinel-" nil ".ics"))
        (buffer (generate-new-buffer " *cs-test*"))
        (got 'unset))
    (unwind-protect
        (progn
          (calendar-sync--fetch-sentinel-finish
           t "finished\n" temp-file buffer (lambda (r) (setq got r)))
          (should (equal got temp-file))
          (should (file-exists-p temp-file))
          (should-not (buffer-live-p buffer)))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-calendar-sync-fetch-sentinel-failure-deletes-and-passes-nil ()
  "Error: on failure the temp file is deleted and the callback gets nil."
  (let ((temp-file (make-temp-file "calendar-sync-sentinel-" nil ".ics"))
        (buffer (generate-new-buffer " *cs-test*"))
        (got 'unset)
        (logged nil))
    (unwind-protect
        (cl-letf (((symbol-function 'calendar-sync--log-silently)
                   (lambda (&rest _) (setq logged t))))
          (calendar-sync--fetch-sentinel-finish
           nil "exited abnormally with code 22\n" temp-file buffer
           (lambda (r) (setq got r)))
          (should (null got))
          (should-not (file-exists-p temp-file))
          (should logged)
          (should-not (buffer-live-p buffer)))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-calendar-sync-fetch-sentinel-failure-tolerates-missing-temp-file ()
  "Boundary: failure with the temp file already gone does not error."
  (let ((temp-file (make-temp-file "calendar-sync-sentinel-" nil ".ics"))
        (got 'unset))
    (delete-file temp-file)
    (cl-letf (((symbol-function 'calendar-sync--log-silently) #'ignore))
      (calendar-sync--fetch-sentinel-finish
       nil "failed\n" temp-file nil (lambda (r) (setq got r)))
      (should (null got)))))

(ert-deftest test-calendar-sync-fetch-sentinel-tolerates-dead-buffer ()
  "Boundary: a already-dead process buffer is not touched on success."
  (let ((temp-file (make-temp-file "calendar-sync-sentinel-" nil ".ics"))
        (got 'unset))
    (unwind-protect
        (progn
          (calendar-sync--fetch-sentinel-finish
           t "finished\n" temp-file nil (lambda (r) (setq got r)))
          (should (equal got temp-file)))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(provide 'test-calendar-sync-source-fetch-sentinel)
;;; test-calendar-sync-source-fetch-sentinel.el ends here
