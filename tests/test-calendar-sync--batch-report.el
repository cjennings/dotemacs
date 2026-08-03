;;; test-calendar-sync--batch-report.el --- Batch report output tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; `calendar-sync-batch-run-and-report' is what the systemd timer runs, so its
;; printed rows are the only record that survives the process.  Batch Emacs
;; discards *Messages* at exit, which is where the interactive failure path
;; logs its reason -- so a failed row has to carry its recorded `:last-error'
;; in the printed output or the journal shows "error" with no way to tell a
;; cold gpg-agent from a revoked feed token or a dead network.

;;; Code:

(require 'ert)
(require 'cl-lib)  ;; cl-letf; calendar-sync pulls it in transitively, don't rely on that

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calendar-sync)

(defun test-calendar-sync-batch-report--capture (states results)
  "Return the report's printed output for STATES and RESULTS.
STATES is an alist of NAME . PLIST seeded into the state table; RESULTS is
what `calendar-sync-batch-run' is stubbed to return, so the report is
exercised without driving a real sync."
  (let ((calendar-sync--calendar-states (make-hash-table :test 'equal)))
    (dolist (entry states)
      (puthash (car entry) (cdr entry) calendar-sync--calendar-states))
    (cl-letf (((symbol-function 'calendar-sync-batch-run)
               (lambda (&rest _) results)))
      (with-output-to-string
        (calendar-sync-batch-run-and-report)))))

;;; Normal

(ert-deftest test-calendar-sync-batch-report-failed-row-carries-its-reason ()
  "Normal: a failed calendar prints the recorded `:last-error' reason.
Without it the journal records only \"error\", and the operator cannot tell a
cold gpg-agent from a revoked token without re-running the sync by hand."
  (let ((out (test-calendar-sync-batch-report--capture
              '(("google" . (:status error :last-error "Decryption failed"))
                ("proton" . (:status ok)))
              '(("google" . error) ("proton" . ok)))))
    (should (string-match-p "google: error" out))
    (should (string-match-p "Decryption failed" out))))

(ert-deftest test-calendar-sync-batch-report-ok-row-stays-bare ()
  "Normal: a calendar that synced prints its status and nothing more.
A stale `:last-error' from an earlier failure must not be appended to a row
that succeeded this run."
  (let ((out (test-calendar-sync-batch-report--capture
              '(("google" . (:status ok :last-error "Decryption failed")))
              '(("google" . ok)))))
    (should (string-match-p "google: ok" out))
    (should-not (string-match-p "Decryption failed" out))))

;;; Boundary

(ert-deftest test-calendar-sync-batch-report-failure-without-reason-still-prints ()
  "Boundary: a failed row with no recorded reason prints its status alone.
`never' and `syncing' never record a `:last-error', so the reason lookup has
to tolerate nil rather than printing \"nil\" or signalling."
  (let ((out (test-calendar-sync-batch-report--capture
              '(("google" . (:status syncing)))
              '(("google" . syncing) ("absent" . never)))))
    (should (string-match-p "google: syncing" out))
    (should (string-match-p "absent: never" out))
    (should-not (string-match-p "nil" out))))

;;; Error

(defun test-calendar-sync-batch-report--exit-code (results)
  "Return the report's exit code for RESULTS, discarding its printed output."
  (let ((calendar-sync--calendar-states (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'calendar-sync-batch-run)
               (lambda (&rest _) results)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (calendar-sync-batch-run-and-report))))))

(ert-deftest test-calendar-sync-batch-report-exit-code-tracks-failures ()
  "Error: the return value becomes the process exit code, so it stays 1 on any
non-ok row and 0 only when every calendar synced.  Appending the reason to the
printed line must not disturb it."
  (should (equal 1 (test-calendar-sync-batch-report--exit-code '(("google" . error)))))
  (should (equal 1 (test-calendar-sync-batch-report--exit-code
                    '(("google" . ok) ("proton" . never)))))
  (should (equal 0 (test-calendar-sync-batch-report--exit-code '(("google" . ok))))))

(provide 'test-calendar-sync--batch-report)
;;; test-calendar-sync--batch-report.el ends here
