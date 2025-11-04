;;; test-transcription-log-cleanup.el --- Tests for log cleanup logic -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--should-keep-log function
;; Categories: Normal cases, Boundary cases

;;; Code:

(require 'ert)
(require 'transcription-config)

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-cj/--should-keep-log-success-keep-disabled ()
  "Test that logs are deleted on success when keep-log is nil."
  (let ((cj/transcription-keep-log-when-done nil))
    (should-not (cj/--should-keep-log t))))

(ert-deftest test-cj/--should-keep-log-success-keep-enabled ()
  "Test that logs are kept on success when keep-log is t."
  (let ((cj/transcription-keep-log-when-done t))
    (should (cj/--should-keep-log t))))

(ert-deftest test-cj/--should-keep-log-error-keep-disabled ()
  "Test that logs are always kept on error, even if keep-log is nil."
  (let ((cj/transcription-keep-log-when-done nil))
    (should (cj/--should-keep-log nil))))

(ert-deftest test-cj/--should-keep-log-error-keep-enabled ()
  "Test that logs are kept on error when keep-log is t."
  (let ((cj/transcription-keep-log-when-done t))
    (should (cj/--should-keep-log nil))))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-cj/--should-keep-log-default-behavior ()
  "Test default behavior (should not keep on success)."
  ;; Default is nil based on defcustom
  (let ((cj/transcription-keep-log-when-done nil))
    (should-not (cj/--should-keep-log t))
    (should (cj/--should-keep-log nil))))

(provide 'test-transcription-log-cleanup)
;;; test-transcription-log-cleanup.el ends here
