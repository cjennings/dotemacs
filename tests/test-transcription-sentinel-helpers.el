;;; test-transcription-sentinel-helpers.el --- Tests for sentinel helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the four helpers extracted from `cj/--transcription-sentinel':
;; - `cj/--write-transcript-on-success'  (writes process output to txt)
;; - `cj/--append-to-log'                 (appends event + output to log)
;; - `cj/--update-transcription-status'   (mutates tracking-list status)
;; - `cj/--notify-completion'             (sends completion notification)

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap))

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args) nil))

(require 'transcription-config)

(defmacro test-sentinel-with-temp-file (var extension &rest body)
  "Bind VAR to a fresh temp file path with EXTENSION and run BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "transcription-sentinel-" nil ,extension)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defmacro test-sentinel-with-process-buffer (var content &rest body)
  "Bind VAR to a fresh buffer containing CONTENT; kill it after BODY."
  (declare (indent 2))
  `(let ((,var (generate-new-buffer " *test-sentinel-proc*")))
     (unwind-protect
         (progn
           (with-current-buffer ,var (insert ,content))
           ,@body)
       (when (buffer-live-p ,var) (kill-buffer ,var)))))

(defun test-sentinel-file-contents (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

;;; cj/--write-transcript-on-success

(ert-deftest test-sentinel-write-transcript-normal-writes-on-success ()
  "On success with a live buffer, writes buffer contents to TXT-FILE."
  (test-sentinel-with-temp-file txt-file ".txt"
    (test-sentinel-with-process-buffer buf "hello transcript"
      (cj/--write-transcript-on-success buf t txt-file)
      (should (equal "hello transcript" (test-sentinel-file-contents txt-file))))))

(ert-deftest test-sentinel-write-transcript-boundary-noop-on-failure ()
  "On failure, TXT-FILE is not touched."
  (test-sentinel-with-temp-file txt-file ".txt"
    (delete-file txt-file)
    (test-sentinel-with-process-buffer buf "ignore me"
      (cj/--write-transcript-on-success buf nil txt-file))
    (should-not (file-exists-p txt-file))))

(ert-deftest test-sentinel-write-transcript-boundary-noop-on-dead-buffer ()
  "A dead process buffer is a no-op, even on success."
  (test-sentinel-with-temp-file txt-file ".txt"
    (delete-file txt-file)
    (let ((buf (generate-new-buffer " *dead*")))
      (kill-buffer buf)
      (cj/--write-transcript-on-success buf t txt-file))
    (should-not (file-exists-p txt-file))))

;;; cj/--append-to-log

(ert-deftest test-sentinel-append-to-log-normal-appends-event-and-output ()
  "Appends a timestamped event line and the process-buffer contents."
  (test-sentinel-with-temp-file log-file ".log"
    (with-temp-file log-file (insert "HEADER\n"))
    (test-sentinel-with-process-buffer buf "process stderr here"
      (cj/--append-to-log buf log-file "finished\n"))
    (let ((contents (test-sentinel-file-contents log-file)))
      (should (string-match-p "HEADER" contents))
      (should (string-match-p "finished" contents))
      (should (string-match-p "process stderr here" contents)))))

(ert-deftest test-sentinel-append-to-log-boundary-noop-on-dead-buffer ()
  "A dead process buffer is a no-op."
  (test-sentinel-with-temp-file log-file ".log"
    (with-temp-file log-file (insert "ORIGINAL\n"))
    (let ((buf (generate-new-buffer " *dead*")))
      (kill-buffer buf)
      (cj/--append-to-log buf log-file "event"))
    (should (equal "ORIGINAL\n" (test-sentinel-file-contents log-file)))))

;;; cj/--update-transcription-status

(ert-deftest test-sentinel-update-status-normal-success-marks-complete ()
  "On success, the matching entry's status becomes `complete'."
  (let ((cj/transcriptions-list '((proc-a "/a.m4a" nil running)
                                  (proc-b "/b.m4a" nil running))))
    (cj/--update-transcription-status 'proc-a t)
    (should (eq 'complete (nth 3 (assq 'proc-a cj/transcriptions-list))))
    (should (eq 'running  (nth 3 (assq 'proc-b cj/transcriptions-list))))))

(ert-deftest test-sentinel-update-status-normal-failure-marks-error ()
  "On failure, the matching entry's status becomes `error'."
  (let ((cj/transcriptions-list '((proc-a "/a.m4a" nil running))))
    (cj/--update-transcription-status 'proc-a nil)
    (should (eq 'error (nth 3 (assq 'proc-a cj/transcriptions-list))))))

(ert-deftest test-sentinel-update-status-boundary-unknown-process-noop ()
  "Updating a process that isn't in the list is a no-op."
  (let ((cj/transcriptions-list '((proc-a "/a.m4a" nil running))))
    (cj/--update-transcription-status 'proc-unknown t)
    (should (eq 'running (nth 3 (assq 'proc-a cj/transcriptions-list))))))

;;; cj/--notify-completion

(ert-deftest test-sentinel-notify-completion-normal-success-mentions-txt ()
  "On success, notification body references the TXT-FILE."
  (let (captured)
    (cl-letf (((symbol-function 'cj/--notify)
               (lambda (title body &optional urgency)
                 (setq captured (list title body urgency)))))
      (cj/--notify-completion t "/tmp/out.txt" "/tmp/out.log"))
    (should (string-match-p "out\\.txt" (nth 1 captured)))
    (should-not (nth 2 captured))))  ; normal urgency = nil

(ert-deftest test-sentinel-notify-completion-normal-failure-mentions-log-and-critical ()
  "On failure, notification body references the LOG-FILE at critical urgency."
  (let (captured)
    (cl-letf (((symbol-function 'cj/--notify)
               (lambda (title body &optional urgency)
                 (setq captured (list title body urgency)))))
      (cj/--notify-completion nil "/tmp/out.txt" "/tmp/out.log"))
    (should (string-match-p "out\\.log" (nth 1 captured)))
    (should (eq 'critical (nth 2 captured)))))

(provide 'test-transcription-sentinel-helpers)
;;; test-transcription-sentinel-helpers.el ends here
