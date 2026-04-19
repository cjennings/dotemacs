;;; test-transcription-start-helpers.el --- Tests for process-start helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the two helpers extracted from `cj/--start-transcription-process':
;; - `cj/--init-log-file' (writes the initial log header)
;; - `cj/--track-transcription' (pushes an entry onto the active list)

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap))

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args) nil))

(require 'transcription-config)

(defmacro test-start-helpers-with-temp-log (var &rest body)
  "Bind VAR to a fresh temp log-file path and run BODY, cleaning up after."
  (declare (indent 1))
  `(let ((,var (make-temp-file "transcription-log-" nil ".log")))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defun test-start-helpers-file-contents (path)
  "Return contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;; cj/--init-log-file — Normal Cases

(ert-deftest test-init-log-file-normal-creates-file ()
  "The log file exists after `cj/--init-log-file' runs."
  (test-start-helpers-with-temp-log log-file
    (delete-file log-file)  ; start from a clean slate
    (cj/--init-log-file log-file "/tmp/foo.m4a" "/tmp/script")
    (should (file-exists-p log-file))))

(ert-deftest test-init-log-file-normal-records-audio-path ()
  "Log header includes the full AUDIO-FILE path."
  (test-start-helpers-with-temp-log log-file
    (cj/--init-log-file log-file "/tmp/foo.m4a" "/tmp/script")
    (should (string-match-p "Audio file: /tmp/foo\\.m4a"
                            (test-start-helpers-file-contents log-file)))))

(ert-deftest test-init-log-file-normal-records-script-path ()
  "Log header includes the SCRIPT path."
  (test-start-helpers-with-temp-log log-file
    (cj/--init-log-file log-file "/tmp/foo.m4a" "/usr/local/bin/whisper")
    (should (string-match-p "Script: /usr/local/bin/whisper"
                            (test-start-helpers-file-contents log-file)))))

(ert-deftest test-init-log-file-normal-records-backend ()
  "Log header includes the current `cj/transcribe-backend' value."
  (test-start-helpers-with-temp-log log-file
    (let ((cj/transcribe-backend 'local-whisper))
      (cj/--init-log-file log-file "/tmp/foo.m4a" "/tmp/script"))
    (should (string-match-p "Backend: local-whisper"
                            (test-start-helpers-file-contents log-file)))))

(ert-deftest test-init-log-file-normal-records-start-timestamp ()
  "Log header begins with a `Transcription started:' line."
  (test-start-helpers-with-temp-log log-file
    (cj/--init-log-file log-file "/tmp/foo.m4a" "/tmp/script")
    (should (string-match-p "Transcription started:"
                            (test-start-helpers-file-contents log-file)))))

;;; cj/--init-log-file — Boundary Cases

(ert-deftest test-init-log-file-boundary-overwrites-existing ()
  "An existing log file is overwritten (not appended to)."
  (test-start-helpers-with-temp-log log-file
    (with-temp-file log-file (insert "STALE CONTENT\n"))
    (cj/--init-log-file log-file "/tmp/foo.m4a" "/tmp/script")
    (should-not (string-match-p "STALE CONTENT"
                                (test-start-helpers-file-contents log-file)))))

;;; cj/--track-transcription — Normal Cases

(ert-deftest test-track-transcription-normal-adds-entry-to-list ()
  "A call to `cj/--track-transcription' pushes one entry."
  (let ((cj/transcriptions-list '()))
    (cj/--track-transcription 'proc1 "/tmp/foo.m4a")
    (should (= 1 (length cj/transcriptions-list)))))

(ert-deftest test-track-transcription-normal-entry-shape ()
  "Pushed entry is (PROC AUDIO-FILE TIME running)."
  (let ((cj/transcriptions-list '()))
    (cj/--track-transcription 'proc1 "/tmp/foo.m4a")
    (let ((entry (car cj/transcriptions-list)))
      (should (eq 'proc1 (nth 0 entry)))
      (should (equal "/tmp/foo.m4a" (nth 1 entry)))
      (should (nth 2 entry))          ; start-time is non-nil
      (should (eq 'running (nth 3 entry))))))

(ert-deftest test-track-transcription-normal-multiple-entries-accumulate ()
  "Multiple calls accumulate onto the list."
  (let ((cj/transcriptions-list '()))
    (cj/--track-transcription 'p1 "/a.m4a")
    (cj/--track-transcription 'p2 "/b.m4a")
    (cj/--track-transcription 'p3 "/c.m4a")
    (should (= 3 (length cj/transcriptions-list)))))

(ert-deftest test-track-transcription-normal-preserves-existing-entries ()
  "Pre-existing entries in the list are not disturbed."
  (let ((cj/transcriptions-list '((old-proc "/old.m4a" nil complete))))
    (cj/--track-transcription 'new-proc "/new.m4a")
    (should (= 2 (length cj/transcriptions-list)))
    (should (member '(old-proc "/old.m4a" nil complete) cj/transcriptions-list))))

(provide 'test-transcription-start-helpers)
;;; test-transcription-start-helpers.el ends here
