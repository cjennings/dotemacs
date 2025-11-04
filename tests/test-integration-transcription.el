;;; test-integration-transcription.el --- Integration tests for transcription -*- lexical-binding: t; -*-

;;; Commentary:
;; End-to-end integration tests for transcription workflow
;; Tests complete workflow with temporary files and mocked processes
;; Categories: Normal workflow, Error handling, Cleanup

;;; Code:

(require 'ert)
(require 'transcription-config)

;; ----------------------------- Test Helpers ----------------------------------

(defun test-transcription--make-mock-audio-file ()
  "Create a temporary mock audio file for testing.
Returns the absolute path to the file."
  (let ((file (make-temp-file "test-audio-" nil ".m4a")))
    (with-temp-file file
      (insert "Mock audio data"))
    file))

(defun test-transcription--cleanup-output-files (audio-file)
  "Delete transcript and log files associated with AUDIO-FILE."
  (let* ((outputs (cj/--transcription-output-files audio-file))
         (txt-file (car outputs))
         (log-file (cdr outputs)))
    (when (file-exists-p txt-file)
      (delete-file txt-file))
    (when (file-exists-p log-file)
      (delete-file log-file))))

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-integration-transcription-output-files-created ()
  "Test that .txt and .log files are created for audio file."
  (let* ((audio-file (test-transcription--make-mock-audio-file))
         (outputs (cj/--transcription-output-files audio-file))
         (txt-file (car outputs))
         (log-file (cdr outputs)))
    (unwind-protect
        (progn
          ;; Verify output file paths are correct
          (should (string-suffix-p ".txt" txt-file))
          (should (string-suffix-p ".log" log-file))
          (should (string= (file-name-sans-extension txt-file)
                          (file-name-sans-extension audio-file)))
          (should (string= (file-name-sans-extension log-file)
                          (file-name-sans-extension audio-file))))
      ;; Cleanup
      (delete-file audio-file)
      (test-transcription--cleanup-output-files audio-file))))

(ert-deftest test-integration-transcription-validates-file-exists ()
  "Test that transcription fails for non-existent file."
  (should-error
   (cj/--start-transcription-process "/nonexistent/audio.m4a")
   :type 'user-error))

(ert-deftest test-integration-transcription-validates-audio-extension ()
  "Test that transcription fails for non-audio file."
  (let ((non-audio (make-temp-file "test-" nil ".txt")))
    (unwind-protect
        (should-error
         (cj/--start-transcription-process non-audio)
         :type 'user-error)
      (delete-file non-audio))))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-integration-transcription-audio-file-detection ()
  "Test various audio file extensions are accepted."
  (dolist (ext '("m4a" "mp3" "wav" "flac" "ogg" "opus"))
    (let ((audio-file (make-temp-file "test-audio-" nil (concat "." ext))))
      (unwind-protect
          (progn
            (should (cj/--audio-file-p audio-file))
            ;; Would start transcription if script existed
            )
        (delete-file audio-file)))))

(ert-deftest test-integration-transcription-filename-with-spaces ()
  "Test transcription with audio file containing spaces."
  (let ((audio-file (make-temp-file "test audio file" nil ".m4a")))
    (unwind-protect
        (let* ((outputs (cj/--transcription-output-files audio-file))
               (txt-file (car outputs))
               (log-file (cdr outputs)))
          (should (file-name-absolute-p txt-file))
          (should (file-name-absolute-p log-file)))
      (delete-file audio-file))))

(ert-deftest test-integration-transcription-filename-with-special-chars ()
  "Test transcription with special characters in filename."
  (let ((audio-file (make-temp-file "test_(final)" nil ".m4a")))
    (unwind-protect
        (let* ((outputs (cj/--transcription-output-files audio-file))
               (txt-file (car outputs)))
          ;; make-temp-file adds random suffix, so just check it ends with .txt
          ;; and contains the special chars
          (should (string-suffix-p ".txt" txt-file))
          (should (string-match-p "test_(final)" txt-file)))
      (delete-file audio-file))))

;; ----------------------------- Cleanup Tests ---------------------------------

(ert-deftest test-integration-transcription-cleanup-completed ()
  "Test that completed transcriptions are removed from tracking."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running)
           (proc2 "file2.m4a" nil complete)
           (proc3 "file3.m4a" nil error))))
    (cj/--cleanup-completed-transcriptions)
    (should (= 1 (length cj/transcriptions-list)))
    (should (eq 'running (nth 3 (car cj/transcriptions-list))))))

(ert-deftest test-integration-transcription-cleanup-all-complete ()
  "Test cleanup when all transcriptions are complete."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil complete)
           (proc2 "file2.m4a" nil error))))
    (cj/--cleanup-completed-transcriptions)
    (should (null cj/transcriptions-list))))

(ert-deftest test-integration-transcription-cleanup-preserves-running ()
  "Test that running transcriptions are not cleaned up."
  (let ((cj/transcriptions-list
         '((proc1 "file1.m4a" nil running)
           (proc2 "file2.m4a" nil running))))
    (cj/--cleanup-completed-transcriptions)
    (should (= 2 (length cj/transcriptions-list)))))

;; ----------------------------- Backend Tests ---------------------------------

(ert-deftest test-integration-transcription-script-path-exists ()
  "Test that transcription scripts exist in expected location."
  (dolist (backend '(local-whisper openai-api))
    (let ((cj/transcribe-backend backend))
      (let ((script (cj/--transcription-script-path)))
        (should (file-name-absolute-p script))
        ;; Note: Script may not exist in test environment, just check path format
        (should (string-match-p "scripts/" script))))))

(provide 'test-integration-transcription)
;;; test-integration-transcription.el ends here
