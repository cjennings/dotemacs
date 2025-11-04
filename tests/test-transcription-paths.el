;;; test-transcription-paths.el --- Tests for transcription file path logic -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--transcription-output-files and cj/--transcription-script-path
;; Categories: Normal cases, Boundary cases, Error cases

;;; Code:

(require 'ert)
(require 'transcription-config)

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-cj/--transcription-output-files-simple ()
  "Test output file paths for simple filename."
  (let ((result (cj/--transcription-output-files "meeting.m4a")))
    (should (string= (car result) "meeting.txt"))
    (should (string= (cdr result) "meeting.log"))))

(ert-deftest test-cj/--transcription-output-files-with-path ()
  "Test output file paths with full path."
  (let ((result (cj/--transcription-output-files "/home/user/audio/podcast.mp3")))
    (should (string= (car result) "/home/user/audio/podcast.txt"))
    (should (string= (cdr result) "/home/user/audio/podcast.log"))))

(ert-deftest test-cj/--transcription-output-files-different-extensions ()
  "Test output files for various audio extensions."
  (dolist (ext '("m4a" "mp3" "wav" "flac" "ogg"))
    (let* ((input (format "audio.%s" ext))
           (result (cj/--transcription-output-files input)))
      (should (string= (car result) "audio.txt"))
      (should (string= (cdr result) "audio.log")))))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-cj/--transcription-output-files-multiple-dots ()
  "Test output files for filename with multiple dots."
  (let ((result (cj/--transcription-output-files "meeting.2025-11-04.final.m4a")))
    (should (string= (car result) "meeting.2025-11-04.final.txt"))
    (should (string= (cdr result) "meeting.2025-11-04.final.log"))))

(ert-deftest test-cj/--transcription-output-files-no-extension ()
  "Test output files for filename without extension."
  (let ((result (cj/--transcription-output-files "meeting")))
    (should (string= (car result) "meeting.txt"))
    (should (string= (cdr result) "meeting.log"))))

(ert-deftest test-cj/--transcription-output-files-spaces-in-name ()
  "Test output files for filename with spaces."
  (let ((result (cj/--transcription-output-files "team meeting 2025.m4a")))
    (should (string= (car result) "team meeting 2025.txt"))
    (should (string= (cdr result) "team meeting 2025.log"))))

(ert-deftest test-cj/--transcription-output-files-special-chars ()
  "Test output files for filename with special characters."
  (let ((result (cj/--transcription-output-files "meeting_(final).m4a")))
    (should (string= (car result) "meeting_(final).txt"))
    (should (string= (cdr result) "meeting_(final).log"))))

;; ----------------------------- Script Path Tests -----------------------------

(ert-deftest test-cj/--transcription-script-path-local-whisper ()
  "Test script path for local-whisper backend."
  (let ((cj/transcribe-backend 'local-whisper))
    (should (string-suffix-p "scripts/local-whisper"
                             (cj/--transcription-script-path)))))

(ert-deftest test-cj/--transcription-script-path-openai-api ()
  "Test script path for openai-api backend."
  (let ((cj/transcribe-backend 'openai-api))
    (should (string-suffix-p "scripts/oai-transcribe"
                             (cj/--transcription-script-path)))))

(ert-deftest test-cj/--transcription-script-path-absolute ()
  "Test that script path is absolute."
  (let ((path (cj/--transcription-script-path)))
    (should (file-name-absolute-p path))))

(provide 'test-transcription-paths)
;;; test-transcription-paths.el ends here
