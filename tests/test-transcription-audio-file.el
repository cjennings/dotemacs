;;; test-transcription-audio-file.el --- Tests for audio file detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--audio-file-p function
;; Categories: Normal cases, Boundary cases, Error cases

;;; Code:

(require 'ert)
(require 'transcription-config)

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-cj/--audio-file-p-m4a ()
  "Test that .m4a files are recognized as audio."
  (should (cj/--audio-file-p "meeting.m4a")))

(ert-deftest test-cj/--audio-file-p-mp3 ()
  "Test that .mp3 files are recognized as audio."
  (should (cj/--audio-file-p "podcast.mp3")))

(ert-deftest test-cj/--audio-file-p-wav ()
  "Test that .wav files are recognized as audio."
  (should (cj/--audio-file-p "recording.wav")))

(ert-deftest test-cj/--audio-file-p-flac ()
  "Test that .flac files are recognized as audio."
  (should (cj/--audio-file-p "music.flac")))

(ert-deftest test-cj/--audio-file-p-with-path ()
  "Test audio file recognition with full path."
  (should (cj/--audio-file-p "/home/user/recordings/meeting.m4a")))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-cj/--audio-file-p-uppercase-extension ()
  "Test that uppercase extensions are recognized."
  (should (cj/--audio-file-p "MEETING.M4A")))

(ert-deftest test-cj/--audio-file-p-mixed-case ()
  "Test that mixed case extensions are recognized."
  (should (cj/--audio-file-p "podcast.Mp3")))

(ert-deftest test-cj/--audio-file-p-no-extension ()
  "Test that files without extension are not recognized."
  (should-not (cj/--audio-file-p "meeting")))

(ert-deftest test-cj/--audio-file-p-empty-string ()
  "Test that empty string is not recognized as audio."
  (should-not (cj/--audio-file-p "")))

(ert-deftest test-cj/--audio-file-p-dotfile ()
  "Test that dotfiles without proper extension are not recognized."
  (should-not (cj/--audio-file-p ".hidden")))

(ert-deftest test-cj/--audio-file-p-multiple-dots ()
  "Test file with multiple dots but audio extension."
  (should (cj/--audio-file-p "meeting.2025-11-04.final.m4a")))

;; ------------------------------ Error Cases ----------------------------------

(ert-deftest test-cj/--audio-file-p-not-audio ()
  "Test that non-audio files are not recognized."
  (should-not (cj/--audio-file-p "document.pdf")))

(ert-deftest test-cj/--audio-file-p-text-file ()
  "Test that text files are not recognized as audio."
  (should-not (cj/--audio-file-p "notes.txt")))

(ert-deftest test-cj/--audio-file-p-org-file ()
  "Test that org files are not recognized as audio."
  (should-not (cj/--audio-file-p "tasks.org")))

(ert-deftest test-cj/--audio-file-p-video-file ()
  "Test that video files are not recognized as audio."
  (should-not (cj/--audio-file-p "video.mp4")))

(ert-deftest test-cj/--audio-file-p-nil ()
  "Test that nil input returns nil."
  (should-not (cj/--audio-file-p nil)))

(provide 'test-transcription-audio-file)
;;; test-transcription-audio-file.el ends here
