;;; test-integration-recording-monitor-capture.el --- Integration test for monitor audio capture -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-14

;;; Commentary:
;;
;; Integration test that verifies phone call audio (speaker output) is captured
;; during recording. This tests the actual PulseAudio device selection end-to-end.
;;
;; This test:
;; 1. Plays known speech through speakers (simulating phone call audio)
;; 2. Records it using the configured monitor device
;; 3. Transcribes the recording
;; 4. Verifies the expected text appears in the transcription
;;
;; Requirements:
;; - Audio system must be working (PulseAudio/PipeWire)
;; - Recording devices must be configured (C-; r c)
;; - paplay must be available
;; - Transcription backend must be configured
;;
;; This is a MANUAL integration test - not suitable for CI since it requires
;; working audio hardware.
;;
;; USAGE:
;;   M-x ert RET test-integration-recording-monitor-capture RET
;;
;;   Or from command line:
;;   make test-file FILE=tests/test-integration-recording-monitor-capture.el

;;; Code:

(require 'ert)
(require 'video-audio-recording)
(require 'transcription-config)

(defvar test-recording-monitor--test-audio-file
  (expand-file-name "tests/fixtures/audio/speaker-output-test.wav"
                    user-emacs-directory)
  "Test audio file with known speech for speaker output testing.")

(defvar test-recording-monitor--expected-phrases
  '("quick brown fox" "lazy dog" "speaker output test")
  "Phrases expected in transcription.
We use partial matches since transcription may not be 100% accurate.")

(defun test-recording-monitor--wait-for-transcription (audio-file timeout)
  "Wait for transcription of AUDIO-FILE to complete, up to TIMEOUT seconds.
Returns the path to the .txt file if successful, nil if timeout."
  (let* ((txt-file (concat (file-name-sans-extension audio-file) ".txt"))
         (deadline (time-add (current-time) (seconds-to-time timeout))))
    ;; Wait for .txt file to be created and have content
    (while (and (time-less-p (current-time) deadline)
                (or (not (file-exists-p txt-file))
                    (= 0 (file-attribute-size (file-attributes txt-file)))))
      (sleep-for 1)
      (message "Waiting for transcription... (%d seconds remaining)"
               (ceiling (float-time (time-subtract deadline (current-time))))))
    (if (and (file-exists-p txt-file)
             (> (file-attribute-size (file-attributes txt-file)) 0))
        txt-file
      nil)))

(defun test-recording-monitor--cleanup (recording-file)
  "Clean up RECORDING-FILE and associated transcription files."
  (when (and recording-file (file-exists-p recording-file))
    (let* ((base (file-name-sans-extension recording-file))
           (txt-file (concat base ".txt"))
           (log-file (concat base ".log")))
      (when (file-exists-p recording-file)
        (delete-file recording-file))
      (when (file-exists-p txt-file)
        (delete-file txt-file))
      (when (file-exists-p log-file)
        (delete-file log-file)))))

(ert-deftest test-integration-recording-monitor-capture ()
  "Integration test: verify speaker output (monitor) is captured during recording.

This test simulates a phone call scenario:
- Test audio plays through speakers (simulates remote person speaking)
- Recording should capture this via the monitor device
- Transcription should contain the expected speech

This verifies that the system audio monitor device selection is working correctly.

Components integrated:
- video-audio-recording.el (device selection, ffmpeg command generation)
- PulseAudio/PipeWire (actual audio routing)
- ffmpeg (recording process)
- transcription-config.el (speech-to-text verification)

Test category: Integration test (requires working audio hardware)"
  :tags '(:integration :manual :audio)

  ;; Skip if prerequisites not met
  (skip-unless (executable-find "paplay"))
  (skip-unless (executable-find "ffmpeg"))
  (skip-unless (file-exists-p test-recording-monitor--test-audio-file))
  (skip-unless (and cj/recording-mic-device cj/recording-system-device))

  (let ((recording-file nil)
        (playback-process nil))
    (unwind-protect
        (progn
          (message "\n========================================")
          (message "Starting monitor capture integration test")
          (message "========================================")
          (message "Test audio: %s" test-recording-monitor--test-audio-file)
          (message "Mic device: %s" cj/recording-mic-device)
          (message "Monitor device: %s" cj/recording-system-device)
          (message "Backend: %s" cj/transcribe-backend)

          ;; Step 1: Start recording
          (message "\n[1/6] Starting recording...")
          (let ((temp-dir (make-temp-file "recording-test-" t)))
            (cj/ffmpeg-record-audio temp-dir)
            ;; Give ffmpeg a moment to initialize
            (sleep-for 1)
            (should (process-live-p cj/audio-recording-ffmpeg-process))
            (message "✓ Recording started (process: %s)"
                     (process-name cj/audio-recording-ffmpeg-process))

            ;; Determine the recording filename (ffmpeg uses timestamp)
            ;; We'll find it after stopping
            (setq recording-file
                  (expand-file-name
                   (concat (format-time-string "%Y-%m-%d-%H-%M-%S") ".m4a")
                   temp-dir)))

          ;; Step 2: Play test audio through speakers
          (message "\n[2/6] Playing test audio through speakers...")
          (setq playback-process
                (start-process "test-audio-playback"
                               "*test-audio-playback*"
                               "paplay"
                               test-recording-monitor--test-audio-file))
          (message "✓ Playback started (process: %s)"
                   (process-name playback-process))

          ;; Step 3: Wait for playback to complete
          (message "\n[3/6] Waiting for playback to complete...")
          (let ((max-wait 10)
                (elapsed 0))
            (while (and (process-live-p playback-process)
                        (< elapsed max-wait))
              (sleep-for 0.5)
              (setq elapsed (+ elapsed 0.5)))
            (if (process-live-p playback-process)
                (progn
                  (kill-process playback-process)
                  (error "Playback did not complete within %d seconds" max-wait))
              (message "✓ Playback completed (%.1f seconds)" elapsed)))

          ;; Give audio a moment to be captured
          (sleep-for 1)

          ;; Step 4: Stop recording
          (message "\n[4/6] Stopping recording...")
          (cj/audio-recording-stop)
          (sleep-for 1)
          (should-not (process-live-p cj/audio-recording-ffmpeg-process))

          ;; Find the actual recording file (may differ by timestamp)
          (let* ((dir (file-name-directory recording-file))
                 (files (directory-files dir t "\\.m4a$")))
            (should (= 1 (length files)))
            (setq recording-file (car files)))

          (should (file-exists-p recording-file))
          (message "✓ Recording stopped")
          (message "  Recording file: %s" recording-file)
          (message "  File size: %d bytes"
                   (file-attribute-size (file-attributes recording-file)))

          ;; Step 5: Transcribe recording
          (message "\n[5/6] Transcribing recording (this may take 30-60 seconds)...")
          (cj/transcribe-audio recording-file)

          ;; Wait for transcription to complete
          (let ((txt-file (test-recording-monitor--wait-for-transcription
                           recording-file 120)))
            (should txt-file)
            (message "✓ Transcription completed")
            (message "  Transcript file: %s" txt-file)

            ;; Step 6: Verify transcription contains expected phrases
            (message "\n[6/6] Verifying transcription content...")
            (let ((transcript (with-temp-buffer
                                (insert-file-contents txt-file)
                                (downcase (buffer-string)))))
              (message "Transcript length: %d characters" (length transcript))
              (message "Transcript preview: %s"
                       (substring transcript 0 (min 100 (length transcript))))

              ;; Verify at least 2 of the expected phrases appear
              (let ((matches 0))
                (dolist (phrase test-recording-monitor--expected-phrases)
                  (when (string-match-p phrase transcript)
                    (setq matches (1+ matches))
                    (message "✓ Found expected phrase: '%s'" phrase)))

                (message "\nMatched %d of %d expected phrases"
                         matches (length test-recording-monitor--expected-phrases))

                (should (>= matches 2))
                (message "\n✓✓✓ TEST PASSED ✓✓✓")
                (message "Monitor device is correctly capturing speaker audio!")))))

      ;; Cleanup
      (message "\nCleaning up...")
      (when (process-live-p playback-process)
        (kill-process playback-process))
      (when (and cj/audio-recording-ffmpeg-process
                 (process-live-p cj/audio-recording-ffmpeg-process))
        (cj/audio-recording-stop))
      (test-recording-monitor--cleanup recording-file)
      (message "✓ Cleanup complete"))))

(provide 'test-integration-recording-monitor-capture)
;;; test-integration-recording-monitor-capture.el ends here
