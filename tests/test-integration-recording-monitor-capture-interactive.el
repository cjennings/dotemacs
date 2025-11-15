;;; test-integration-recording-monitor-capture-interactive.el --- Interactive recording test -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-14

;;; Commentary:
;;
;; **INTERACTIVE TEST - Run from within Emacs**
;;
;; This test must be run from an interactive Emacs session where recording
;; devices are already configured (C-; r c).
;;
;; USAGE:
;;   1. Ensure devices are configured: C-; r c
;;   2. Load this file: M-x load-file RET tests/test-integration-recording-monitor-capture-interactive.el RET
;;   3. Run test: M-x test-recording-monitor-now RET
;;
;; OR simply:
;;   M-x ert RET test-integration-recording-monitor-capture RET
;;
;; The test will:
;; - Play test audio through your speakers (5 seconds)
;; - Record it
;; - Transcribe it
;; - Verify the transcription contains the expected text
;;
;; This verifies that phone call audio (speaker output) is being captured correctly.

;;; Code:

(require 'video-audio-recording)
(require 'transcription-config)

(defvar test-recording--test-audio
  (expand-file-name "tests/fixtures/audio/speaker-output-test.wav" user-emacs-directory)
  "Test audio file for speaker output testing.")

(defvar test-recording--expected-phrases
  '("hear me" "testing" "one")
  "Expected phrases in transcription (partial match OK).
Based on actual recording: 'Can you hear me? Testing, one, two, three.'")

(defun test-recording--cleanup-files (recording-file)
  "Clean up RECORDING-FILE and associated files."
  (when (and recording-file (file-exists-p recording-file))
    (let* ((base (file-name-sans-extension recording-file))
           (txt-file (concat base ".txt"))
           (log-file (concat base ".log")))
      (when (file-exists-p recording-file) (delete-file recording-file))
      (when (file-exists-p txt-file) (delete-file txt-file))
      (when (file-exists-p log-file) (delete-file log-file)))))

(defun test-recording--wait-for-file (file timeout)
  "Wait for FILE to exist and have content, up to TIMEOUT seconds.
Returns FILE path if successful, nil if timeout."
  (let ((deadline (time-add (current-time) (seconds-to-time timeout))))
    (while (and (time-less-p (current-time) deadline)
                (or (not (file-exists-p file))
                    (= 0 (file-attribute-size (file-attributes file)))))
      (sleep-for 1)
      (message "Waiting for %s... (%d sec remaining)"
               (file-name-nondirectory file)
               (ceiling (float-time (time-subtract deadline (current-time))))))
    (when (and (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      file)))

;;;###autoload
(defun test-recording-monitor-now ()
  "Test recording monitor capture interactively.
This function can be called with M-x to test recording without ERT framework."
  (interactive)

  ;; Pre-flight checks
  (unless (executable-find "paplay")
    (user-error "paplay not found. Install pulseaudio-utils"))
  (unless (executable-find "ffmpeg")
    (user-error "ffmpeg not found. Install ffmpeg"))
  (unless (file-exists-p test-recording--test-audio)
    (user-error "Test audio file not found: %s" test-recording--test-audio))
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Recording devices not configured. Run C-; r c first"))

  (let ((test-dir (make-temp-file "recording-test-" t))
        (recording-file nil)
        (playback-proc nil))
    (unwind-protect
        (progn
          (message "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
          (message "RECORDING MONITOR CAPTURE TEST")
          (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
          (message "Configuration:")
          (message "  Mic: %s" cj/recording-mic-device)
          (message "  Monitor: %s" cj/recording-system-device)
          (message "  Backend: %s\n" cj/transcribe-backend)

          ;; Step 1: Start recording
          (message "[1/6] Starting recording...")
          (cj/ffmpeg-record-audio test-dir)
          (sleep-for 1)
          (unless (process-live-p cj/audio-recording-ffmpeg-process)
            (error "Failed to start recording"))
          (message "✓ Recording started\n")

          ;; Step 2: Play test audio
          (message "[2/6] Playing test audio through speakers...")
          (setq playback-proc (start-process "test-playback" "*test-playback*"
                                             "paplay" test-recording--test-audio))
          (message "✓ Playback started\n")

          ;; Step 3: Wait for playback
          (message "[3/6] Waiting for playback to complete...")
          (let ((waited 0))
            (while (and (process-live-p playback-proc) (< waited 10))
              (sleep-for 0.5)
              (setq waited (+ waited 0.5)))
            (when (process-live-p playback-proc)
              (kill-process playback-proc)
              (error "Playback timed out")))
          (sleep-for 1)
          (message "✓ Playback completed\n")

          ;; Step 4: Stop recording
          (message "[4/6] Stopping recording...")
          (cj/audio-recording-stop)
          (sleep-for 1)

          ;; Find recording file
          (let ((files (directory-files test-dir t "\\.m4a$")))
            (unless (= 1 (length files))
              (error "Expected 1 recording file, found %d" (length files)))
            (setq recording-file (car files)))

          (message "✓ Recording stopped")
          (message "  File: %s" recording-file)
          (message "  Size: %d bytes\n"
                   (file-attribute-size (file-attributes recording-file)))

          ;; Step 5: Transcribe
          (message "[5/6] Transcribing (this may take 30-60 seconds)...")
          (cj/transcribe-audio recording-file)

          (let ((txt-file (concat (file-name-sans-extension recording-file) ".txt")))
            (unless (test-recording--wait-for-file txt-file 120)
              (error "Transcription timed out or failed"))
            (message "✓ Transcription completed\n")

            ;; Step 6: Verify
            (message "[6/6] Verifying transcription...")
            (let ((transcript (with-temp-buffer
                                (insert-file-contents txt-file)
                                (downcase (buffer-string))))
                  (matches 0))
              (message "Transcript (%d chars): %s..."
                       (length transcript)
                       (substring transcript 0 (min 80 (length transcript))))

              (dolist (phrase test-recording--expected-phrases)
                (when (string-match-p phrase transcript)
                  (setq matches (1+ matches))
                  (message "  ✓ Found: '%s'" phrase)))

              (message "\nMatched %d/%d expected phrases"
                       matches (length test-recording--expected-phrases))

              (if (>= matches 2)
                  (progn
                    (message "\n✓✓✓ TEST PASSED ✓✓✓")
                    (message "Monitor is correctly capturing speaker audio!"))
                (error "TEST FAILED: Only matched %d/%d phrases"
                       matches (length test-recording--expected-phrases)))))))

      ;; Cleanup
      (when (and playback-proc (process-live-p playback-proc))
        (kill-process playback-proc))
      (when (and cj/audio-recording-ffmpeg-process
                 (process-live-p cj/audio-recording-ffmpeg-process))
        (cj/audio-recording-stop))
      (when recording-file
        (test-recording--cleanup-files recording-file))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t))
      (message "\nCleanup complete."))))

(provide 'test-integration-recording-monitor-capture-interactive)
;;; test-integration-recording-monitor-capture-interactive.el ends here
