;;; test-integration-recording-toggle-workflow.el --- Integration tests for recording toggle workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests covering the complete recording toggle workflow from
;; user action through device setup, recording, and cleanup.
;;
;; This tests the ACTUAL user workflow: Press C-; r a → setup → record → stop → cleanup
;;
;; Components integrated:
;; - cj/audio-recording-toggle (entry point)
;; - cj/video-recording-toggle (entry point)
;; - cj/recording-get-devices (device prompting and setup)
;; - cj/recording-quick-setup-for-calls (device selection workflow)
;; - cj/ffmpeg-record-audio (process creation and ffmpeg command)
;; - cj/ffmpeg-record-video (process creation and ffmpeg command)
;; - cj/recording-modeline-indicator (UI state display)
;; - cj/audio-recording-stop (cleanup)
;; - cj/video-recording-stop (cleanup)
;; - cj/recording-process-sentinel (auto-cleanup on process death)
;;
;; Validates:
;; - Complete workflow from toggle to cleanup
;; - Device setup on first use
;; - Process creation and management
;; - Modeline updates at each step
;; - Cleanup on user stop
;; - Auto-cleanup when process dies

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub directory variables
(defvar video-recordings-dir "/tmp/video-recordings/")
(defvar audio-recordings-dir "/tmp/audio-recordings/")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-integration-toggle-setup ()
  "Reset all variables before each test."
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

(defun test-integration-toggle-teardown ()
  "Clean up after each test."
  (when cj/video-recording-ffmpeg-process
    (ignore-errors (delete-process cj/video-recording-ffmpeg-process)))
  (when cj/audio-recording-ffmpeg-process
    (ignore-errors (delete-process cj/audio-recording-ffmpeg-process)))
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Integration Tests - Audio Recording Workflow

(ert-deftest test-integration-recording-toggle-workflow-audio-first-use-full-cycle ()
  "Test complete audio recording workflow from first use through cleanup.

When user presses C-; r a for the first time:
1. Device setup prompt appears (no devices configured)
2. User chooses quick setup
3. Devices are selected and saved
4. Recording starts with correct ffmpeg command
5. Process is created and sentinel attached
6. Modeline shows recording indicator
7. User presses C-; r a again to stop
8. Recording stops gracefully
9. Modeline indicator clears

Components integrated:
- cj/audio-recording-toggle (toggles start/stop)
- cj/recording-get-devices (prompts for setup on first use)
- cj/recording-quick-setup-for-calls (device selection)
- cj/ffmpeg-record-audio (creates recording process)
- cj/recording-modeline-indicator (UI state)
- cj/audio-recording-stop (cleanup)

Validates:
- Full user workflow from first use to stop
- Device setup on first toggle
- Recording starts after setup
- Modeline updates correctly
- Stop works after recording"
  (test-integration-toggle-setup)
  (unwind-protect
      (let ((setup-called nil)
            (ffmpeg-cmd nil)
            (process-created nil))
        ;; Mock the device setup to simulate user choosing quick setup
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t))  ; User says yes to quick setup
                  ((symbol-function 'cj/recording-quick-setup-for-calls)
                   (lambda ()
                     (setq setup-called t)
                     (setq cj/recording-mic-device "test-mic")
                     (setq cj/recording-system-device "test-monitor")))
                  ((symbol-function 'file-directory-p)
                   (lambda (_dir) t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq process-created t)
                     (setq ffmpeg-cmd cmd)
                     (make-process :name "fake-audio" :command '("sleep" "1000")))))

          ;; STEP 1: First toggle - should trigger device setup
          (cj/audio-recording-toggle nil)

          ;; Verify setup was called
          (should setup-called)

          ;; Verify devices were set
          (should (equal "test-mic" cj/recording-mic-device))
          (should (equal "test-monitor" cj/recording-system-device))

          ;; Verify recording started
          (should process-created)
          (should cj/audio-recording-ffmpeg-process)
          (should (string-match-p "ffmpeg" ffmpeg-cmd))
          (should (string-match-p "test-mic" ffmpeg-cmd))
          (should (string-match-p "test-monitor" ffmpeg-cmd))

          ;; Verify modeline shows recording
          (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

          ;; STEP 2: Second toggle - should stop recording
          (cj/audio-recording-toggle nil)

          ;; Verify recording stopped
          (should (null cj/audio-recording-ffmpeg-process))

          ;; Verify modeline cleared
          (should (equal "" (cj/recording-modeline-indicator)))))
    (test-integration-toggle-teardown)))

(ert-deftest test-integration-recording-toggle-workflow-audio-subsequent-use-no-setup ()
  "Test that subsequent audio recordings skip device setup.

After devices are configured, pressing C-; r a should:
1. Skip device setup (already configured)
2. Start recording immediately
3. Use previously configured devices

Components integrated:
- cj/audio-recording-toggle
- cj/recording-get-devices (returns cached devices)
- cj/ffmpeg-record-audio (uses cached devices)

Validates:
- Device setup is cached across recordings
- Second recording doesn't prompt
- Same devices are used"
  (test-integration-toggle-setup)
  (unwind-protect
      (progn
        ;; Pre-configure devices (simulating previous setup)
        (setq cj/recording-mic-device "cached-mic")
        (setq cj/recording-system-device "cached-monitor")

        (let ((setup-called nil)
              (ffmpeg-cmd nil))
          (cl-letf (((symbol-function 'cj/recording-quick-setup-for-calls)
                     (lambda () (setq setup-called t)))
                    ((symbol-function 'file-directory-p)
                     (lambda (_dir) t))
                    ((symbol-function 'start-process-shell-command)
                     (lambda (_name _buffer cmd)
                       (setq ffmpeg-cmd cmd)
                       (make-process :name "fake-audio" :command '("sleep" "1000")))))

            ;; Toggle to start recording
            (cj/audio-recording-toggle nil)

            ;; Setup should NOT be called
            (should-not setup-called)

            ;; Should use cached devices
            (should (string-match-p "cached-mic" ffmpeg-cmd))
            (should (string-match-p "cached-monitor" ffmpeg-cmd)))))
    (test-integration-toggle-teardown)))

;;; Integration Tests - Video Recording Workflow

(ert-deftest test-integration-recording-toggle-workflow-video-full-cycle ()
  "Test complete video recording workflow.

Components integrated:
- cj/video-recording-toggle
- cj/recording-get-devices
- cj/ffmpeg-record-video
- cj/recording-modeline-indicator
- cj/video-recording-stop

Validates:
- Video recording follows same workflow as audio
- Modeline shows video indicator
- Toggle works for video"
  (test-integration-toggle-setup)
  (unwind-protect
      (let ((setup-called nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'cj/recording-quick-setup-for-calls)
                   (lambda ()
                     (setq setup-called t)
                     (setq cj/recording-mic-device "test-mic")
                     (setq cj/recording-system-device "test-monitor")))
                  ((symbol-function 'file-directory-p)
                   (lambda (_dir) t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000")))))

          ;; Start video recording
          (cj/video-recording-toggle nil)

          ;; Verify setup and recording
          (should setup-called)
          (should cj/video-recording-ffmpeg-process)
          (should (equal " 🔴Video " (cj/recording-modeline-indicator)))

          ;; Stop recording
          (cj/video-recording-toggle nil)

          ;; Verify cleanup
          (should (null cj/video-recording-ffmpeg-process))
          (should (equal "" (cj/recording-modeline-indicator)))))
    (test-integration-toggle-teardown)))

;;; Integration Tests - Both Recordings Simultaneously

(ert-deftest test-integration-recording-toggle-workflow-both-simultaneous ()
  "Test that both audio and video can record simultaneously.

Components integrated:
- cj/audio-recording-toggle
- cj/video-recording-toggle
- cj/recording-modeline-indicator (shows both)
- Both ffmpeg-record functions

Validates:
- Audio and video can run together
- Modeline shows both indicators
- Stopping one doesn't affect the other"
  (test-integration-toggle-setup)
  (unwind-protect
      (progn
        ;; Pre-configure devices
        (setq cj/recording-mic-device "test-mic")
        (setq cj/recording-system-device "test-monitor")

        (cl-letf (((symbol-function 'file-directory-p)
                   (lambda (_dir) t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (name _buffer _cmd)
                     (make-process :name name :command '("sleep" "1000")))))

          ;; Start both recordings
          (cj/audio-recording-toggle nil)
          (cj/video-recording-toggle nil)

          ;; Verify both are recording
          (should cj/audio-recording-ffmpeg-process)
          (should cj/video-recording-ffmpeg-process)
          (should (equal " 🔴A+V " (cj/recording-modeline-indicator)))

          ;; Stop audio only
          (cj/audio-recording-toggle nil)

          ;; Verify only video still recording
          (should (null cj/audio-recording-ffmpeg-process))
          (should cj/video-recording-ffmpeg-process)
          (should (equal " 🔴Video " (cj/recording-modeline-indicator)))

          ;; Stop video
          (cj/video-recording-toggle nil)

          ;; Verify all cleared
          (should (null cj/video-recording-ffmpeg-process))
          (should (equal "" (cj/recording-modeline-indicator)))))
    (test-integration-toggle-teardown)))

;;; Integration Tests - Sentinel Auto-Cleanup

(ert-deftest test-integration-recording-toggle-workflow-sentinel-auto-cleanup ()
  "Test that sentinel auto-cleans when recording process dies unexpectedly.

When the ffmpeg process crashes or exits unexpectedly:
1. Sentinel detects process death
2. Variable is automatically cleared
3. Modeline updates to show no recording
4. User can start new recording

Components integrated:
- cj/audio-recording-toggle (process creation)
- cj/ffmpeg-record-audio (attaches sentinel)
- cj/recording-process-sentinel (cleanup on death)
- cj/recording-modeline-indicator (updates on cleanup)

Validates:
- Sentinel cleans up on unexpected process death
- Modeline syncs when sentinel runs
- User can toggle again after crash"
  (test-integration-toggle-setup)
  (unwind-protect
      (progn
        ;; Pre-configure devices
        (setq cj/recording-mic-device "test-mic")
        (setq cj/recording-system-device "test-monitor")

        (let ((process nil))
          (cl-letf (((symbol-function 'file-directory-p)
                     (lambda (_dir) t))
                    ((symbol-function 'start-process-shell-command)
                     (lambda (name _buffer _cmd)
                       (setq process (make-process :name name :command '("sh" "-c" "exit 1"))))))

            ;; Start recording
            (cj/audio-recording-toggle nil)

            ;; Verify recording started
            (should cj/audio-recording-ffmpeg-process)
            (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

            ;; Wait for process to exit (sentinel should run)
            (sit-for 0.3)

            ;; Verify sentinel cleaned up
            (should (null cj/audio-recording-ffmpeg-process))
            (should (equal "" (cj/recording-modeline-indicator)))

            ;; Verify user can start new recording after crash
            (cj/audio-recording-toggle nil)
            (should cj/audio-recording-ffmpeg-process))))
    (test-integration-toggle-teardown)))

(provide 'test-integration-recording-toggle-workflow)
;;; test-integration-recording-toggle-workflow.el ends here
