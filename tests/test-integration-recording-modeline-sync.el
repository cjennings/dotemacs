;;; test-integration-recording-modeline-sync.el --- Integration tests for modeline sync -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests validating that the modeline indicator NEVER desyncs
;; from the actual recording state throughout the entire toggle lifecycle.
;;
;; This tests the critical requirement: modeline must always accurately
;; reflect whether recording is happening, with NO desyncs.
;;
;; Components integrated:
;; - cj/audio-recording-toggle (state changes)
;; - cj/video-recording-toggle (state changes)
;; - cj/recording-modeline-indicator (UI state display)
;; - cj/ffmpeg-record-audio (process creation)
;; - cj/ffmpeg-record-video (process creation)
;; - cj/recording-process-sentinel (auto-updates modeline)
;; - cj/audio-recording-stop (cleanup triggers update)
;; - cj/video-recording-stop (cleanup triggers update)
;; - force-mode-line-update (explicit refresh calls)
;;
;; Validates:
;; - Modeline updates immediately on toggle start
;; - Modeline updates immediately on toggle stop
;; - Modeline updates when sentinel runs (process dies)
;; - Modeline shows correct state for audio, video, or both
;; - Modeline never shows stale state
;; - process-live-p check prevents desync on dead processes

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

(defun test-integration-modeline-setup ()
  "Reset all variables before each test."
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device "test-mic")
  (setq cj/recording-system-device "test-monitor"))

(defun test-integration-modeline-teardown ()
  "Clean up after each test."
  (when cj/video-recording-ffmpeg-process
    (ignore-errors (delete-process cj/video-recording-ffmpeg-process)))
  (when cj/audio-recording-ffmpeg-process
    (ignore-errors (delete-process cj/audio-recording-ffmpeg-process)))
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Integration Tests - Modeline Sync on Toggle

(ert-deftest test-integration-recording-modeline-sync-audio-start-updates-immediately ()
  "Test that modeline updates immediately when audio recording starts.

When user toggles audio recording on:
1. Process is created
2. Modeline indicator updates to show 🔴Audio
3. State is in sync immediately (not delayed)

Components integrated:
- cj/audio-recording-toggle
- cj/ffmpeg-record-audio (calls force-mode-line-update)
- cj/recording-modeline-indicator

Validates:
- Modeline syncs on start
- No delay or race condition"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   (make-process :name name :command '("sleep" "1000")))))

        ;; Before toggle: no recording
        (should (equal "" (cj/recording-modeline-indicator)))

        ;; Toggle on
        (cj/audio-recording-toggle nil)

        ;; Immediately after toggle: modeline should show recording
        (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

        ;; Process should be alive
        (should (process-live-p cj/audio-recording-ffmpeg-process)))
    (test-integration-modeline-teardown)))

(ert-deftest test-integration-recording-modeline-sync-audio-stop-updates-immediately ()
  "Test that modeline updates immediately when audio recording stops.

When user toggles audio recording off:
1. Process is interrupted
2. Variable is cleared
3. Modeline indicator updates to show empty
4. State is in sync immediately

Components integrated:
- cj/audio-recording-toggle (stop path)
- cj/audio-recording-stop (calls force-mode-line-update)
- cj/recording-modeline-indicator

Validates:
- Modeline syncs on stop
- No stale indicator after stop"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   (make-process :name name :command '("sleep" "1000")))))

        ;; Start recording
        (cj/audio-recording-toggle nil)
        (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

        ;; Stop recording
        (cj/audio-recording-toggle nil)

        ;; Immediately after stop: modeline should be empty
        (should (equal "" (cj/recording-modeline-indicator)))

        ;; Process should be nil
        (should (null cj/audio-recording-ffmpeg-process)))
    (test-integration-modeline-teardown)))

(ert-deftest test-integration-recording-modeline-sync-video-lifecycle ()
  "Test modeline sync through complete video recording lifecycle.

Components integrated:
- cj/video-recording-toggle (both start and stop)
- cj/ffmpeg-record-video
- cj/video-recording-stop
- cj/recording-modeline-indicator

Validates:
- Video recording follows same sync pattern as audio
- Modeline shows 🔴Video correctly"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   (make-process :name name :command '("sleep" "1000")))))

        ;; Initial state
        (should (equal "" (cj/recording-modeline-indicator)))

        ;; Start video
        (cj/video-recording-toggle nil)
        (should (equal " 🔴Video " (cj/recording-modeline-indicator)))

        ;; Stop video
        (cj/video-recording-toggle nil)
        (should (equal "" (cj/recording-modeline-indicator))))
    (test-integration-modeline-teardown)))

;;; Integration Tests - Modeline Sync with Both Recordings

(ert-deftest test-integration-recording-modeline-sync-both-recordings-transitions ()
  "Test modeline sync through all possible state transitions.

Tests transitions:
- none → audio → both → video → none
- Validates modeline updates at every transition

Components integrated:
- cj/audio-recording-toggle
- cj/video-recording-toggle
- cj/recording-modeline-indicator (handles all states)

Validates:
- Modeline accurately reflects all combinations
- Transitions are clean with no stale state"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   (make-process :name name :command '("sleep" "1000")))))

        ;; State 1: None
        (should (equal "" (cj/recording-modeline-indicator)))

        ;; State 2: Audio only
        (cj/audio-recording-toggle nil)
        (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

        ;; State 3: Both
        (cj/video-recording-toggle nil)
        (should (equal " 🔴A+V " (cj/recording-modeline-indicator)))

        ;; State 4: Video only (stop audio)
        (cj/audio-recording-toggle nil)
        (should (equal " 🔴Video " (cj/recording-modeline-indicator)))

        ;; State 5: None (stop video)
        (cj/video-recording-toggle nil)
        (should (equal "" (cj/recording-modeline-indicator))))
    (test-integration-modeline-teardown)))

;;; Integration Tests - Modeline Sync with Sentinel

(ert-deftest test-integration-recording-modeline-sync-sentinel-updates-on-crash ()
  "Test that modeline syncs when process dies and sentinel runs.

When recording process crashes:
1. Sentinel detects process death
2. Sentinel clears variable
3. Sentinel calls force-mode-line-update
4. Modeline indicator shows no recording

Components integrated:
- cj/ffmpeg-record-audio (attaches sentinel)
- cj/recording-process-sentinel (cleanup + modeline update)
- cj/recording-modeline-indicator

Validates:
- Sentinel updates modeline on process death
- Modeline syncs automatically without user action
- Critical: prevents desync when process crashes"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   ;; Create process that exits immediately
                   (make-process :name name :command '("sh" "-c" "exit 1")))))

        ;; Start recording
        (cj/audio-recording-toggle nil)

        ;; Immediately after start: should show recording
        (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

        ;; Wait for process to exit and sentinel to run
        (sit-for 0.3)

        ;; After sentinel runs: modeline should be clear
        (should (equal "" (cj/recording-modeline-indicator)))

        ;; Variable should be nil
        (should (null cj/audio-recording-ffmpeg-process)))
    (test-integration-modeline-teardown)))

(ert-deftest test-integration-recording-modeline-sync-dead-process-not-shown ()
  "Test that modeline never shows dead process as recording.

The modeline indicator uses process-live-p to check if process is ACTUALLY
alive, not just if the variable is set. This prevents desync.

Components integrated:
- cj/recording-modeline-indicator (uses process-live-p)

Validates:
- Dead process doesn't show as recording
- process-live-p check prevents desync
- Critical: if variable is set but process is dead, shows empty"
  (test-integration-modeline-setup)
  (unwind-protect
      (let ((dead-process (make-process :name "test-audio" :command '("sh" "-c" "exit 0"))))
        ;; Set variable to dead process (simulating race condition)
        (setq cj/audio-recording-ffmpeg-process dead-process)

        ;; Wait for process to die
        (sit-for 0.1)

        ;; Modeline should NOT show recording (process is dead)
        (should (equal "" (cj/recording-modeline-indicator)))

        ;; Even though variable is set
        (should (eq dead-process cj/audio-recording-ffmpeg-process))

        ;; Process is dead
        (should-not (process-live-p dead-process)))
    (test-integration-modeline-teardown)))

;;; Integration Tests - Modeline Sync Under Rapid Toggling

(ert-deftest test-integration-recording-modeline-sync-rapid-toggle-stays-synced ()
  "Test modeline stays synced under rapid start/stop toggling.

When user rapidly toggles recording on and off:
- Modeline should stay in sync at every step
- No race conditions or stale state

Components integrated:
- cj/audio-recording-toggle (rapid calls)
- cj/ffmpeg-record-audio
- cj/audio-recording-stop
- cj/recording-modeline-indicator

Validates:
- Modeline syncs even with rapid state changes
- No race conditions in update logic"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   (make-process :name name :command '("sleep" "1000")))))

        ;; Rapid toggling
        (dotimes (_i 5)
          ;; Start
          (cj/audio-recording-toggle nil)
          (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))
          (should cj/audio-recording-ffmpeg-process)

          ;; Stop
          (cj/audio-recording-toggle nil)
          (should (equal "" (cj/recording-modeline-indicator)))
          (should (null cj/audio-recording-ffmpeg-process))))
    (test-integration-modeline-teardown)))

(ert-deftest test-integration-recording-modeline-sync-both-recordings-independent ()
  "Test that audio and video modeline updates are independent.

When one recording stops, the other's indicator persists.
When one recording starts, both indicators combine correctly.

Components integrated:
- cj/audio-recording-toggle
- cj/video-recording-toggle
- cj/recording-modeline-indicator (combines states)

Validates:
- Independent recordings don't interfere
- Modeline correctly shows: audio-only, video-only, or both
- Stopping one doesn't affect other's indicator"
  (test-integration-modeline-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer _cmd)
                   (make-process :name name :command '("sleep" "1000")))))

        ;; Start audio
        (cj/audio-recording-toggle nil)
        (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

        ;; Add video - modeline should combine
        (cj/video-recording-toggle nil)
        (should (equal " 🔴A+V " (cj/recording-modeline-indicator)))

        ;; Stop audio - video indicator should persist
        (cj/audio-recording-toggle nil)
        (should (equal " 🔴Video " (cj/recording-modeline-indicator)))

        ;; Start audio again - should recombine
        (cj/audio-recording-toggle nil)
        (should (equal " 🔴A+V " (cj/recording-modeline-indicator)))

        ;; Stop video - audio indicator should persist
        (cj/video-recording-toggle nil)
        (should (equal " 🔴Audio " (cj/recording-modeline-indicator)))

        ;; Stop audio - should be empty
        (cj/audio-recording-toggle nil)
        (should (equal "" (cj/recording-modeline-indicator))))
    (test-integration-modeline-teardown)))

(provide 'test-integration-recording-modeline-sync)
;;; test-integration-recording-modeline-sync.el ends here
