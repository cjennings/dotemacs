;;; video-audio-recording-capture.el --- ffmpeg capture engine and process lifecycle -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/S.
;; Load shape: library.
;; Top-level side effects: none (defuns only).
;; Runtime requires: subr-x, system-lib, video-audio-recording-devices.
;; Direct test load: yes (requires video-audio-recording-devices explicitly).
;;
;; Capture engine for video-audio-recording: ffmpeg / wf-recorder command
;; construction, the recording process lifecycle (sentinel, graceful
;; producer-first shutdown, exit polling), the modeline indicator,
;; dependency checks, device acquisition and validation, and the start/stop
;; entry points.  Builds on the devices layer for discovery and selection.
;;
;; Configuration and the recording process-handle variables are owned by
;; the top video-audio-recording module; they are forward-declared here so
;; the engine reads and updates them without a back-require onto the top
;; module.

;;; Code:

(require 'subr-x)
(require 'system-lib)  ;; provides cj/log-silently
(require 'video-audio-recording-devices)

;; Configuration and process-handle state owned by the top module;
;; declared special here so the engine reads and updates them without a
;; back-require onto video-audio-recording.el.
(defvar cj/recording-mic-boost)
(defvar cj/recording-system-volume)
(defvar cj/recording-mic-device)
(defvar cj/recording-system-device)
(defvar cj/video-recording-ffmpeg-process)
(defvar cj/audio-recording-ffmpeg-process)

;;; Modeline Indicator

(defun cj/recording-modeline-indicator ()
  "Return modeline string showing active recordings.
Shows 🎤 (microphone) for audio, 🎬 (clapper board) for video.
Checks if process is actually alive, not just if variable is set."
  (let ((audio-active (and cj/audio-recording-ffmpeg-process
                          (process-live-p cj/audio-recording-ffmpeg-process)))
        (video-active (and cj/video-recording-ffmpeg-process
                          (process-live-p cj/video-recording-ffmpeg-process))))
    (cond
     ((and audio-active video-active) " 🎤🎬 ")
     (audio-active " 🎤 ")
     (video-active " 🎬 ")
     (t ""))))

;;; Process Lifecycle (Sentinel and Graceful Shutdown)

(defun cj/recording-process-sentinel (process event)
  "Sentinel for recording processes — handles unexpected exits.
PROCESS is the ffmpeg shell process, EVENT describes what happened.
This is called by Emacs when the process changes state (exits, is
killed, etc.).  It clears the process variable and updates the modeline
so the recording indicator disappears even if the recording crashes or
is killed externally."
  (when (memq (process-status process) '(exit signal))
    (cond
     ((eq process cj/audio-recording-ffmpeg-process)
      (setq cj/audio-recording-ffmpeg-process nil)
      (message "Audio recording stopped: %s" (string-trim event)))
     ((eq process cj/video-recording-ffmpeg-process)
      (setq cj/video-recording-ffmpeg-process nil)
      (let ((start (process-get process 'cj-start-time)))
        (if (and start
                 (not (process-get process 'cj-stopping))
                 (cj/recording--start-failed-p (- (float-time) start)
                                               cj/recording-start-fail-threshold))
            ;; Died almost immediately and the user didn't stop it: wf-recorder
            ;; couldn't grab the screen. Say so instead of silently clearing,
            ;; so Craig isn't left blind-retrying (which bursts fragment files).
            (message "Video recording failed to start (wf-recorder couldn't grab the screen). Try again.")
          (message "Video recording stopped: %s" (string-trim event))))))
    (force-mode-line-update t)))

(defun cj/recording--wait-for-exit (process timeout-secs)
  "Wait for PROCESS to exit, polling until done or TIMEOUT-SECS elapsed.
Returns t if the process exited within the timeout, nil if it timed out.

This replaces fixed `sit-for' delays with an actual check that ffmpeg has
finished writing its output file. Container finalization (writing index
tables, flushing buffers) can take several seconds for large recordings,
so a fixed 0.5s wait was causing zero-byte output files."
  (let ((deadline (+ (float-time) timeout-secs)))
    (while (and (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.1))
    (not (process-live-p process))))

(defvar cj/recording-wf-recorder-wait-timeout 2.0
  "Seconds to wait for a dying wf-recorder to release the compositor capture.
Bounds the start-path poll in `cj/ffmpeg-record-video' so it never hangs.")

(defvar cj/recording-start-fail-threshold 1.5
  "Seconds below which a video recording that exits is treated as a failed start.
A wf-recorder that can't grab the screen dies almost immediately (~0.5s); a real
recording runs far longer, so an exit sooner than this is a start failure, not a
normal stop.")

(defun cj/recording--wf-recorder-running-p ()
  "Return non-nil if any wf-recorder process is currently running."
  (eq 0 (call-process "pgrep" nil nil nil "-x" "wf-recorder")))

(defun cj/recording--wait-for-no-wf-recorder (timeout-secs &optional running-p)
  "Poll until no wf-recorder remains, or TIMEOUT-SECS elapse.
Returns t if wf-recorder cleared within the timeout, nil on timeout.  RUNNING-P
is the predicate checked each poll (default `cj/recording--wf-recorder-running-p');
tests inject a fake.  This replaces a fixed `sit-for' after the start-path
`pkill -INT wf-recorder': the kill signals the old recorder to finalize and
exit, but releasing the compositor capture takes longer than a fixed wait, so
launching too soon loses the grab and produces a ~0.5s fragment file."
  (let ((check (or running-p #'cj/recording--wf-recorder-running-p))
        (deadline (+ (float-time) timeout-secs)))
    (while (and (funcall check) (< (float-time) deadline))
      (sleep-for 0.05))
    (not (funcall check))))

(defun cj/recording--start-failed-p (elapsed threshold)
  "Return non-nil when ELAPSED seconds since start is below THRESHOLD.
A failed wf-recorder start exits almost immediately; a real recording does not."
  (< elapsed threshold))

;;; Dependency Checks

(defun cj/recording-check-ffmpeg ()
  "Check if ffmpeg is available.  Error if not found."
  (unless (executable-find "ffmpeg")
    (user-error "Ffmpeg not found.  Install with: sudo pacman -S ffmpeg")
    nil)
  t)

(defun cj/recording--wayland-p ()
  "Return non-nil if running under Wayland."
  (string= (getenv "XDG_SESSION_TYPE") "wayland"))

(defun cj/recording--check-wf-recorder ()
  "Check if wf-recorder is available (needed for Wayland video capture)."
  (if (executable-find "wf-recorder")
      t
    (user-error "wf-recorder not found. Install with: sudo pacman -S wf-recorder")
    nil))

;;; Device Acquisition and Validation

(defun cj/recording-quick-setup ()
  "Quick device setup for recording — two-step mic + sink selection.
Step 1: Pick a microphone.  Each mic shows its status:
  [in use]    = an app is actively using this mic
  [ready]     = recently used, still open
  [available] = no app has this mic open
  [muted]     = device is muted in PulseAudio
Step 2: Pick an audio output to capture.  Same status labels, plus
application names for outputs with active streams (e.g. \"Firefox\").
Devices are sorted: in use → ready → available → muted.
The chosen output's .monitor source is set as the system audio device.

This approach is portable across systems — plug in a new mic, run this
command, and it appears in the list.  No hardware-specific configuration
needed."
  (interactive)
  (let* ((mic-entries (cj/recording--label-devices (cj/recording--get-available-mics))))
    (unless mic-entries
      (user-error "No microphones found.  Is a mic connected?"))
    (let ((mic-device (cj/recording--select-from-labeled "Select microphone: " mic-entries))
          (sink-entries (cj/recording--label-sinks (cj/recording--get-available-sinks))))
      (let ((sink-device (cj/recording--select-from-labeled "Select audio output to capture: " sink-entries)))
        (setq cj/recording-mic-device mic-device)
        (setq cj/recording-system-device (concat sink-device ".monitor"))
        (message "Recording ready!\n  Mic: %s\n  System audio: %s.monitor"
                 (car (rassoc mic-device mic-entries))
                 (file-name-nondirectory sink-device))))))

(defun cj/recording-get-devices ()
  "Get audio devices, prompting user if not already configured.
Returns (mic-device . system-device) cons cell.
If devices aren't set, goes straight into quick setup (mic selection)."
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (cj/recording-quick-setup))
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Audio devices not configured.  Run C-; r s (quick setup) or C-; r S (manual select)"))
  (cj/recording--validate-system-audio)
  (cons cj/recording-mic-device cj/recording-system-device))

(defun cj/recording--validate-system-audio ()
  "Validate that the configured system audio device will capture audio.
Checks two things:
1. Does the configured device still exist as a PulseAudio source?
2. Is anything currently playing through the monitored sink?

Auto-fixes stale devices by falling back to the default sink's monitor.
Warns (but doesn't block) if no audio is currently playing.
Respects the user's explicit sink choice from quick-setup."
  (when cj/recording-system-device
    (let* ((sources-output (shell-command-to-string "pactl list sources short 2>/dev/null"))
           (current-default (cj/recording--get-default-sink-monitor))
           (device-exists (cj/recording--source-exists-p
                           cj/recording-system-device sources-output)))
      ;; Check 1: Device no longer exists — auto-update
      (unless device-exists
        (let ((old cj/recording-system-device))
          (setq cj/recording-system-device current-default)
          (message "System audio device updated: %s → %s (old device no longer exists)"
                   old current-default)))
      ;; Check 2: No active audio on the monitored sink — warn
      (let* ((sink-name (if (string-suffix-p ".monitor" cj/recording-system-device)
                            (substring cj/recording-system-device 0 -8)
                          cj/recording-system-device))
             (sinks-output (shell-command-to-string "pactl list sinks short 2>/dev/null"))
             (sink-index (cj/recording--get-sink-index sink-name sinks-output))
             (sink-inputs (shell-command-to-string "pactl list sink-inputs 2>/dev/null"))
             (has-audio (and sink-index
                             (cj/recording--sink-has-active-audio-p sink-index sink-inputs))))
        (unless has-audio
          (message "Warning: No audio connected to %s. Run C-; r s to check devices"
                   sink-name)
          (cj/log-silently
           (concat "No audio connected to %s. "
                   "Run C-; r s to see active streams and switch devices")
           sink-name))))))

;;; ffmpeg Command Construction

(defun cj/recording--build-video-command (mic-device system-device filename on-wayland)
  "Build the shell command string for video recording.
MIC-DEVICE and SYSTEM-DEVICE are PulseAudio device names.
FILENAME is the output .mkv path.  ON-WAYLAND selects the capture method.

On Wayland: wf-recorder captures screen as H.264 in matroska container,
piped to ffmpeg which adds mic + system audio, then writes the final MKV.

On X11: ffmpeg captures screen directly via x11grab with PulseAudio audio."
  (if on-wayland
      (progn
        (cj/recording--check-wf-recorder)
        ;; wf-recorder stderr is NOT discarded: it flows to the process buffer
        ;; (*ffmpeg-video-recording*) so a failed capture grab is diagnosable
        ;; instead of silent.
        (format (concat "wf-recorder -y -c libx264 -m matroska -f /dev/stdout | "
                        "ffmpeg -i pipe:0 "
                        "-f pulse -i %s "
                        "-f pulse -i %s "
                        "-filter_complex \"[1:a]volume=%.1f[mic];[2:a]volume=%.1f[sys];[mic][sys]amerge=inputs=2[out]\" "
                        "-map 0:v -map \"[out]\" "
                        "-c:v copy "
                        "%s")
                (shell-quote-argument mic-device)
                (shell-quote-argument system-device)
                cj/recording-mic-boost
                cj/recording-system-volume
                (shell-quote-argument filename)))
    (format (concat "ffmpeg -framerate 30 -f x11grab -i :0.0+ "
                    "-f pulse -i %s "
                    "-ac 1 "
                    "-f pulse -i %s "
                    "-ac 2 "
                    "-filter_complex \"[1:a]volume=%.1f[mic];[2:a]volume=%.1f[sys];[mic][sys]amerge=inputs=2[out]\" "
                    "-map 0:v -map \"[out]\" "
                    "%s")
            (shell-quote-argument mic-device)
            (shell-quote-argument system-device)
            cj/recording-mic-boost
            cj/recording-system-volume
            (shell-quote-argument filename))))

(defun cj/recording--build-audio-command (mic-device system-device filename)
  "Build the ffmpeg shell command string for audio-only recording.
MIC-DEVICE and SYSTEM-DEVICE are PulseAudio device names.  FILENAME is
the output .flac path.  Mixes mic + system monitor into a single lossless
FLAC file.  FLAC is used instead of AAC/M4A for two reasons: it is
lossless (no encoder-quality loss, which matters for transcription
accuracy), and its frames are self-contained so an abruptly stopped
recording is still decodable -- unlike MP4, which needs a moov trailer
written at close."
  (format (concat "ffmpeg "
                  "-f pulse -i %s "   ; Input 0: microphone
                  "-f pulse -i %s "   ; Input 1: system audio monitor
                  "-filter_complex \""
                  "[0:a]volume=%.1f[mic];"
                  "[1:a]volume=%.1f[sys];"
                  "[mic][sys]amix=inputs=2:duration=longest[out]\" "
                  "-map \"[out]\" "
                  "-c:a flac "
                  "%s")
          (shell-quote-argument mic-device)
          (shell-quote-argument system-device)
          cj/recording-mic-boost
          cj/recording-system-volume
          (shell-quote-argument filename)))

;;; Start Recording

(defun cj/ffmpeg-record-video (directory)
  "Start a video recording, saving output to DIRECTORY.
Uses wf-recorder on Wayland, x11grab on X11."
  (cj/recording-check-ffmpeg)
  (unless cj/video-recording-ffmpeg-process
    ;; On Wayland, kill any orphan wf-recorder processes left over from
    ;; previous crashes. Without this, old wf-recorders hold the compositor
    ;; capture and new ones fail silently. This one stays a broad by-name
    ;; kill on purpose: the orphans' launching shells are already dead, so
    ;; there is no live PID to scope to. The stop path, by contrast, scopes
    ;; to our own shell's child (see cj/recording--interrupt-child-wf-recorder).
    ;; Wait for the signalled wf-recorder to actually exit and release the
    ;; compositor capture before launching a new one. A fixed `sit-for' here
    ;; raced the dying recorder and left ~0.5s fragment files (same class the
    ;; stop path already fixed with `cj/recording--wait-for-exit').
    (when (cj/recording--wayland-p)
      (call-process "pkill" nil nil nil "-INT" "wf-recorder")
      (cj/recording--wait-for-no-wf-recorder cj/recording-wf-recorder-wait-timeout))
    (let* ((devices (cj/recording-get-devices))
           (mic-device (car devices))
           (system-device (cdr devices))
           (location (expand-file-name directory))
           (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (filename (expand-file-name (concat name ".mkv") location))
           (on-wayland (cj/recording--wayland-p))
           (record-command (cj/recording--build-video-command
                            mic-device system-device filename on-wayland)))
      (setq cj/video-recording-ffmpeg-process
            (start-process-shell-command "ffmpeg-video-recording"
                                         "*ffmpeg-video-recording*"
                                         record-command))
      (set-process-query-on-exit-flag cj/video-recording-ffmpeg-process nil)
      (set-process-sentinel cj/video-recording-ffmpeg-process #'cj/recording-process-sentinel)
      ;; Stamp the start time so the sentinel can tell a ~0.5s failed start
      ;; (wf-recorder couldn't grab the screen) from a normal recording.
      (process-put cj/video-recording-ffmpeg-process 'cj-start-time (float-time))
      (force-mode-line-update t)
      (message "Started video recording to %s (%s, mic: %.1fx, system: %.1fx)."
               filename
               (if on-wayland "Wayland/wf-recorder" "X11")
               cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/ffmpeg-record-audio (directory)
  "Start an audio recording, saving output to DIRECTORY.
Records from microphone and system audio monitor (configured device),
mixing them together into a single lossless FLAC file.

The filter graph mixes two PulseAudio inputs:
  [mic] → volume boost → amix → FLAC encoder → .flac
  [sys] → volume boost ↗"
  (cj/recording-check-ffmpeg)
  (unless cj/audio-recording-ffmpeg-process
    (let* ((devices (cj/recording-get-devices))
           (mic-device (car devices))
           (system-device (cdr devices))
           (location (expand-file-name directory))
           (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (filename (expand-file-name (concat name ".flac") location))
           (ffmpeg-command
            (cj/recording--build-audio-command mic-device system-device filename)))
      (message "Recording from mic: %s + ALL system outputs" mic-device)
      (cj/log-silently "Audio recording ffmpeg command: %s" ffmpeg-command)
      (setq cj/audio-recording-ffmpeg-process
            (start-process-shell-command "ffmpeg-audio-recording"
                                         "*ffmpeg-audio-recording*"
                                         ffmpeg-command))
      (set-process-query-on-exit-flag cj/audio-recording-ffmpeg-process nil)
      (set-process-sentinel cj/audio-recording-ffmpeg-process #'cj/recording-process-sentinel)
      (force-mode-line-update t)
      (message "Started recording to %s (mic: %.1fx, all system audio: %.1fx)"
               filename cj/recording-mic-boost cj/recording-system-volume))))

;;; Stop Recording

(defun cj/recording--interrupt-child-wf-recorder (shell-pid)
  "Send SIGINT to the wf-recorder child of SHELL-PID, if any.
Scopes the producer-first stop to the wf-recorder this module launched
\(a child of our recording shell) via `pkill -P', instead of killing
every wf-recorder on the system by name.  Does nothing when SHELL-PID
is nil (the shell already exited, so there is no child to signal)."
  (when shell-pid
    (call-process "pkill" nil nil nil
                  "-INT" "-P" (number-to-string shell-pid) "wf-recorder")))

(defun cj/video-recording-stop ()
  "Stop the video recording, waiting for ffmpeg to finalize the file.
On Wayland, kills wf-recorder first so ffmpeg gets a clean EOF on its
video input pipe, then signals the process group. Waits up to 5 seconds
for ffmpeg to write container metadata before giving up."
  (interactive)
  (if (not cj/video-recording-ffmpeg-process)
      (message "No video recording in progress.")
    (let ((proc cj/video-recording-ffmpeg-process))
      ;; Mark this as a user stop so the sentinel's fail-fast check doesn't
      ;; misread a quick intentional stop as a failed start.
      (process-put proc 'cj-stopping t)
      ;; On Wayland, kill the producer (wf-recorder) FIRST so ffmpeg sees
      ;; a clean EOF on pipe:0. This triggers ffmpeg's orderly shutdown:
      ;; drain remaining frames, write container metadata, close file.
      ;; Without this, simultaneous SIGINT to both causes ffmpeg to abort
      ;; without creating a file.
      (when (cj/recording--wayland-p)
        (cj/recording--interrupt-child-wf-recorder (process-id proc))
        (sit-for 0.3))  ; Brief pause for pipe to close
      ;; Now send SIGINT to the process group. On Wayland, this reaches
      ;; ffmpeg (which is already shutting down from the pipe EOF) and
      ;; reinforces the stop. On X11, this is the primary shutdown signal.
      (let ((pid (process-id proc)))
        (when pid
          (signal-process (- pid) 2)))  ; 2 = SIGINT
      ;; Wait for ffmpeg to finalize the container. MKV files need index
      ;; tables written at the end — without this wait, the file is truncated.
      (let ((exited (cj/recording--wait-for-exit proc 5)))
        (unless exited
          (message "Warning: recording process did not exit within 5 seconds")))
      ;; Safety net: signal our own straggler wf-recorder on Wayland.
      ;; If the shell already exited, process-id returns nil and this is
      ;; a no-op (the child is already gone with it).
      (when (cj/recording--wayland-p)
        (cj/recording--interrupt-child-wf-recorder (process-id proc)))
      ;; The sentinel handles clearing cj/video-recording-ffmpeg-process
      ;; and updating the modeline. If the process already exited during
      ;; our wait, the sentinel has already fired. If not, force cleanup.
      (when (eq cj/video-recording-ffmpeg-process proc)
        (setq cj/video-recording-ffmpeg-process nil)
        (force-mode-line-update t))
      (message "Stopped video recording."))))

(defun cj/audio-recording-stop ()
  "Stop the audio recording, waiting for ffmpeg to finalize the file.
Sends SIGINT to the process group and waits up to 3 seconds for ffmpeg
to flush audio frames and finalize the FLAC stream."
  (interactive)
  (if (not cj/audio-recording-ffmpeg-process)
      (message "No audio recording in progress.")
    (let ((proc cj/audio-recording-ffmpeg-process))
      ;; Send SIGINT to the process group (see video-recording-stop for details)
      (let ((pid (process-id proc)))
        (when pid
          (signal-process (- pid) 2)))
      ;; On a clean stop ffmpeg seeks back and backfills the FLAC STREAMINFO
      ;; (total samples, MD5) so duration reads correctly.  Even a hard
      ;; truncation leaves a decodable file, since FLAC frames are
      ;; self-contained -- there is no end-of-file trailer to miss.
      (let ((exited (cj/recording--wait-for-exit proc 3)))
        (unless exited
          (message "Warning: recording process did not exit within 3 seconds")))
      ;; Fallback cleanup if sentinel hasn't fired yet
      (when (eq cj/audio-recording-ffmpeg-process proc)
        (setq cj/audio-recording-ffmpeg-process nil)
        (force-mode-line-update t))
      (message "Stopped audio recording."))))

(provide 'video-audio-recording-capture)
;;; video-audio-recording-capture.el ends here
