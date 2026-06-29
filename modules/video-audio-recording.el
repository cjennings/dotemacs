;;; video-audio-recording.el --- Video and Audio Recording -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/S.
;; Load shape: eager.
;; Eager reason: none; records only on command, but registers C-; r at load.
;; Top-level side effects: defines cj/record-map and registers it when possible.
;; Runtime requires: system-lib, keybindings, video-audio-recording-devices,
;;   video-audio-recording-capture.
;; Direct test load: yes.
;;
;; Starts and stops ffmpeg-backed audio/video recordings from Emacs. Audio
;; captures microphone plus system monitor; video uses x11grab on X11 and
;; wf-recorder piped into ffmpeg on Wayland.
;;
;; This is the public face of the module: it owns configuration and the
;; recording process-handle state, the device-diagnostic and device-test
;; commands, the toggle commands, and the C-; r keymap.  PulseAudio
;; discovery lives in video-audio-recording-devices and the ffmpeg capture
;; engine in video-audio-recording-capture, both required here.  Every
;; public name is unchanged so existing callers and tests keep working.

;;; Code:

(require 'system-lib)
(require 'keybindings)  ;; provides cj/custom-keymap
(require 'video-audio-recording-devices)
(require 'video-audio-recording-capture)

;;; ============================================================
;;; Configuration Variables
;;; ============================================================

;; Forward declarations for variables defined in user-constants.el
(eval-when-compile (defvar video-recordings-dir))
(eval-when-compile (defvar audio-recordings-dir))
(defvar cj/custom-keymap)

(defvar cj/recording-mic-boost 2.0
  "Volume multiplier for microphone in recordings.
1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB).")

(defvar cj/recording-system-volume 2.0
  "Volume multiplier for system audio in recordings.
1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB).
Default is 2.0 because the amerge filter reduces level, so 2.0x compensates.")

(defvar cj/recording-mic-device nil
  "PulseAudio device name for microphone input.
If nil, will auto-detect on first use.")

(defvar cj/recording-system-device nil
  "PulseAudio device name for system audio monitor.
If nil, will auto-detect on first use.")

;;; ============================================================
;;; Internal State
;;; ============================================================

;; These hold the Emacs process objects for running recordings.
;; The process is the shell that runs the ffmpeg (or wf-recorder|ffmpeg)
;; pipeline. When non-nil, a recording is in progress.  The capture engine
;; reads and clears them; the toggle commands below read them.

(defvar cj/video-recording-ffmpeg-process nil
  "Emacs process object for the active video recording shell, or nil.")

(defvar cj/audio-recording-ffmpeg-process nil
  "Emacs process object for the active audio recording shell, or nil.")

;;; ============================================================
;;; Device Diagnostics and Selection Commands
;;; ============================================================

(defun cj/recording-list-devices ()
  "Show all available audio sources in a readable format.
Opens a buffer showing devices with their states."
  (interactive)
  (let ((sources (cj/recording-parse-sources)))
    (with-current-buffer (get-buffer-create "*Recording Devices*")
      (erase-buffer)
      (insert "Available Audio Sources\n")
      (insert "========================\n\n")
      (insert "Note: 'Ready' devices are available and will activate when recording starts.\n\n")
      (insert "Current Configuration:\n")
      (insert (format "  Microphone:   %s\n" (or cj/recording-mic-device "Not set")))
      (insert (format "  System Audio: %s\n\n" (or cj/recording-system-device "Not set")))
      (insert "Available Devices:\n\n")
      (if sources
          (dolist (source sources)
            (let ((device (nth 0 source))
                  (driver (nth 1 source))
                  (_state (nth 2 source))
                  (friendly-state (cj/recording-friendly-state (nth 2 source))))
              (insert (format "%-10s [%s]\n" friendly-state driver))
              (insert (format "  %s\n\n" device))))
        (insert "  No audio sources found. Is PulseAudio/PipeWire running?\n"))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer-other-window "*Recording Devices*")))

(defun cj/recording-show-active-audio ()
  "Show which audio sinks are currently PLAYING audio.
Useful for diagnosing why phone call audio isn't being captured — helps
identify which device the phone app is actually using for output."
  (interactive)
  (let ((output (shell-command-to-string "pactl list sink-inputs")))
    (with-current-buffer (get-buffer-create "*Active Audio Playback*")
      (erase-buffer)
      (insert "Active Audio Playback (Updated: " (format-time-string "%H:%M:%S") ")\n")
      (insert "======================================================\n\n")
      (insert "This shows which applications are CURRENTLY playing audio and through which device.\n")
      (insert "If you're on a phone call, you should see the phone app listed here.\n")
      (insert "The 'Sink' line shows which output device it's using.\n\n")
      (if (string-match-p "Sink Input" output)
          (progn
            (insert output)
            (insert "\n\nTIP: The '.monitor' device corresponding to the 'Sink' above is what\n")
            (insert "you need to select for system audio to capture the other person's voice.\n\n")
            (insert "For example, if Sink is 'alsa_output.usb...Jabra...analog-stereo',\n")
            (insert "then you need 'alsa_output.usb...Jabra...analog-stereo.monitor'\n"))
        (insert "No active audio playback detected.\n\n")
        (insert "This means no applications are currently playing audio.\n")
        (insert "If you're on a phone call and see this, the phone app might be:\n")
        (insert "  1. Using a different audio system (not PulseAudio/PipeWire)\n")
        (insert "  2. Using a Bluetooth device directly (bypassing system audio)\n")
        (insert "  3. Not actually playing audio (check if you can hear the other person)\n"))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer-other-window "*Active Audio Playback*")
    (message "Showing active audio playback. Press 'g' to refresh, 'q' to quit.")))

(defun cj/recording-select-devices ()
  "Interactively select microphone and system audio devices separately.
Sets `cj/recording-mic-device' and `cj/recording-system-device'."
  (interactive)
  (setq cj/recording-mic-device
        (cj/recording-select-device "Select microphone device: " 'mic))
  (setq cj/recording-system-device
        (cj/recording-select-device "Select system audio monitor: " 'monitor))
  (message "Devices set - Mic: %s, System: %s"
           cj/recording-mic-device
           cj/recording-system-device))

;;; ============================================================
;;; Device Testing
;;; ============================================================

(defun cj/recording--test-device (device prefix prompt-action)
  "Record 5 seconds from DEVICE and play it back.
PREFIX is the temp file name prefix (e.g. \"mic-test-\").
PROMPT-ACTION is the action text shown to the user (e.g. \"SPEAK NOW!\")."
  (let* ((temp-file (make-temp-file prefix nil ".wav"))
         (duration 5))
    (message "Recording for %d seconds... %s" duration prompt-action)
    (shell-command
     (format "ffmpeg -f pulse -i %s -t %d -y %s 2>/dev/null"
             (shell-quote-argument device)
             duration
             (shell-quote-argument temp-file)))
    (message "Playing back recording...")
    (shell-command (format "ffplay -autoexit -nodisp %s 2>/dev/null &"
                           (shell-quote-argument temp-file)))
    (message "Test complete. Temp file: %s" temp-file)))

(defun cj/recording-test-mic ()
  "Test microphone by recording 5 seconds and playing it back."
  (interactive)
  (unless cj/recording-mic-device
    (user-error "No microphone configured. Run C-; r s first"))
  (cj/recording--test-device cj/recording-mic-device "mic-test-" "SPEAK NOW!"))

(defun cj/recording-test-monitor ()
  "Test system audio monitor by recording 5 seconds and playing it back.
Play some audio/video during the test so there's something to capture."
  (interactive)
  (unless cj/recording-system-device
    (user-error "No system monitor configured. Run C-; r s first"))
  (cj/recording--test-device cj/recording-system-device "monitor-test-" "PLAY SOMETHING NOW!"))

(defun cj/recording-test-both ()
  "Guided test of both mic and monitor together.
Runs three sequential tests:
  1. Mic only — speak into it
  2. Monitor only — play audio/video
  3. Both together — speak while audio plays
Run this before important recordings to verify everything works."
  (interactive)
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Devices not configured. Run C-; r s first"))

  (when (y-or-n-p "Test 1: Record from MICROPHONE only (5 sec). Ready? ")
    (cj/recording-test-mic)
    (sit-for 6))

  (when (y-or-n-p "Test 2: Record from SYSTEM AUDIO only (5 sec). Start playing audio/video, then press y: ")
    (cj/recording-test-monitor)
    (sit-for 6))

  (when (y-or-n-p "Test 3: Record BOTH mic + system audio (5 sec). Speak while audio plays, then press y: ")
    (let* ((temp-file (make-temp-file "both-test-" nil ".wav"))
           (duration 5))
      (message "Recording BOTH for %d seconds... SPEAK + PLAY AUDIO NOW!" duration)
      (shell-command
       (format "ffmpeg -f pulse -i %s -f pulse -i %s -filter_complex \"[0:a]volume=%.1f[mic];[1:a]volume=%.1f[sys];[mic][sys]amix=inputs=2:duration=longest\" -t %d -y %s 2>/dev/null"
               (shell-quote-argument cj/recording-mic-device)
               (shell-quote-argument cj/recording-system-device)
               cj/recording-mic-boost
               cj/recording-system-volume
               duration
               (shell-quote-argument temp-file)))
      (message "Playing back recording...")
      (shell-command (format "ffplay -autoexit -nodisp %s 2>/dev/null &"
                             (shell-quote-argument temp-file)))
      (sit-for 6)
      (message "All tests complete! Temp file: %s" temp-file)))

  (message "Device testing complete. If you heard audio in all tests, recording will work!"))

;;; ============================================================
;;; Toggle Commands (User-Facing)
;;; ============================================================

(defun cj/recording--normalize-recording-dir (location)
  "Return LOCATION as an absolute directory path ending in a slash.
The recording target is always a directory ffmpeg writes a timestamped
file into, so normalizing here ensures the *selected* directory is the
one created and recorded into, not its parent (which is what
`file-name-directory' would yield for a path without a trailing slash)."
  (file-name-as-directory (expand-file-name location)))

(defun cj/video-recording-toggle (arg)
  "Toggle video recording: start if not recording, stop if recording.
On first use (or when devices not configured), runs quick setup (C-; r s).
With prefix ARG, prompt for recording location.
Otherwise use the default location in `video-recordings-dir'."
  (interactive "P")
  (if cj/video-recording-ffmpeg-process
      (cj/video-recording-stop)
    (let ((directory (cj/recording--normalize-recording-dir
                      (if arg
                          (read-directory-name "Enter recording location: ")
                        video-recordings-dir))))
      (make-directory directory t)
      (cj/ffmpeg-record-video directory))))

(defun cj/audio-recording-toggle (arg)
  "Toggle audio recording: start if not recording, stop if recording.
On first use (or when devices not configured), runs quick setup (C-; r s).
With prefix ARG, prompt for recording location.
Otherwise use the default location in `audio-recordings-dir'."
  (interactive "P")
  (if cj/audio-recording-ffmpeg-process
      (cj/audio-recording-stop)
    (let ((directory (cj/recording--normalize-recording-dir
                      (if arg
                          (read-directory-name "Enter recording location: ")
                        audio-recordings-dir))))
      (make-directory directory t)
      (cj/ffmpeg-record-audio directory))))

;;; ============================================================
;;; Volume Adjustment
;;; ============================================================

(defun cj/recording-adjust-volumes ()
  "Interactively adjust recording volume levels.
Changes take effect on the next recording (not the current one)."
  (interactive)
  (let ((mic (read-number "Microphone boost (1.0 = normal, 2.0 = double): "
                          cj/recording-mic-boost))
        (sys (read-number "System audio level (1.0 = normal, 0.5 = half): "
                          cj/recording-system-volume)))
    (setq cj/recording-mic-boost mic)
    (setq cj/recording-system-volume sys)
    (message "Recording levels updated - Mic: %.1fx, System: %.1fx" mic sys)))

;;; ============================================================
;;; Keybindings
;;; ============================================================

;; All recording operations are under the C-; r prefix.
(defvar cj/record-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'cj/video-recording-toggle)
    (define-key map (kbd "a") #'cj/audio-recording-toggle)
    (define-key map (kbd "l") #'cj/recording-adjust-volumes)
    (define-key map (kbd "d") #'cj/recording-list-devices)
    (define-key map (kbd "w") #'cj/recording-show-active-audio)
    (define-key map (kbd "s") #'cj/recording-quick-setup)
    (define-key map (kbd "S") #'cj/recording-select-devices)
    (define-key map (kbd "t m") #'cj/recording-test-mic)
    (define-key map (kbd "t s") #'cj/recording-test-monitor)
    (define-key map (kbd "t b") #'cj/recording-test-both)
    map)
  "Keymap for video/audio recording operations under C-; r.")

(cj/register-prefix-map "r" cj/record-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; r" "recording menu"
    "C-; r v" "toggle video recording"
    "C-; r a" "toggle audio recording"
    "C-; r l" "adjust levels"
    "C-; r d" "list devices"
    "C-; r w" "what's playing (diagnostics)"
    "C-; r s" "quick setup"
    "C-; r S" "select devices (advanced)"
    "C-; r t" "test devices"
    "C-; r t m" "test microphone"
    "C-; r t s" "test system audio"
    "C-; r t b" "test both (guided)"))


(provide 'video-audio-recording)
;;; video-audio-recording.el ends here.
