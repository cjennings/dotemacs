;;; video-audio-recording.el --- Video and Audio Recording -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Desktop video and audio recording from within Emacs using ffmpeg.
;; Records from both microphone and system audio simultaneously, which
;; makes it suitable for capturing meetings, presentations, and desktop activity.
;;
;; Architecture:
;;   - Audio recordings use ffmpeg directly with PulseAudio inputs → M4A/AAC
;;   - Video recordings differ by display server:
;;     - X11: ffmpeg with x11grab + PulseAudio → MKV
;;     - Wayland: wf-recorder piped to ffmpeg for audio mixing → MKV
;;       (wf-recorder captures the compositor, ffmpeg mixes in audio)
;;
;; Process lifecycle:
;;   - Start: `start-process-shell-command` creates a shell running the
;;     ffmpeg (or wf-recorder|ffmpeg) pipeline. Process ref is stored in
;;     `cj/video-recording-ffmpeg-process' or `cj/audio-recording-ffmpeg-process'.
;;   - Stop: SIGINT is sent to the shell's process group so all pipeline
;;     children (wf-recorder, ffmpeg) receive it. We then poll until the
;;     process actually exits, giving ffmpeg time to finalize the container.
;;   - Cleanup: A process sentinel auto-clears the process variable and
;;     updates the modeline if the process dies unexpectedly.
;;
;; Note: video-recordings-dir and audio-recordings-dir are defined
;; (and directory created) in user-constants.el
;;
;; Quick Start
;; ===========
;; 1. Press C-; r s to run quick setup
;; 2. Pick a microphone from the list
;; 3. Pick an audio output — [in use] shows which apps are playing
;; 4. Press C-; r a to start/stop audio recording
;; 5. Recording starts - you'll see 󰍬 in your modeline
;; 6. Press C-; r a again to stop (🔴 disappears)
;;
;; Device Setup
;; ============
;; C-; r a automatically prompts for device selection on first use.
;; Device selection lasts for the current Emacs session only.
;;
;; Manual device selection:
;;
;; C-; r s (cj/recording-quick-setup) - RECOMMENDED
;;   Two-step setup: pick a mic, then pick an audio output to capture.
;;   Both steps show status: [in use], [ready], [available], [muted].
;;   Audio outputs also show which apps are playing through them.
;;   Sorted: in use → ready → available → muted.
;;
;; C-; r S (cj/recording-select-devices) - ADVANCED
;;   Manual selection: choose mic and monitor separately.
;;   Use when you need different devices for input/output.
;;
;; C-; r d (cj/recording-list-devices)
;;   List all available audio devices and current configuration.
;;
;; C-; r w (cj/recording-show-active-audio) - DIAGNOSTIC TOOL
;;   Show which apps are currently playing audio and through which device.
;;   Use this DURING a phone call to see if the call audio is going through
;;   the device you think it is. Helps diagnose "missing one side" issues.
;;
;; Pre-Recording Validation
;; ========================
;; Every time you start a recording, the system audio device is
;; validated automatically:
;;   1. If the configured monitor device no longer exists (e.g.
;;      USB DAC unplugged), it's auto-updated to the current
;;      default sink's monitor.
;;   2. If no audio is currently playing through the monitored sink,
;;      a warning is shown in the echo area. Recording proceeds
;;      without interruption — run C-; r s to see active streams.
;;
;; Testing Devices Before Important Recordings
;; ============================================
;; Always test devices before important recordings:
;;
;; C-; r t b (cj/recording-test-both) - RECOMMENDED
;;   Guided test: mic only, monitor only, then both together.
;;   Catches hardware issues before they ruin recordings!
;;
;; C-; r t m (cj/recording-test-mic)
;;   Quick 5-second mic test with playback.
;;
;; C-; r t s (cj/recording-test-monitor)
;;   Quick 5-second system audio test with playback.
;;
;; To adjust volumes:
;; - Use =M-x cj/recording-adjust-volumes= (or your keybinding =r l=)
;; - Or customize permanently: =M-x customize-group RET cj-recording RET=
;; - Or in your config:
;;   #+begin_src emacs-lisp
;;   (setq cj/recording-mic-boost 1.5)    ; 50% louder
;;   (setq cj/recording-system-volume 0.7) ; 30% quieter
;;
;;; Code:

(require 'system-lib)

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
;; pipeline. When non-nil, a recording is in progress.

(defvar cj/video-recording-ffmpeg-process nil
  "Emacs process object for the active video recording shell, or nil.")

(defvar cj/audio-recording-ffmpeg-process nil
  "Emacs process object for the active audio recording shell, or nil.")

;;; ============================================================
;;; Modeline Indicator
;;; ============================================================

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

;;; ============================================================
;;; Process Lifecycle (Sentinel and Graceful Shutdown)
;;; ============================================================

(defun cj/recording-process-sentinel (process event)
  "Sentinel for recording processes — handles unexpected exits.
PROCESS is the ffmpeg shell process, EVENT describes what happened.
This is called by Emacs when the process changes state (exits, is killed, etc.).
It clears the process variable and updates the modeline so the recording indicator
disappears even if the recording crashes or is killed externally."
  (when (memq (process-status process) '(exit signal))
    (cond
     ((eq process cj/audio-recording-ffmpeg-process)
      (setq cj/audio-recording-ffmpeg-process nil)
      (message "Audio recording stopped: %s" (string-trim event)))
     ((eq process cj/video-recording-ffmpeg-process)
      (setq cj/video-recording-ffmpeg-process nil)
      (message "Video recording stopped: %s" (string-trim event))))
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

;;; ============================================================
;;; Dependency Checks
;;; ============================================================

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

;;; ============================================================
;;; PulseAudio Device Discovery
;;; ============================================================
;;
;; Audio devices are discovered via `pactl list sources short'.
;; Two types of sources matter:
;;   - Input sources (microphones): capture your voice
;;   - Monitor sources (*.monitor): capture system audio output
;;     These tap into what's playing through speakers/headphones,
;;     which is how we capture system audio (music, calls, etc.).
;;
;; Device selection is required before first recording. The quick
;; setup (C-; r s) groups hardware devices and lets you pick one
;; device to use for both mic and monitor — ideal for headsets.

(defun cj/recording--parse-pactl-output (output)
  "Parse pactl sources OUTPUT into structured list.
Returns list of (device-name driver state) tuples.
Extracted as a separate function for testability."
  (let ((sources nil))
    (dolist (line (split-string output "\n" t))
      (when (string-match "^[0-9]+\t\\([^\t]+\\)\t\\([^\t]+\\)\t\\([^\t]+\\)\t\\([^\t]+\\)" line)
        (let ((device (match-string 1 line))
              (driver (match-string 2 line))
              (state (match-string 4 line)))
          (push (list device driver state) sources))))
    (nreverse sources)))

(defun cj/recording-parse-sources ()
  "Parse pactl sources output into structured list.
Returns list of (device-name driver state) tuples."
  (cj/recording--parse-pactl-output
   (shell-command-to-string "pactl list sources short 2>/dev/null")))

(defun cj/recording-friendly-state (state)
  "Convert technical STATE name to user-friendly label.
STATE is the raw state from pactl (SUSPENDED, RUNNING, IDLE, etc.)."
  (pcase state
    ("SUSPENDED" "Ready")
    ("RUNNING" "Active")
    ("IDLE" "Ready")
    (_ state)))

(defun cj/recording--get-default-sink-monitor ()
  "Return the PulseAudio monitor source for the default audio output.
The monitor source captures whatever is playing through the default sink
(music, calls, system sounds, etc.).  This is the correct device
for capturing \"what I hear\" regardless of which output hardware is active."
  (let ((default-sink (string-trim
                       (shell-command-to-string
                        "pactl get-default-sink 2>/dev/null"))))
    (if (string-empty-p default-sink)
        (user-error "No default audio output found.  Is PulseAudio/PipeWire running?")
      (concat default-sink ".monitor"))))

(defun cj/recording--parse-pactl-sources-verbose (output)
  "Parse verbose `pactl list sources' OUTPUT into structured list.
Returns list of (name description mute state) tuples.
OUTPUT should be the full output of `pactl list sources'."
  (let ((sources nil)
        (current-name nil)
        (current-desc nil)
        (current-mute nil)
        (current-state nil))
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "^Source #" line)
        ;; Save previous source if complete
        (when current-name
          (push (list current-name current-desc current-mute current-state)
                sources))
        (setq current-name nil current-desc nil
              current-mute nil current-state nil))
       ((string-match "^\\s-+Name:\\s-+\\(.+\\)" line)
        (setq current-name (match-string 1 line)))
       ((string-match "^\\s-+Description:\\s-+\\(.+\\)" line)
        (setq current-desc (match-string 1 line)))
       ((string-match "^\\s-+Mute:\\s-+\\(.+\\)" line)
        (setq current-mute (match-string 1 line)))
       ((string-match "^\\s-+State:\\s-+\\(.+\\)" line)
        (setq current-state (match-string 1 line)))))
    ;; Don't forget the last source
    (when current-name
      (push (list current-name current-desc current-mute current-state)
            sources))
    (nreverse sources)))

(defun cj/recording--get-available-mics ()
  "Return available microphone sources as list of (name description state mute).
Filters out monitor sources but includes muted devices (shown with
a [muted] label in the UI).  Uses the friendly description from
PulseAudio (e.g. \"Jabra SPEAK 510 Mono\") rather than the raw
device name.  State is the PulseAudio state string (RUNNING, IDLE,
or SUSPENDED).  Mute is \"yes\" or \"no\"."
  (let* ((output (shell-command-to-string "pactl list sources 2>/dev/null"))
         (sources (cj/recording--parse-pactl-sources-verbose output))
         (mics nil))
    (dolist (source sources)
      (let ((name (nth 0 source))
            (desc (nth 1 source))
            (mute (nth 2 source))
            (state (nth 3 source)))
        (when (not (string-match-p "\\.monitor$" name))
          (push (list name (or desc name) state mute) mics))))
    (nreverse mics)))

(defun cj/recording--parse-pactl-sinks-verbose (output)
  "Parse verbose `pactl list sinks' OUTPUT into structured list.
Returns list of (name description mute state) tuples.
OUTPUT should be the full output of `pactl list sinks'."
  (let ((sinks nil)
        (current-name nil)
        (current-desc nil)
        (current-mute nil)
        (current-state nil))
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "^Sink #" line)
        ;; Save previous sink if complete
        (when current-name
          (push (list current-name current-desc current-mute current-state)
                sinks))
        (setq current-name nil current-desc nil
              current-mute nil current-state nil))
       ((string-match "^\\s-+Name:\\s-+\\(.+\\)" line)
        (setq current-name (match-string 1 line)))
       ((string-match "^\\s-+Description:\\s-+\\(.+\\)" line)
        (setq current-desc (match-string 1 line)))
       ((string-match "^\\s-+Mute:\\s-+\\(.+\\)" line)
        (setq current-mute (match-string 1 line)))
       ((string-match "^\\s-+State:\\s-+\\(.+\\)" line)
        (setq current-state (match-string 1 line)))))
    ;; Don't forget the last sink
    (when current-name
      (push (list current-name current-desc current-mute current-state)
            sinks))
    (nreverse sinks)))

(defun cj/recording--get-available-sinks ()
  "Return available audio sinks as list of (name description state mute).
Includes muted sinks (shown with a [muted] label in the UI).  Uses
the friendly description from PulseAudio (e.g. \"JDS Labs Element IV
Analog Stereo\").  State is the PulseAudio state string (RUNNING,
IDLE, or SUSPENDED).  Mute is \"yes\" or \"no\"."
  (let* ((output (shell-command-to-string "pactl list sinks 2>/dev/null"))
         (sinks (cj/recording--parse-pactl-sinks-verbose output))
         (result nil))
    (dolist (sink sinks)
      (let ((name (nth 0 sink))
            (desc (nth 1 sink))
            (mute (nth 2 sink))
            (state (nth 3 sink)))
        (push (list name (or desc name) state mute) result)))
    (nreverse result)))

;;; ============================================================
;;; Device Selection UI
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

(defun cj/recording-select-device (prompt device-type)
  "Interactively select an audio device.
PROMPT is shown to user.  DEVICE-TYPE is \\='mic or \\='monitor for filtering.
Monitor devices end in .monitor (they tap system audio output).
Returns selected device name or nil."
  (let* ((sources (cj/recording-parse-sources))
         (filtered (if (eq device-type 'monitor)
                       (seq-filter (lambda (s) (string-match-p "\\.monitor$" (car s))) sources)
                     (seq-filter (lambda (s) (not (string-match-p "\\.monitor$" (car s)))) sources)))
         (choices (mapcar (lambda (s)
                           (let ((device (nth 0 s))
                                 (_driver (nth 1 s))
                                 (_state (nth 2 s))
                                 (friendly-state (cj/recording-friendly-state (nth 2 s))))
                             (cons (format "%-10s %s" friendly-state device) device)))
                         filtered)))
    (if choices
        (cdr (assoc (completing-read prompt choices nil t) choices))
      (user-error "No %s devices found" (if (eq device-type 'monitor) "monitor" "input")))))

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

(defun cj/recording-group-devices-by-hardware ()
  "Group audio sources by physical hardware device.
Returns alist of (friendly-name . (mic-source . monitor-source)).
Only includes devices that have BOTH a mic and a monitor source,
since recording needs both to capture your voice and system audio."
  (let ((sources (cj/recording-parse-sources))
        (devices (make-hash-table :test 'equal))
        (result nil))
    ;; Group sources by base device name (hardware identifier)
    (dolist (source sources)
      (let* ((device (nth 0 source))
             ;; Extract hardware ID — the unique part identifying the physical device.
             ;; Different device types use different naming conventions in PulseAudio.
             (base-name (cond
                         ;; USB devices: extract usb-XXXXX-XX part
                         ((string-match "\\.\\(usb-[^.]+\\-[0-9]+\\)\\." device)
                          (match-string 1 device))
                         ;; Built-in (PCI) devices: extract pci-XXXXX part
                         ((string-match "\\.\\(pci-[^.]+\\)\\." device)
                          (match-string 1 device))
                         ;; Bluetooth devices: extract and normalize MAC address
                         ;; (input uses colons, output uses underscores)
                         ((string-match "bluez_\\(?:input\\|output\\)\\.\\([^.]+\\)" device)
                          (replace-regexp-in-string "_" ":" (match-string 1 device)))
                         (t device)))
             (is-monitor (string-match-p "\\.monitor$" device))
             (device-entry (gethash base-name devices)))
        (unless device-entry
          (setf device-entry (cons nil nil))
          (puthash base-name device-entry devices))
        (if is-monitor
            (setcdr device-entry device)
          (setcar device-entry device))))

    ;; Convert hash table to alist with user-friendly names
    (maphash (lambda (base-name pair)
               (when (and (car pair) (cdr pair))
                 (let ((friendly-name
                        (cond
                         ((string-match-p "usb.*[Jj]abra" base-name) "Jabra SPEAK 510 USB")
                         ((string-match-p "^usb-" base-name) "USB Audio Device")
                         ((string-match-p "^pci-" base-name) "Built-in Audio")
                         ((string-match-p "^[0-9A-Fa-f:]+$" base-name) "Bluetooth Headset")
                         (t base-name))))
                   (push (cons friendly-name pair) result))))
             devices)
    (nreverse result)))

(defun cj/recording--device-sort-key (state muted)
  "Return a numeric sort key for a device with STATE and MUTED flag.
Lower values sort first: RUNNING (0) → IDLE (1) → SUSPENDED (2) → muted (3)."
  (if (equal muted "yes")
      3
    (pcase (upcase (or state ""))
      ("RUNNING" 0)
      ("IDLE" 1)
      (_ 2))))

(defun cj/recording--device-status-label (state muted)
  "Return a human-readable status label for a device.
MUTED is \"yes\" or \"no\".  STATE is the PulseAudio state string."
  (if (equal muted "yes")
      "[muted]"
    (pcase (upcase (or state ""))
      ("RUNNING" "[in use]")
      ("IDLE"    "[ready]")
      (_         "[available]"))))

(defun cj/recording--label-devices (devices)
  "Build labeled (label . name) alist from DEVICES for `completing-read'.
DEVICES is a list of (name description state mute) as returned by
`cj/recording--get-available-mics' or `cj/recording--get-available-sinks'.
Labels are formatted as \"Description [in use]\" etc.
Sorted: in use → ready → available → muted."
  (let* ((labeled (mapcar
                   (lambda (dev)
                     (let* ((name  (nth 0 dev))
                            (desc  (nth 1 dev))
                            (state (nth 2 dev))
                            (muted (nth 3 dev))
                            (label (concat desc " "
                                           (cj/recording--device-status-label state muted))))
                       (list label name (cj/recording--device-sort-key state muted))))
                   devices))
         (sorted (sort labeled (lambda (a b) (< (nth 2 a) (nth 2 b))))))
    (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry))) sorted)))

(defun cj/recording--get-sink-apps ()
  "Return alist mapping sink index to list of application names.
Parses `pactl list sink-inputs' to find which apps are playing
audio through each sink."
  (let ((output (shell-command-to-string "pactl list sink-inputs 2>/dev/null"))
        (apps (make-hash-table :test 'equal))
        (current-sink nil))
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "^Sink Input #" line)
        (setq current-sink nil))
       ((string-match "^[ \t]+Sink:[ \t]+\\([0-9]+\\)" line)
        (setq current-sink (match-string 1 line)))
       ((and current-sink
             (string-match "application\\.name = \"\\([^\"]+\\)\"" line))
        (let ((existing (gethash current-sink apps)))
          (unless (member (match-string 1 line) existing)
            (puthash current-sink
                     (append existing (list (match-string 1 line)))
                     apps))))))
    ;; Convert hash to alist
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k v) result)) apps)
      result)))

(defun cj/recording--label-sinks (sinks)
  "Build labeled (label . name) alist from SINKS for `completing-read'.
Like `cj/recording--label-devices' but also appends application names
for sinks with active audio streams.  E.g. \"JDS Labs [in use] (Firefox)\"."
  (let* ((sink-apps (cj/recording--get-sink-apps))
         (sinks-short (shell-command-to-string "pactl list sinks short 2>/dev/null"))
         (labeled
          (mapcar
           (lambda (dev)
             (let* ((name  (nth 0 dev))
                    (desc  (nth 1 dev))
                    (state (nth 2 dev))
                    (muted (nth 3 dev))
                    (index (cj/recording--get-sink-index name sinks-short))
                    (apps  (and index (cdr (assoc index sink-apps))))
                    (status (cj/recording--device-status-label state muted))
                    (app-str (if apps (concat " (" (string-join apps ", ") ")") ""))
                    (label (concat desc " " status app-str)))
               (list label name (cj/recording--device-sort-key state muted))))
           sinks))
         (sorted (sort labeled (lambda (a b) (< (nth 2 a) (nth 2 b))))))
    (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry))) sorted)))

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
  ;; Step 1: Mic selection
  (let* ((mics (cj/recording--get-available-mics))
         (mic-entries (cj/recording--label-devices mics))
         (mic-alist-with-cancel (append mic-entries '(("Cancel" . nil)))))
    (if (null mic-entries)
        (user-error "No microphones found.  Is a mic connected?")
      (let* ((mic-choice (completing-read "Select microphone: "
                                          (lambda (string pred action)
                                            (if (eq action 'metadata)
                                                '(metadata (display-sort-function . identity))
                                              (complete-with-action action mic-alist-with-cancel string pred)))
                                          nil t))
             (mic-device (cdr (assoc mic-choice mic-alist-with-cancel))))
        (if (null mic-device)
            (user-error "Device setup cancelled")
          ;; Step 2: Sink selection
          (let* ((sinks (cj/recording--get-available-sinks))
                 (sink-entries (cj/recording--label-sinks sinks))
                 (sink-alist-with-cancel (append sink-entries '(("Cancel" . nil))))
                 (sink-choice (completing-read "Select audio output to capture: "
                                              (lambda (string pred action)
                                                (if (eq action 'metadata)
                                                    '(metadata (display-sort-function . identity))
                                                  (complete-with-action action sink-alist-with-cancel string pred)))
                                              nil t))
                 (sink-device (cdr (assoc sink-choice sink-alist-with-cancel))))
            (if (null sink-device)
                (user-error "Device setup cancelled")
              (setq cj/recording-mic-device mic-device)
              (setq cj/recording-system-device (concat sink-device ".monitor"))
              (message "Recording ready!\n  Mic: %s\n  System audio: %s.monitor"
                       mic-choice
                       (file-name-nondirectory sink-device)))))))))

;;; ============================================================
;;; Device Testing
;;; ============================================================
;;
;; These functions record short clips and play them back so you can
;; verify hardware works BEFORE an important recording.

(defun cj/recording-test-mic ()
  "Test microphone by recording 5 seconds and playing it back."
  (interactive)
  (unless cj/recording-mic-device
    (user-error "No microphone configured. Run C-; r s first"))
  (let* ((temp-file (make-temp-file "mic-test-" nil ".wav"))
         (duration 5))
    (message "Recording from mic for %d seconds... SPEAK NOW!" duration)
    (shell-command
     (format "ffmpeg -f pulse -i %s -t %d -y %s 2>/dev/null"
             (shell-quote-argument cj/recording-mic-device)
             duration
             (shell-quote-argument temp-file)))
    (message "Playing back recording...")
    (shell-command (format "ffplay -autoexit -nodisp %s 2>/dev/null &"
                           (shell-quote-argument temp-file)))
    (message "Mic test complete. Temp file: %s" temp-file)))

(defun cj/recording-test-monitor ()
  "Test system audio monitor by recording 5 seconds and playing it back.
Play some audio/video during the test so there's something to capture."
  (interactive)
  (unless cj/recording-system-device
    (user-error "No system monitor configured. Run C-; r s first"))
  (let* ((temp-file (make-temp-file "monitor-test-" nil ".wav"))
         (duration 5))
    (message "Recording system audio for %d seconds... PLAY SOMETHING NOW!" duration)
    (shell-command
     (format "ffmpeg -f pulse -i %s -t %d -y %s 2>/dev/null"
             (shell-quote-argument cj/recording-system-device)
             duration
             (shell-quote-argument temp-file)))
    (message "Playing back recording...")
    (shell-command (format "ffplay -autoexit -nodisp %s 2>/dev/null &"
                           (shell-quote-argument temp-file)))
    (message "Monitor test complete. Temp file: %s" temp-file)))

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
;;; Device Validation
;;; ============================================================

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

(defun cj/recording--source-exists-p (source-name pactl-output)
  "Return non-nil if SOURCE-NAME exists in PACTL-OUTPUT.
PACTL-OUTPUT should be the output of `pactl list sources short'."
  (let ((found nil))
    (dolist (line (split-string pactl-output "\n" t))
      (when (string-match "^[0-9]+\t\\([^\t]+\\)\t" line)
        (when (equal source-name (match-string 1 line))
          (setq found t))))
    found))

(defun cj/recording--get-sink-index (sink-name sinks-output)
  "Return the numeric index of SINK-NAME from SINKS-OUTPUT.
SINKS-OUTPUT should be the output of `pactl list sinks short'.
Returns the index as a string, or nil if not found."
  (let ((index nil))
    (dolist (line (split-string sinks-output "\n" t))
      (when (string-match "^\\([0-9]+\\)\t\\([^\t]+\\)\t" line)
        (when (equal sink-name (match-string 2 line))
          (setq index (match-string 1 line)))))
    index))

(defun cj/recording--sink-has-active-audio-p (sink-index pactl-output)
  "Return non-nil if SINK-INDEX has active audio streams.
PACTL-OUTPUT should be the output of `pactl list sink-inputs'.
SINK-INDEX is the numeric sink index as a string."
  (let ((found nil)
        (lines (split-string pactl-output "\n")))
    (dolist (line lines)
      (when (string-match "^[ \t]+Sink:[ \t]+\\([0-9]+\\)" line)
        (when (equal sink-index (match-string 1 line))
          (setq found t))))
    found))

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

;;; ============================================================
;;; Toggle Commands (User-Facing)
;;; ============================================================

(defun cj/video-recording-toggle (arg)
  "Toggle video recording: start if not recording, stop if recording.
On first use (or when devices not configured), runs quick setup (C-; r s).
With prefix ARG, prompt for recording location.
Otherwise use the default location in `video-recordings-dir'."
  (interactive "P")
  (if cj/video-recording-ffmpeg-process
      (cj/video-recording-stop)
    (let* ((location (if arg
                         (read-directory-name "Enter recording location: ")
                       video-recordings-dir))
           (directory (file-name-directory location)))
      (unless (file-directory-p directory)
        (make-directory directory t))
      (cj/ffmpeg-record-video location))))

(defun cj/audio-recording-toggle (arg)
  "Toggle audio recording: start if not recording, stop if recording.
On first use (or when devices not configured), runs quick setup (C-; r s).
With prefix ARG, prompt for recording location.
Otherwise use the default location in `audio-recordings-dir'."
  (interactive "P")
  (if cj/audio-recording-ffmpeg-process
      (cj/audio-recording-stop)
    (let* ((location (if arg
                         (read-directory-name "Enter recording location: ")
                       audio-recordings-dir))
           (directory (file-name-directory location)))
      (unless (file-directory-p directory)
        (make-directory directory t))
      (cj/ffmpeg-record-audio location))))

;;; ============================================================
;;; Start Recording
;;; ============================================================

(defun cj/ffmpeg-record-video (directory)
  "Start a video recording, saving output to DIRECTORY.
Uses wf-recorder on Wayland, x11grab on X11.

On Wayland, the pipeline is:
  wf-recorder (captures screen → H.264) | ffmpeg (mixes in audio → MKV)

On X11, ffmpeg handles everything:
  ffmpeg (x11grab for screen + PulseAudio for audio → MKV)"
  (cj/recording-check-ffmpeg)
  (unless cj/video-recording-ffmpeg-process
    ;; On Wayland, kill any orphan wf-recorder processes left over from
    ;; previous crashes. Without this, old wf-recorders hold the compositor
    ;; capture and new ones fail silently.
    (when (cj/recording--wayland-p)
      (call-process "pkill" nil nil nil "-INT" "wf-recorder")
      (sit-for 0.1))
    (let* ((devices (cj/recording-get-devices))
           (mic-device (car devices))
           (system-device (cdr devices))
           (location (expand-file-name directory))
           (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (filename (expand-file-name (concat name ".mkv") location))
           (on-wayland (cj/recording--wayland-p))
           (record-command
            (if on-wayland
                (progn
                  (cj/recording--check-wf-recorder)
                  ;; Wayland pipeline: wf-recorder captures screen as H.264 in
                  ;; matroska container, piped to ffmpeg which adds mic + system
                  ;; audio via PulseAudio, then writes the final MKV.
                  ;; -c:v copy means ffmpeg passes video through without re-encoding.
                  (format (concat "wf-recorder -y -c libx264 -m matroska -f /dev/stdout 2>/dev/null | "
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
              ;; X11: ffmpeg captures screen directly via x11grab
              (format (concat "ffmpeg -framerate 30 -f x11grab -i :0.0+ "
                              "-f pulse -i %s "
                              "-ac 1 "
                              "-f pulse -i %s "
                              "-ac 2 "
                              "-filter_complex \"[1:a]volume=%.1f[mic];[2:a]volume=%.1f[sys];[mic][sys]amerge=inputs=2[out]\" "
                              "-map 0:v -map \"[out]\" "
                              "%s")
                      mic-device
                      system-device
                      cj/recording-mic-boost
                      cj/recording-system-volume
                      filename))))
      (setq cj/video-recording-ffmpeg-process
            (start-process-shell-command "ffmpeg-video-recording"
                                         "*ffmpeg-video-recording*"
                                         record-command))
      (set-process-query-on-exit-flag cj/video-recording-ffmpeg-process nil)
      (set-process-sentinel cj/video-recording-ffmpeg-process #'cj/recording-process-sentinel)
      (force-mode-line-update t)
      (message "Started video recording to %s (%s, mic: %.1fx, system: %.1fx)."
               filename
               (if on-wayland "Wayland/wf-recorder" "X11")
               cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/ffmpeg-record-audio (directory)
  "Start an audio recording, saving output to DIRECTORY.
Records from microphone and system audio monitor (configured device),
mixing them together into a single M4A/AAC file.

The filter graph mixes two PulseAudio inputs:
  [mic] → volume boost → amerge → AAC encoder → .m4a
  [sys] → volume boost ↗"
  (cj/recording-check-ffmpeg)
  (unless cj/audio-recording-ffmpeg-process
    (let* ((devices (cj/recording-get-devices))
           (mic-device (car devices))
           (system-device (cdr devices))
           (location (expand-file-name directory))
           (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
           (filename (expand-file-name (concat name ".m4a") location))
           (ffmpeg-command
            (format (concat "ffmpeg "
                            "-f pulse -i %s "   ; Input 0: microphone
                            "-f pulse -i %s "   ; Input 1: system audio monitor
                            "-filter_complex \""
                            "[0:a]volume=%.1f[mic];"
                            "[1:a]volume=%.1f[sys];"
                            "[mic][sys]amix=inputs=2:duration=longest[out]\" "
                            "-map \"[out]\" "
                            "-c:a aac "
                            "-b:a 64k "
                            "%s")
                    mic-device
                    system-device
                    cj/recording-mic-boost
                    cj/recording-system-volume
                    filename)))
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

;;; ============================================================
;;; Stop Recording
;;; ============================================================
;;
;; Stopping a recording requires careful process management, especially
;; on Wayland where we have a two-process pipeline (wf-recorder | ffmpeg).
;;
;; Wayland shutdown order (CRITICAL — order matters!):
;;   1. Kill wf-recorder first (the producer). This closes the pipe
;;      to ffmpeg, giving ffmpeg a clean EOF on its video input.
;;   2. Signal the process group with SIGINT so ffmpeg begins its
;;      graceful shutdown (flushing audio, writing container metadata).
;;   3. Wait for the shell/ffmpeg to actually exit. MKV container
;;      finalization (index tables, seek entries) can take several
;;      seconds. A fixed `sit-for' is insufficient.
;;   4. Kill any remaining wf-recorder as a safety net.
;;
;; Why producer-first matters: In a `wf-recorder | ffmpeg` pipeline,
;; sending SIGINT to all processes simultaneously causes ffmpeg to
;; abort mid-stream (no clean EOF on pipe:0). The result is no output
;; file at all. Killing the producer first lets ffmpeg see EOF, start
;; its orderly shutdown, and then SIGINT reinforces "stop now."
;;
;; X11 shutdown: simpler — ffmpeg is the only process, so we just
;; send SIGINT to the process group and wait.

(defun cj/video-recording-stop ()
  "Stop the video recording, waiting for ffmpeg to finalize the file.
On Wayland, kills wf-recorder first so ffmpeg gets a clean EOF on its
video input pipe, then signals the process group. Waits up to 5 seconds
for ffmpeg to write container metadata before giving up."
  (interactive)
  (if (not cj/video-recording-ffmpeg-process)
      (message "No video recording in progress.")
    (let ((proc cj/video-recording-ffmpeg-process))
      ;; On Wayland, kill the producer (wf-recorder) FIRST so ffmpeg sees
      ;; a clean EOF on pipe:0. This triggers ffmpeg's orderly shutdown:
      ;; drain remaining frames, write container metadata, close file.
      ;; Without this, simultaneous SIGINT to both causes ffmpeg to abort
      ;; without creating a file.
      (when (cj/recording--wayland-p)
        (call-process "pkill" nil nil nil "-INT" "wf-recorder")
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
      ;; Safety net: kill any straggler wf-recorder on Wayland.
      (when (cj/recording--wayland-p)
        (call-process "pkill" nil nil nil "-INT" "wf-recorder"))
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
to flush audio frames and write the M4A container trailer."
  (interactive)
  (if (not cj/audio-recording-ffmpeg-process)
      (message "No audio recording in progress.")
    (let ((proc cj/audio-recording-ffmpeg-process))
      ;; Send SIGINT to the process group (see video-recording-stop for details)
      (let ((pid (process-id proc)))
        (when pid
          (signal-process (- pid) 2)))
      ;; M4A finalization is faster than MKV, but still needs time to write
      ;; the AAC trailer and flush the output buffer.
      (let ((exited (cj/recording--wait-for-exit proc 3)))
        (unless exited
          (message "Warning: recording process did not exit within 3 seconds")))
      ;; Fallback cleanup if sentinel hasn't fired yet
      (when (eq cj/audio-recording-ffmpeg-process proc)
        (setq cj/audio-recording-ffmpeg-process nil)
        (force-mode-line-update t))
      (message "Stopped audio recording."))))

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

;; Only bind keys when running interactively (not in batch/test mode)
(when (boundp 'cj/custom-keymap)
  (keymap-set cj/custom-keymap "r" cj/record-map))

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
