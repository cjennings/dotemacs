;;; video-audio-recording.el --- Video and Audio Recording -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Use ffmpeg to record desktop video or just audio.
;; Records audio from both microphone and system audio (for calls/meetings).
;; Audio recordings use M4A/AAC format for best compatibility.
;;
;; Note: video-recordings-dir and audio-recordings-dir are defined
;; (and directory created) in user-constants.el
;;
;; Quick Start
;; ===========
;; 1. Press C-; r a (start/stop audio recording)
;; 2. First time: you'll be prompted for device setup
;; 3. Choose "Bluetooth Headset" (or your device)
;; 4. Recording starts - you'll see 🔴Audio in your modeline
;; 5. Press C-; r a again to stop (🔴 disappears)
;;
;; Device Setup (First Time Only)
;; ===============================
;; C-; r a automatically prompts for device selection on first use.
;; Device selection persists across Emacs sessions.
;;
;; Manual device selection:
;;
;; C-; r c (cj/recording-quick-setup-for-calls) - RECOMMENDED
;;   Quick setup: picks one device for both mic and monitor.
;;   Perfect for calls, meetings, or when using headset.
;;
;; C-; r s (cj/recording-select-devices) - ADVANCED
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
;; Testing Devices Before Important Recordings
;; ============================================
;; Always test devices before important meetings/calls:
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

;; Forward declarations
(eval-when-compile (defvar video-recordings-dir))
(eval-when-compile (defvar audio-recordings-dir))
(defvar cj/custom-keymap)

(defvar cj/recording-mic-boost 2.0
  "Volume multiplier for microphone in recordings.
1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB).")

(defvar cj/recording-system-volume 2.0
  "Volume multiplier for system audio in recordings.
1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB).
Default is 2.0 because the pan filter reduces by 50%, so final level is 1.0x.")

(defvar cj/recording-mic-device nil
  "PulseAudio device name for microphone input.
If nil, will auto-detect on first use.")

(defvar cj/recording-system-device nil
  "PulseAudio device name for system audio monitor.
If nil, will auto-detect on first use.")

(defvar cj/video-recording-ffmpeg-process nil
  "Variable to store the process of the ffmpeg video recording.")

(defvar cj/audio-recording-ffmpeg-process nil
  "Variable to store the process of the ffmpeg audio recording.")

;; Modeline recording indicator
(defun cj/recording-modeline-indicator ()
  "Return modeline string showing active recordings.
Shows 🔴 when recording (audio and/or video).
Checks if process is actually alive, not just if variable is set."
  (let ((audio-active (and cj/audio-recording-ffmpeg-process
                          (process-live-p cj/audio-recording-ffmpeg-process)))
        (video-active (and cj/video-recording-ffmpeg-process
                          (process-live-p cj/video-recording-ffmpeg-process))))
    (cond
     ((and audio-active video-active) " 🔴A+V ")
     (audio-active " 🔴Audio ")
     (video-active " 🔴Video ")
     (t ""))))

(defun cj/recording-process-sentinel (process event)
  "Sentinel for recording processes to clean up and update modeline.
PROCESS is the ffmpeg process, EVENT describes what happened."
  (when (memq (process-status process) '(exit signal))
    ;; Process ended - clear the variable
    (cond
     ((eq process cj/audio-recording-ffmpeg-process)
      (setq cj/audio-recording-ffmpeg-process nil)
      (message "Audio recording stopped: %s" (string-trim event)))
     ((eq process cj/video-recording-ffmpeg-process)
      (setq cj/video-recording-ffmpeg-process nil)
      (message "Video recording stopped: %s" (string-trim event))))
    ;; Force modeline update
    (force-mode-line-update t)))

(defun cj/recording-check-ffmpeg ()
  "Check if ffmpeg is available.
Return t if found, nil otherwise."
  (unless (executable-find "ffmpeg")
    (user-error "Ffmpeg not found.  Install with: sudo pacman -S ffmpeg")
    nil)
  t)

;; Auto-detection functions removed - they were unreliable and preferred built-in
;; audio over Bluetooth/USB devices. Use explicit device selection instead:
;; - C-; r c (cj/recording-quick-setup-for-calls) - recommended for most use cases
;; - C-; r s (cj/recording-select-devices) - manual selection of mic + monitor

(defun cj/recording--parse-pactl-output (output)
  "Internal parser for pactl sources output.  Takes OUTPUT string.
Returns list of (device-name driver state) tuples.
Extracted for testing without shell command execution."
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
  "Convert technical state name to user-friendly label.
STATE is the raw state from pactl (SUSPENDED, RUNNING, IDLE, etc.)."
  (pcase state
    ("SUSPENDED" "Ready")
    ("RUNNING" "Active")
    ("IDLE" "Ready")
    (_ state)))  ; fallback to original if unknown

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
                  (state (nth 2 source))
                  (friendly-state (cj/recording-friendly-state (nth 2 source))))
              (insert (format "%-10s [%s]\n" friendly-state driver))
              (insert (format "  %s\n\n" device))))
        (insert "  No audio sources found. Is PulseAudio/PipeWire running?\n"))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer-other-window "*Recording Devices*")))

(defun cj/recording-show-active-audio ()
  "Show which audio sinks are currently PLAYING audio in real-time.
Useful for diagnosing why phone call audio isn't being captured - helps identify
which device the phone app is actually using for output."
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
PROMPT is shown to user.  DEVICE-TYPE is 'mic or 'monitor for filtering.
Returns selected device name or nil."
  (let* ((sources (cj/recording-parse-sources))
         (filtered (if (eq device-type 'monitor)
                       (seq-filter (lambda (s) (string-match-p "\\.monitor$" (car s))) sources)
                     (seq-filter (lambda (s) (not (string-match-p "\\.monitor$" (car s)))) sources)))
         (choices (mapcar (lambda (s)
                           (let ((device (nth 0 s))
                                 (driver (nth 1 s))
                                 (state (nth 2 s))
                                 (friendly-state (cj/recording-friendly-state (nth 2 s))))
                             (cons (format "%-10s %s" friendly-state device) device)))
                         filtered)))
    (if choices
        (cdr (assoc (completing-read prompt choices nil t) choices))
      (user-error "No %s devices found" (if (eq device-type 'monitor) "monitor" "input")))))

(defun cj/recording-select-devices ()
  "Interactively select microphone and system audio devices.
Sets cj/recording-mic-device and cj/recording-system-device."
  (interactive)
  (setq cj/recording-mic-device
        (cj/recording-select-device "Select microphone device: " 'mic))
  (setq cj/recording-system-device
        (cj/recording-select-device "Select system audio monitor: " 'monitor))
  (message "Devices set - Mic: %s, System: %s"
           cj/recording-mic-device
           cj/recording-system-device))

(defun cj/recording-group-devices-by-hardware ()
  "Group audio sources by hardware device.
Returns alist of (device-name . (mic-source . monitor-source))."
  (let ((sources (cj/recording-parse-sources))
        (devices (make-hash-table :test 'equal))
        (result nil))
    ;; Group sources by base device name
    (dolist (source sources)
      (let* ((device (nth 0 source))
             (driver (nth 1 source))
             ;; Extract hardware ID (the unique part that identifies the physical device)
             (base-name (cond
                         ;; USB devices: extract usb-XXXXX-XX part
                         ((string-match "\\.\\(usb-[^.]+\\-[0-9]+\\)\\." device)
                          (match-string 1 device))
                         ;; Built-in (pci) devices: extract pci-XXXXX part
                         ((string-match "\\.\\(pci-[^.]+\\)\\." device)
                          (match-string 1 device))
                         ;; Bluetooth devices: extract and normalize MAC address
                         ;; (input uses colons, output uses underscores - normalize to colons)
                         ((string-match "bluez_\\(?:input\\|output\\)\\.\\([^.]+\\)" device)
                          (replace-regexp-in-string "_" ":" (match-string 1 device)))
                         (t device)))
             (is-monitor (string-match-p "\\.monitor$" device))
             (device-entry (gethash base-name devices)))
        (unless device-entry
          (setf device-entry (cons nil nil))
          (puthash base-name device-entry devices))
        ;; Store mic or monitor in the pair
        (if is-monitor
            (setcdr device-entry device)
          (setcar device-entry device))))

    ;; Convert hash table to alist with friendly names
    (maphash (lambda (base-name pair)
               (when (and (car pair) (cdr pair))  ; Only include if we have both mic and monitor
                 (let ((friendly-name
                        (cond
                         ((string-match-p "usb.*[Jj]abra" base-name) "Jabra SPEAK 510 USB")
                         ((string-match-p "^usb-" base-name) "USB Audio Device")
                         ((string-match-p "^pci-" base-name) "Built-in Laptop Audio")
                         ((string-match-p "^[0-9A-Fa-f:]+$" base-name) "Bluetooth Headset")
                         (t base-name))))
                   (push (cons friendly-name pair) result))))
             devices)
    (nreverse result)))

(defun cj/recording-quick-setup-for-calls ()
  "Quick setup for recording call/meetings.
Detects available audio devices and lets you pick one device to use for
both microphone (your voice) and monitor (remote person + sound effects).
Perfect for recording video calls, phone calls, or presentations."
  (interactive)
  (let* ((grouped-devices (cj/recording-group-devices-by-hardware))
         (choices (mapcar #'car grouped-devices)))
    (if (null choices)
        (user-error "No complete audio devices found (need both mic and monitor)")
      (let* ((choice (completing-read "Which device are you using for the call? " choices nil t))
             (device-pair (cdr (assoc choice grouped-devices)))
             (mic (car device-pair))
             (monitor (cdr device-pair)))
        (setq cj/recording-mic-device mic)
        (setq cj/recording-system-device monitor)
        (message "Call recording ready! Using: %s\n  Mic: %s\n  Monitor: %s"
                 choice
                 (file-name-nondirectory mic)
                 (file-name-nondirectory monitor))))))

(defun cj/recording-test-mic ()
  "Test microphone by recording 5 seconds and playing it back.
Records from configured mic device, saves to temp file, plays back.
Useful for verifying mic hardware works before important recordings."
  (interactive)
  (unless cj/recording-mic-device
    (user-error "No microphone configured. Run C-; r c first"))

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
Records from configured monitor device (system audio output).
Play some audio/video during test. Useful for verifying you can capture
conference call audio, YouTube, etc."
  (interactive)
  (unless cj/recording-system-device
    (user-error "No system monitor configured. Run C-; r c first"))

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
  "Test both mic and monitor together with guided prompts.
This simulates a real recording scenario:
1. Tests mic only (speak into it)
2. Tests monitor only (play audio/video)
3. Tests both together (speak while audio plays)

Run this before important recordings to verify everything works!"
  (interactive)
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Devices not configured. Run C-; r c first"))

  (when (y-or-n-p "Test 1: Record from MICROPHONE only (5 sec). Ready? ")
    (cj/recording-test-mic)
    (sit-for 6))  ; Wait for playback

  (when (y-or-n-p "Test 2: Record from SYSTEM AUDIO only (5 sec). Start playing audio/video, then press y: ")
    (cj/recording-test-monitor)
    (sit-for 6))  ; Wait for playback

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

(defun cj/recording-get-devices ()
  "Get audio devices, prompting user if not already configured.
Returns (mic-device . system-device) or nil on error."
  ;; If devices not set, prompt user to select them
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (if (y-or-n-p "Audio devices not configured. Use quick setup for calls? ")
        (cj/recording-quick-setup-for-calls)
      (cj/recording-select-devices)))

  ;; Final validation
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Audio devices not configured.  Run C-; r c (quick setup) or C-; r s (manual select)"))

  (cons cj/recording-mic-device cj/recording-system-device))

(defun cj/video-recording-toggle (arg)
  "Toggle video recording: start if not recording, stop if recording.
On first use (or when devices not configured), runs quick setup (C-; r c).
With prefix ARG, prompt for recording location.
Otherwise use the default location in `video-recordings-dir'."
  (interactive "P")
  (if cj/video-recording-ffmpeg-process
      ;; Recording in progress - stop it
      (cj/video-recording-stop)
    ;; Not recording - start it
    (let* ((location (if arg
                         (read-directory-name "Enter recording location: ")
                       video-recordings-dir))
           (directory (file-name-directory location)))
      (unless (file-directory-p directory)
        (make-directory directory t))
      (cj/ffmpeg-record-video location))))

(defun cj/audio-recording-toggle (arg)
  "Toggle audio recording: start if not recording, stop if recording.
On first use (or when devices not configured), runs quick setup (C-; r c).
With prefix ARG, prompt for recording location.
Otherwise use the default location in `audio-recordings-dir'."
  (interactive "P")
  (if cj/audio-recording-ffmpeg-process
      ;; Recording in progress - stop it
      (cj/audio-recording-stop)
    ;; Not recording - start it
    (let* ((location (if arg
                         (read-directory-name "Enter recording location: ")
                       audio-recordings-dir))
           (directory (file-name-directory location)))
      (unless (file-directory-p directory)
        (make-directory directory t))
      (cj/ffmpeg-record-audio location))))

(defun cj/ffmpeg-record-video (directory)
  "Start an ffmpeg video recording.  Save output to DIRECTORY."
  (cj/recording-check-ffmpeg)
  (unless cj/video-recording-ffmpeg-process
	(let* ((devices (cj/recording-get-devices))
		   (mic-device (car devices))
		   (system-device (cdr devices))
		   (location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (expand-file-name (concat name ".mkv") location))
		   (ffmpeg-command
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
					filename)))
	  ;; start the recording
	  (setq cj/video-recording-ffmpeg-process
			(start-process-shell-command "ffmpeg-video-recording"
										 "*ffmpeg-video-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/video-recording-ffmpeg-process nil)
	  (set-process-sentinel cj/video-recording-ffmpeg-process #'cj/recording-process-sentinel)
	  (force-mode-line-update t)
	  (message "Started video recording to %s (mic: %.1fx, system: %.1fx)."
			   filename cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/ffmpeg-record-audio (directory)
  "Start an ffmpeg audio recording.  Save output to DIRECTORY.
Records from microphone and system audio monitor (configured device), mixing them together.
Use C-; r c to configure which device to use - it must match the device your phone call uses."
  (cj/recording-check-ffmpeg)
  (unless cj/audio-recording-ffmpeg-process
	(let* ((devices (cj/recording-get-devices))
		   (mic-device (car devices))
		   ;; Use the explicitly configured monitor device
		   ;; This must match the device your phone call/audio is using
		   (system-device (cdr devices))
		   (location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (expand-file-name (concat name ".m4a") location))
		   (ffmpeg-command
			(format (concat "ffmpeg "
							"-f pulse -i %s "  ; Input 0: Microphone (specific device)
							"-f pulse -i %s "  ; Input 1: System audio monitor
							"-filter_complex \""
							"[0:a]volume=%.1f[mic];"     ; Apply mic boost
							"[1:a]volume=%.1f[sys];"     ; Apply system volume
							"[mic][sys]amix=inputs=2:duration=longest[out]\" "  ; Mix both inputs
							"-map \"[out]\" "
							"-c:a aac "
							"-b:a 64k "
							"%s")
					mic-device
					system-device
					cj/recording-mic-boost
					cj/recording-system-volume
					filename)))
	  ;; Log the command for debugging
	  (message "Recording from mic: %s + ALL system outputs" mic-device)
	  (cj/log-silently "Audio recording ffmpeg command: %s" ffmpeg-command)
	  ;; start the recording
	  (setq cj/audio-recording-ffmpeg-process
			(start-process-shell-command "ffmpeg-audio-recording"
										 "*ffmpeg-audio-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/audio-recording-ffmpeg-process nil)
	  (set-process-sentinel cj/audio-recording-ffmpeg-process #'cj/recording-process-sentinel)
	  (force-mode-line-update t)
	  (message "Started recording to %s (mic: %.1fx, all system audio: %.1fx)"
			   filename cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/video-recording-stop ()
  "Stop the ffmpeg video recording process."
  (interactive)
  (if cj/video-recording-ffmpeg-process
	  (progn
		;; Use interrupt-process to send SIGINT (graceful termination)
		(interrupt-process cj/video-recording-ffmpeg-process)
		;; Give ffmpeg a moment to finalize the file
		(sit-for 0.2)
		(setq cj/video-recording-ffmpeg-process nil)
		(force-mode-line-update t)
		(message "Stopped video recording."))
	(message "No video recording in progress.")))

(defun cj/audio-recording-stop ()
  "Stop the ffmpeg audio recording process."
  (interactive)
  (if cj/audio-recording-ffmpeg-process
	  (progn
		;; Use interrupt-process to send SIGINT (graceful termination)
		(interrupt-process cj/audio-recording-ffmpeg-process)
		;; Give ffmpeg a moment to finalize the file
		(sit-for 0.2)
		(setq cj/audio-recording-ffmpeg-process nil)
		(force-mode-line-update t)
		(message "Stopped audio recording."))
	(message "No audio recording in progress.")))

(defun cj/recording-adjust-volumes ()
  "Interactively adjust recording volume levels."
  (interactive)
  (let ((mic (read-number "Microphone boost (1.0 = normal, 2.0 = double): "
						  cj/recording-mic-boost))
		(sys (read-number "System audio level (1.0 = normal, 0.5 = half): "
						  cj/recording-system-volume)))
	(setq cj/recording-mic-boost mic)
	(setq cj/recording-system-volume sys)
	(message "Recording levels updated - Mic: %.1fx, System: %.1fx" mic sys)))

;; Recording operations prefix and keymap
(defvar cj/record-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'cj/video-recording-toggle)
    (define-key map (kbd "a") #'cj/audio-recording-toggle)
    (define-key map (kbd "l") #'cj/recording-adjust-volumes)
    (define-key map (kbd "d") #'cj/recording-list-devices)
    (define-key map (kbd "w") #'cj/recording-show-active-audio)  ; "w" for "what's playing"
    (define-key map (kbd "s") #'cj/recording-select-devices)
    (define-key map (kbd "c") #'cj/recording-quick-setup-for-calls)
    (define-key map (kbd "t m") #'cj/recording-test-mic)
    (define-key map (kbd "t s") #'cj/recording-test-monitor)
    (define-key map (kbd "t b") #'cj/recording-test-both)
    map)
  "Keymap for video/audio recording operations.")

;; Only set keybinding if cj/custom-keymap is bound (not in batch mode)
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
    "C-; r s" "select devices"
    "C-; r c" "quick setup for calls"
    "C-; r t" "test devices"
    "C-; r t m" "test microphone"
    "C-; r t s" "test system audio"
    "C-; r t b" "test both (guided)"))

(provide 'video-audio-recording)
;;; video-audio-recording.el ends here.
