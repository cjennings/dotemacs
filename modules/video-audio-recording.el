;;; video-audio-recording.el --- Video and Audio Recording -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Use ffmpeg to record desktop video or just audio.
;; with audio from mic and audio from default audio sink
;; Also supports audio-only recording in Opus format.
;;
;; Note: video-recordings-dir and audio-recordings-dir are defined
;; (and directory created) in user-constants.el
;;
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

;; Forward declarations
(eval-when-compile (defvar video-recordings-dir))
(eval-when-compile (defvar audio-recordings-dir))
(defvar cj/custom-keymap)

(defvar cj/recording-mic-boost 2.0
  "Volume multiplier for microphone in recordings.
1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB).")

(defvar cj/recording-system-volume 0.5
  "Volume multiplier for system audio in recordings.
1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB).")

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

(defun cj/recording-check-ffmpeg ()
  "Check if ffmpeg is available.
Return t if found, nil otherwise."
  (unless (executable-find "ffmpeg")
    (user-error "Ffmpeg not found.  Install with: sudo pacman -S ffmpeg")
    nil)
  t)

(defun cj/recording-detect-mic-device ()
  "Auto-detect PulseAudio microphone input device.
Returns device name or nil if not found."
  (let ((output (shell-command-to-string "pactl list sources short 2>/dev/null")))
    (when (string-match "\\([^\t\n]+\\).*analog.*stereo" output)
      (match-string 1 output))))

(defun cj/recording-detect-system-device ()
  "Auto-detect PulseAudio system audio monitor device.
Returns device name or nil if not found."
  (let ((output (shell-command-to-string "pactl list sources short 2>/dev/null")))
    (when (string-match "\\([^\t\n]+\\.monitor\\)" output)
      (match-string 1 output))))

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

(defun cj/recording-get-devices ()
  "Get or auto-detect audio devices.
Returns (mic-device . system-device) or nil on error."
  ;; Auto-detect if not already set
  (unless cj/recording-mic-device
    (setq cj/recording-mic-device (cj/recording-detect-mic-device)))
  (unless cj/recording-system-device
    (setq cj/recording-system-device (cj/recording-detect-system-device)))

  ;; If auto-detection failed, prompt user to select
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (when (y-or-n-p "Could not auto-detect audio devices.  Select manually? ")
      (cj/recording-select-devices)))

  ;; Final validation
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Audio devices not configured.  Run M-x cj/recording-select-devices"))

  (cons cj/recording-mic-device cj/recording-system-device))

(defun cj/video-recording-start (arg)
  "Start the ffmpeg video recording.
With prefix ARG, prompt for recording location.
Otherwise use the default location in `video-recordings-dir'."
  (interactive "P")
  (let* ((location (if arg
					   (read-directory-name "Enter recording location: ")
					 video-recordings-dir))
		 (directory (file-name-directory location)))
	(unless (file-directory-p directory)
	  (make-directory directory t))
	(cj/ffmpeg-record-video location)))

(defun cj/audio-recording-start (arg)
  "Start the ffmpeg audio recording.
With prefix ARG, prompt for recording location.
Otherwise use the default location in `audio-recordings-dir'."
  (interactive "P")
  (let* ((location (if arg
					   (read-directory-name "Enter recording location: ")
					 audio-recordings-dir))
		 (directory (file-name-directory location)))
	(unless (file-directory-p directory)
	  (make-directory directory t))
	(cj/ffmpeg-record-audio location)))

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
	  (message "Started video recording to %s (mic: %.1fx, system: %.1fx)."
			   filename cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/ffmpeg-record-audio (directory)
  "Start an ffmpeg audio recording.  Save output to DIRECTORY."
  (cj/recording-check-ffmpeg)
  (unless cj/audio-recording-ffmpeg-process
	(let* ((devices (cj/recording-get-devices))
		   (mic-device (car devices))
		   (system-device (cdr devices))
		   (location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (expand-file-name (concat name ".opus") location))
		   (ffmpeg-command
			(format (concat "ffmpeg "
							"-f pulse -i %s "
							"-ac 1 "
							"-f pulse -i %s "
							"-ac 2 "
							"-filter_complex \"[0:a]volume=%.1f[mic];[1:a]volume=%.1f[sys];[mic][sys]amerge=inputs=2\" "
							"-c:a libopus "
							"-b:a 96k "
							"%s")
					mic-device
					system-device
					cj/recording-mic-boost
					cj/recording-system-volume
					filename)))
	  ;; start the recording
	  (setq cj/audio-recording-ffmpeg-process
			(start-process-shell-command "ffmpeg-audio-recording"
										 "*ffmpeg-audio-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/audio-recording-ffmpeg-process nil)
	  (message "Started audio recording to %s (mic: %.1fx, system: %.1fx)."
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
    (define-key map (kbd "V") #'cj/video-recording-stop)
    (define-key map (kbd "v") #'cj/video-recording-start)
    (define-key map (kbd "A") #'cj/audio-recording-stop)
    (define-key map (kbd "a") #'cj/audio-recording-start)
    (define-key map (kbd "l") #'cj/recording-adjust-volumes)
    (define-key map (kbd "d") #'cj/recording-list-devices)
    (define-key map (kbd "s") #'cj/recording-select-devices)
    (define-key map (kbd "c") #'cj/recording-quick-setup-for-calls)
    map)
  "Keymap for video/audio recording operations.")

(keymap-set cj/custom-keymap "r" cj/record-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; r" "recording menu"
    "C-; r v" "start video"
    "C-; r V" "stop video"
    "C-; r a" "start audio"
    "C-; r A" "stop audio"
    "C-; r l" "adjust levels"
    "C-; r d" "list devices"
    "C-; r s" "select devices"
    "C-; r c" "quick setup for calls"))

(provide 'video-audio-recording)
;;; video-audio-recording.el ends here.
