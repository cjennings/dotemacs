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

(defun cj/recording-get-devices ()
  "Get or auto-detect audio devices.
Returns (mic-device . system-device) or nil on error."
  ;; Auto-detect if not already set
  (unless cj/recording-mic-device
    (setq cj/recording-mic-device (cj/recording-detect-mic-device)))
  (unless cj/recording-system-device
    (setq cj/recording-system-device (cj/recording-detect-system-device)))

  ;; Validate devices
  (unless (and cj/recording-mic-device cj/recording-system-device)
    (user-error "Could not detect audio devices. Set cj/recording-mic-device and cj/recording-system-device manually"))

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
  "Start an ffmpeg video recording. Save output to DIRECTORY."
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
  "Start an ffmpeg audio recording. Save output to DIRECTORY."
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
    "C-; r l" "adjust levels"))

(provide 'video-audio-recording)
;;; video-audio-recording.el ends here.
