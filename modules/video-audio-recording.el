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

(require 'user-constants)

(defgroup cj-recording nil
  "Settings for video and audio recording."
  :group 'multimedia)

(defcustom cj/recording-mic-boost 2.0
  "Volume multiplier for microphone in recordings.

1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB)."
  :type 'number
  :group 'cj-recording)

(defcustom cj/recording-system-volume 0.5
  "Volume multiplier for system audio in recordings.

1.0 = normal volume, 2.0 = double volume (+6dB), 0.5 = half volume (-6dB)."
  :type 'number
  :group 'cj-recording)

(defvar cj/video-recording-ffmpeg-process nil
  "Variable to store the process of the ffmpeg video recording.")

(defvar cj/audio-recording-ffmpeg-process nil
  "Variable to store the process of the ffmpeg audio recording.")

(defun cj/video-recording-start (arg)
  "Starts the ffmpeg video recording.

If called with a prefix arg C-u, choose the location on where to save the
recording, otherwise use the default location in =video-recordings-dir'."
  (interactive "P")
  (let* ((location (if arg
					   (read-directory-name "Enter recording location: ")
					 video-recordings-dir))
		 (directory (file-name-directory location)))
	(unless (file-directory-p directory)
	  (make-directory directory t))
	(cj/ffmpeg-record-video location)))

(defun cj/audio-recording-start (arg)
  "Starts the ffmpeg audio recording.

If called with a prefix arg C-u, choose the location on where to save the
recording, otherwise use the default location in =video-recordings-dir'."
  (interactive "P")
  (let* ((location (if arg
					   (read-directory-name "Enter recording location: ")
					 video-recordings-dir))
		 (directory (file-name-directory location)))
	(unless (file-directory-p directory)
	  (make-directory directory t))
	(cj/ffmpeg-record-audio location)))

(defun cj/ffmpeg-record-video (directory)
  "Start an ffmpeg video recording. Save output to DIRECTORY."
  (unless cj/video-recording-ffmpeg-process
	(let* ((location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (expand-file-name (concat name ".mkv") location))
		   (ffmpeg-command
			(format (concat "ffmpeg -framerate 30 -f x11grab -i :0.0+ "
							"-f pulse -i "
							"alsa_input.pci-0000_00_1b.0.analog-stereo "
							"-ac 1 "
							"-f pulse -i "
							"alsa_output.pci-0000_00_1b.0.analog-stereo.monitor "
							"-ac 2 "
							"-filter_complex \"[1:a]volume=%.1f[mic];[2:a]volume=%.1f[sys];[mic][sys]amerge=inputs=2[out]\" "
							"-map 0:v -map \"[out]\" "
							"%s")
					cj/recording-mic-boost
					cj/recording-system-volume
					filename)))
	  ;; start the recording
	  (setq cj/video-recording-ffmpeg-process
			(start-process-shell-command "ffmpeg-video-recording"
										 "*ffmpeg-video-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/video-recording-ffmpeg-process nil)
	  (message "Started video recording process (mic boost: %.1fx, system volume: %.1fx)."
			   cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/ffmpeg-record-audio (directory)
  "Start an ffmpeg audio recording. Save output to DIRECTORY."
  (unless cj/audio-recording-ffmpeg-process
	(let* ((location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (expand-file-name (concat name ".opus") location))
		   (ffmpeg-command
			(format (concat "ffmpeg "
							"-f pulse -i "
							"alsa_input.pci-0000_00_1b.0.analog-stereo "
							"-ac 1 "
							"-f pulse -i "
							"alsa_output.pci-0000_00_1b.0.analog-stereo.monitor "
							"-ac 2 "
							"-filter_complex \"[0:a]volume=%.1f[mic];[1:a]volume=%.1f[sys];[mic][sys]amerge=inputs=2\" "
							"-c:a libopus "
							"-b:a 96k "
							"%s")
					cj/recording-mic-boost
					cj/recording-system-volume
					filename)))
	  ;; start the recording
	  (setq cj/audio-recording-ffmpeg-process
			(start-process-shell-command "ffmpeg-audio-recording"
										 "*ffmpeg-audio-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/audio-recording-ffmpeg-process nil)
	  (message "Started audio recording process (mic boost: %.1fx, system volume: %.1fx)."
			   cj/recording-mic-boost cj/recording-system-volume))))

(defun cj/video-recording-stop ()
  "Stop the ffmpeg video recording process."
  (interactive)
  (if cj/video-recording-ffmpeg-process
	  (progn
		;; Use interrupt-process to send SIGINT (graceful termination)
		(interrupt-process cj/video-recording-ffmpeg-process)
		;; Give ffmpeg a moment to finalize the file
		(sit-for 1)
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
		(sit-for 1)
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
	(customize-set-variable 'cj/recording-mic-boost mic)
	(customize-set-variable 'cj/recording-system-volume sys)
	(message "Recording levels updated - Mic: %.1fx, System: %.1fx" mic sys)))

;; Recording operations prefix and keymap
(define-prefix-command 'cj/record-map nil
					   "Keymap for video/audio recording operations.")
(define-key cj/custom-keymap "r" 'cj/record-map)
(define-key cj/record-map "V" 'cj/video-recording-stop)
(define-key cj/record-map "v" 'cj/video-recording-start)
(define-key cj/record-map "A" 'cj/audio-recording-stop)
(define-key cj/record-map "a" 'cj/audio-recording-start)
(define-key cj/record-map "l" 'cj/recording-adjust-volumes)  ; 'l' for levels

(provide 'video-audio-recording)
;;; video-audio-recording.el ends here.
