;;; record-desktop.el --- Video Record desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Use ffmpeg to video record your desktop.
;; with audio from mic and audio from default audio sink

;; Note: video-recordings-dir is defined (and directory created) in
;; user-cosntants.el

;;; Code:

(defvar cj/video-recording-ffmpeg-process nil
  "Variable to store the process of the ffmpeg recording.")

(defun cj/video-recording-start (arg)
  "Starts the ffmpeg recording.
If called with a prefix arg C-u, choose the location on where to save the
recording, otherwise use the default location in `video-recordings-dir'."
  (interactive "P")
  (let* ((location (if arg
					   (read-directory-name "Enter recording location: ")
					 video-recordings-dir))
		 (directory (file-name-directory location)))
	(unless (file-directory-p directory)
	  (make-directory directory t))
	(cj/ffmpeg-record location)))

(defun cj/ffmpeg-record (directory)
  "Start an ffmpeg recording. Save output to DIRECTORY."
  (unless cj/video-recording-ffmpeg-process
	(let* ((location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (concat location "/" name ".mkv"))
		   (ffmpeg-command
			(concat "ffmpeg -framerate 30 -f x11grab -i :0.0+ "
					"-f pulse -i "
					"alsa_input.pci-0000_00_1b.0.analog-stereo "
					"-ac 1 "
					"-f pulse -i "
					"alsa_output.pci-0000_00_1b.0.analog-stereo.monitor "
					"-ac 2 " filename)))
	  ;; start the recording
	  (setq cj/video-recording-ffmpeg-process
			(start-process-shell-command "ffmpeg-recording"
										 "*ffmpeg-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/video-recording-ffmpeg-process nil)
	  (message "Started recording process."))))

(defun cj/video-recording-stop ()
  "Stop the ffmpeg recording process."
  (interactive)
  (when cj/video-recording-ffmpeg-process
	(delete-process cj/video-recording-ffmpeg-process)
	(setq cj/video-recording-ffmpeg-process nil)
	(message "Stopped video recording.")))



(provide 'record-desktop)
;;; record-desktop.el ends here.
