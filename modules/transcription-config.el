;;; transcription-config.el --- Audio transcription workflow -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-04

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; optional auth/process transcription workflow, a
;;   command-loaded deferral candidate.
;; Top-level side effects: one add-to-list.
;; Runtime requires: dired, notifications, system-lib, user-constants.
;; Direct test load: yes.
;;
;; Audio transcription workflow with multiple backend options.
;;
;; USAGE:
;;   In dired: Press `T` on an audio OR video file to transcribe
;;            (videos run through ffmpeg first to extract the audio)
;;   Anywhere: M-x cj/transcribe-media  (M-x cj/transcribe-audio still works)
;;   View active: M-x cj/transcriptions-buffer
;;   Switch backend: M-x cj/transcription-switch-backend
;;
;; OUTPUT FILES:
;;   audio.m4a → audio.txt (transcript)
;;            → audio.log (process logs, conditionally kept)
;;
;; BACKENDS:
;;   - 'openai-api: Fast cloud transcription
;;     API key retrieved from authinfo.gpg (machine api.openai.com)
;;   - 'assemblyai: Cloud transcription with speaker diarization
;;     API key retrieved from authinfo.gpg (machine api.assemblyai.com)
;;   - 'local-whisper: Local transcription (requires whisper installed)
;;
;; NOTIFICATIONS:
;;   - "Transcription started on <file>"
;;   - "Transcription complete. Transcript in <file.txt>"
;;   - "Transcription errored. Logs in <file.log>"
;;
;; MODELINE:
;;   Shows active transcription count: ⏺2
;;   Click to view *Transcriptions* buffer
;;
;;; Code:

(require 'dired)
(require 'notifications)
(require 'system-lib)      ; provides cj/auth-source-secret-value
(require 'user-constants)  ; For cj/audio-file-extensions

;; ----------------------------- Configuration ---------------------------------

(defvar cj/transcribe-backend 'assemblyai
  "Transcription backend to use.
- `openai-api': Fast cloud transcription via OpenAI API
- `assemblyai': Cloud transcription with speaker diarization via AssemblyAI
- `local-whisper': Local transcription using installed Whisper")

(defvar cj/transcription-keep-log-when-done nil
  "Whether to keep log files after successful transcription.
If nil, log files are deleted after successful completion.
If t, log files are always kept.
Log files are always kept on error regardless of this setting.")

(defvar cj/transcriptions-list '()
  "List of active transcriptions.
Each entry: (process audio-file start-time status)
Status: running, complete, error")

;; ---------------------------- Backend Descriptors ---------------------------

(defconst cj/--transcription-backends
  '((openai-api    :script "oai-transcribe"        :auth-host "api.openai.com"     :env-var "OPENAI_API_KEY")
    (assemblyai    :script "assemblyai-transcribe" :auth-host "api.assemblyai.com" :env-var "ASSEMBLYAI_API_KEY")
    (local-whisper :script "local-whisper"         :auth-host nil                  :env-var nil))
  "Per-backend descriptors. Each entry: (SYMBOL :script S :auth-host H :env-var V).
`:auth-host' and `:env-var' are nil for local backends that need no API key.")

(defun cj/--backend-plist (backend)
  "Return the descriptor plist for BACKEND, or signal if unknown."
  (or (alist-get backend cj/--transcription-backends)
      (user-error "Unknown transcription backend: %s" backend)))

;; ----------------------------- Pure Functions --------------------------------

(defun cj/--audio-file-p (file)
  "Return non-nil if FILE is an audio file based on extension."
  (when (and file (stringp file))
    (when-let ((ext (file-name-extension file)))
      (member (downcase ext) cj/audio-file-extensions))))

(defun cj/--video-file-p (file)
  "Return non-nil if FILE is a video file based on extension."
  (when (and file (stringp file))
    (when-let ((ext (file-name-extension file)))
      (member (downcase ext) cj/video-file-extensions))))

(defun cj/--media-file-p (file)
  "Return non-nil if FILE is an audio or video file."
  (or (cj/--audio-file-p file)
      (cj/--video-file-p file)))

(defun cj/--transcription-output-files (audio-file)
  "Return cons cell of (TXT-FILE . LOG-FILE) for AUDIO-FILE."
  (let ((base (file-name-sans-extension audio-file)))
    (cons (concat base ".txt")
          (concat base ".log"))))

(defun cj/--transcription-duration (start-time)
  "Return duration string (MM:SS) since START-TIME."
  (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (format "%02d:%02d" minutes seconds)))

(defun cj/--should-keep-log (success-p)
  "Return non-nil if log file should be kept.
SUCCESS-P indicates whether transcription succeeded."
  (or (not success-p)  ; Always keep on error
      cj/transcription-keep-log-when-done))

(defun cj/--transcription-script-path ()
  "Return absolute path to transcription script for the active backend."
  (let ((script-name (plist-get (cj/--backend-plist cj/transcribe-backend) :script)))
    (expand-file-name (concat "scripts/" script-name) user-emacs-directory)))

(defun cj/--auth-source-password (host)
  "Retrieve the auth-source secret for HOST from authinfo.gpg.
Expects an entry like: machine HOST login api password <key>.
Returns the password string, or nil if no matching entry exists."
  (cj/auth-source-secret-value host))

(defun cj/--build-process-environment (backend)
  "Return `process-environment' augmented with BACKEND's API-key env var.
If BACKEND needs no API key (no :auth-host in its descriptor), return
`process-environment' unchanged.  Signals `user-error' if BACKEND requires
a key but none is found in authinfo.gpg."
  (let* ((desc (cj/--backend-plist backend))
         (auth-host (plist-get desc :auth-host))
         (env-var (plist-get desc :env-var)))
    (if (and auth-host env-var)
        (if-let ((api-key (cj/--auth-source-password auth-host)))
            (cons (format "%s=%s" env-var api-key) process-environment)
          (user-error "API key not found in authinfo.gpg for host %s" auth-host))
      process-environment)))

(defun cj/--init-log-file (log-file audio-file script)
  "Create LOG-FILE with a header recording the start of transcription.
Records the current time, active backend, AUDIO-FILE, and SCRIPT path."
  (with-temp-file log-file
    (insert (format "Transcription started: %s\n" (current-time-string))
            (format "Backend: %s\n" cj/transcribe-backend)
            (format "Audio file: %s\n" audio-file)
            (format "Script: %s\n\n" script))))

(defun cj/--track-transcription (process audio-file)
  "Push a running-status entry for PROCESS and AUDIO-FILE, refresh modeline."
  (push (list process audio-file (current-time) 'running) cj/transcriptions-list)
  (force-mode-line-update t))

;; ---------------------------- Process Management -----------------------------

(defun cj/--notify (title message &optional urgency)
  "Send desktop notification and echo area message.
TITLE and MESSAGE are strings.  URGENCY is normal or critical."
  (message "%s: %s" title message)
  (when (and (fboundp 'notifications-notify)
             (getenv "DISPLAY"))
    (notifications-notify
     :title title
     :body message
     :urgency (or urgency 'normal))))

(defun cj/--start-transcription-process (audio-file &optional cleanup-file)
  "Start async transcription process for AUDIO-FILE.
Returns the process object.

When CLEANUP-FILE is non-nil, delete that path once the transcription
sentinel fires (success or failure).  Used by the video flow to drop
the temp audio file produced by ffmpeg after transcription completes."
  (unless (file-exists-p audio-file)
    (user-error "Audio file does not exist: %s" audio-file))

  (unless (cj/--audio-file-p audio-file)
    (user-error "Not an audio file: %s" audio-file))

  (let* ((script (cj/--transcription-script-path))
         (outputs (cj/--transcription-output-files audio-file))
         (txt-file (car outputs))
         (log-file (cdr outputs))
         (buffer-name (format " *transcribe-%s*" (file-name-nondirectory audio-file)))
         (process-name (format "transcribe-%s" (file-name-nondirectory audio-file))))

    (unless (file-executable-p script)
      (user-error "Transcription script not found or not executable: %s" script))

    (cj/--init-log-file log-file audio-file script)

    (let* ((process-environment (cj/--build-process-environment cj/transcribe-backend))
           (process (make-process
                     :name process-name
                     :buffer (get-buffer-create buffer-name)
                     :command (list script audio-file)
                     :sentinel (lambda (proc event)
                                 (cj/--transcription-sentinel proc event audio-file txt-file log-file)
                                 (when cleanup-file
                                   (ignore-errors (delete-file cleanup-file))))
                     :stderr log-file)))
      (cj/--track-transcription process audio-file)
      (cj/--notify "Transcription"
                   (format "Started on %s" (file-name-nondirectory audio-file)))
      process)))

(defun cj/--video-extracted-audio-path (video-file)
  "Return a temp .mp3 path to hold the extracted audio for VIDEO-FILE.
The basename hints at the source so a stuck file is easy to identify."
  (make-temp-file (format "cj-tx-%s-"
                          (file-name-base video-file))
                  nil ".mp3"))

(defun cj/--extract-audio-from-video (video-file output-file on-success)
  "Async-extract the audio track from VIDEO-FILE to OUTPUT-FILE via ffmpeg.

On success, call ON-SUCCESS (no args).  On failure, signal a
descriptive `user-error' via `cj/--notify'.  Signals `user-error'
synchronously if ffmpeg isn't on PATH.

Uses libmp3lame at quality 4 (~165kbps VBR) -- good for speech,
universally accepted by the transcription backends."
  (let ((ffmpeg (cj/executable-find-or-warn
                 "ffmpeg" "video audio extraction" 'transcription-config)))
    (unless ffmpeg
      (user-error "ffmpeg not found on PATH -- install ffmpeg to transcribe videos"))
    (let ((process-name (format "ffmpeg-extract-%s"
                                (file-name-nondirectory video-file))))
      (make-process
       :name process-name
       :buffer (get-buffer-create (format " *%s*" process-name))
       :command (list ffmpeg "-y" "-i" video-file
                      "-vn" "-acodec" "libmp3lame" "-q:a" "4"
                      output-file)
       :sentinel (lambda (proc event)
                   (cond
                    ((and (string-match-p "finished" event)
                          (= 0 (process-exit-status proc)))
                     (let ((buf (process-buffer proc)))
                       (when (buffer-live-p buf) (kill-buffer buf)))
                     (funcall on-success))
                    ((string-match-p "\\(?:exited\\|failed\\|signal\\)" event)
                     (cj/--notify "Transcription"
                                  (format "ffmpeg failed on %s"
                                          (file-name-nondirectory video-file))
                                  'critical)
                     (ignore-errors (delete-file output-file)))))))))

(defun cj/--write-transcript-on-success (process-buffer success-p txt-file)
  "Write PROCESS-BUFFER contents to TXT-FILE when SUCCESS-P is non-nil.
No-op if PROCESS-BUFFER is dead or SUCCESS-P is nil."
  (when (and success-p (buffer-live-p process-buffer))
    (with-current-buffer process-buffer
      (write-region (point-min) (point-max) txt-file nil 'silent))))

(defun cj/--append-to-log (process-buffer log-file event)
  "Append an EVENT marker plus PROCESS-BUFFER contents to LOG-FILE.
No-op if PROCESS-BUFFER is dead."
  (when (buffer-live-p process-buffer)
    (with-temp-buffer
      (insert-file-contents log-file)
      (goto-char (point-max))
      (insert "\n" (format-time-string "[%Y-%m-%d %H:%M:%S] ") event "\n")
      (insert-buffer-substring process-buffer)
      (write-region (point-min) (point-max) log-file nil 'silent))))

(defun cj/--update-transcription-status (process success-p)
  "Mark PROCESS's entry as `complete' or `error' based on SUCCESS-P.
No-op if PROCESS isn't tracked."
  (when-let ((entry (assq process cj/transcriptions-list)))
    (setf (nth 3 entry) (if success-p 'complete 'error))))

(defun cj/--notify-completion (success-p txt-file log-file)
  "Send completion notification based on SUCCESS-P.
References TXT-FILE on success (normal urgency), LOG-FILE on failure
\(critical urgency)."
  (if success-p
      (cj/--notify "Transcription"
                   (format "Complete.  Transcript in %s" (file-name-nondirectory txt-file)))
    (cj/--notify "Transcription"
                 (format "Errored.  Logs in %s" (file-name-nondirectory log-file))
                 'critical)))

(defun cj/--transcription-sentinel (process event _audio-file txt-file log-file)
  "Sentinel for transcription PROCESS.
EVENT is the process event string.  TXT-FILE and LOG-FILE are the
associated output files."
  (let* ((success-p (and (string-match-p "finished" event)
                         (= 0 (process-exit-status process))))
         (process-buffer (process-buffer process)))
    (cj/--write-transcript-on-success process-buffer success-p txt-file)
    (cj/--append-to-log process-buffer log-file event)
    (cj/--update-transcription-status process success-p)
    (when (and success-p (not (cj/--should-keep-log success-p)))
      (delete-file log-file))
    (when (buffer-live-p process-buffer)
      (kill-buffer process-buffer))
    (cj/--notify-completion success-p txt-file log-file)
    (run-at-time 600 nil #'cj/--cleanup-completed-transcriptions)
    (force-mode-line-update t)))

(defun cj/--running-transcriptions ()
  "Return the subset of `cj/transcriptions-list' whose status is `running'."
  (seq-filter (lambda (entry) (eq (nth 3 entry) 'running))
              cj/transcriptions-list))

(defun cj/--cleanup-completed-transcriptions ()
  "Remove completed/errored transcriptions from tracking list."
  (setq cj/transcriptions-list (cj/--running-transcriptions))
  (force-mode-line-update t))

(defun cj/--count-active-transcriptions ()
  "Return count of running transcriptions."
  (length (cj/--running-transcriptions)))

;; ----------------------------- Modeline Integration --------------------------

(defun cj/--transcription-modeline-string ()
  "Return modeline string for active transcriptions."
  (let ((count (cj/--count-active-transcriptions)))
    (when (> count 0)
      (propertize (format " ⏺%d " count)
                  'face 'warning
                  'help-echo (format "%d active transcription%s (click to view)"
                                     count (if (= count 1) "" "s"))
                  'mouse-face 'mode-line-highlight
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                 #'cj/transcriptions-buffer)
                               map)))))

;; Add to mode-line-format (will be activated when module loads)
(add-to-list 'mode-line-misc-info
             '(:eval (cj/--transcription-modeline-string))
             t)

;; --------------------------- Interactive Commands ----------------------------

;;;###autoload
(defun cj/transcribe-media (file)
  "Transcribe FILE asynchronously.  Accepts audio or video.

For audio: hands the file straight to the transcription pipeline.
For video: shells ffmpeg to extract the audio track to a temp .mp3,
then transcribes that.  The temp audio is deleted after the
transcription sentinel fires.

Creates FILE.txt with the transcript (alongside the source) and
FILE.log with process logs.  Uses the backend in
`cj/transcribe-backend'."
  (interactive (list (read-file-name "Media file to transcribe: "
                                      nil nil t nil
                                      #'cj/--media-file-p)))
  (let ((path (expand-file-name file)))
    (unless (cj/--media-file-p path)
      (user-error "Not an audio or video file: %s" path))
    (cond
     ((cj/--audio-file-p path)
      (cj/--start-transcription-process path))
     ((cj/--video-file-p path)
      (let ((extracted (cj/--video-extracted-audio-path path)))
        (cj/--extract-audio-from-video
         path extracted
         (lambda ()
           (cj/--start-transcription-process extracted extracted))))))))

;;;###autoload
(defun cj/transcribe-media-at-point ()
  "Transcribe the audio or video file at point in dired/dirvish."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in dired-mode"))
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file at point"))
    (cj/transcribe-media file)))

;; Backwards-compat aliases.  The audio-only names predate the
;; video-extension work; keep them as drop-in synonyms for anyone
;; with muscle memory or external scripts.
(defalias 'cj/transcribe-audio 'cj/transcribe-media)
(defalias 'cj/transcribe-audio-at-point 'cj/transcribe-media-at-point)

(defun cj/--format-transcription-entry (entry)
  "Return a display string for a transcription ENTRY.
ENTRY is (PROCESS AUDIO-FILE START-TIME STATUS).  Status drives the face;
duration is computed from START-TIME."
  (let* ((audio-file (nth 1 entry))
         (start-time (nth 2 entry))
         (status (nth 3 entry))
         (duration (cj/--transcription-duration start-time))
         (status-face (pcase status
                        ('running 'warning)
                        ('complete 'success)
                        ('error 'error))))
    (concat (propertize (format "%-10s" status) 'face status-face)
            " "
            (file-name-nondirectory audio-file)
            (format " (%s)\n" duration))))

;;;###autoload
(defun cj/transcriptions-buffer ()
  "Show buffer with active transcriptions."
  (interactive)
  (let ((buffer (get-buffer-create "*Transcriptions*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Active Transcriptions\n" 'face 'bold)
                (propertize (make-string 50 ?─) 'face 'shadow)
                "\n\n")
        (if (null cj/transcriptions-list)
            (insert "No active transcriptions.\n")
          (dolist (entry cj/transcriptions-list)
            (insert (cj/--format-transcription-entry entry)))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

;;;###autoload
(defun cj/transcription-kill (process)
  "Kill transcription PROCESS."
  (interactive
   (list (let ((choices (mapcar (lambda (entry)
                                   (cons (file-name-nondirectory (nth 1 entry))
                                         (nth 0 entry)))
                                 cj/transcriptions-list)))
           (unless choices
             (user-error "No active transcriptions"))
           (cdr (assoc (completing-read "Kill transcription: " choices nil t)
                       choices)))))
  (when (process-live-p process)
    (kill-process process)
    (message "Killed transcription process")))

;;;###autoload
(defun cj/transcription-switch-backend ()
  "Switch transcription backend.
Prompts with completing-read to select from available backends."
  (interactive)
  (let* ((backends '(("assemblyai" . assemblyai)
                     ("openai-api" . openai-api)
                     ("local-whisper" . local-whisper)))
         (current (symbol-name cj/transcribe-backend))
         (prompt (format "Transcription backend (current: %s): " current))
         (choice (completing-read prompt backends nil t))
         (new-backend (alist-get choice backends nil nil #'string=)))
    (setq cj/transcribe-backend new-backend)
    (message "Transcription backend: %s" choice)))

;; ------------------------------- Dired Integration ---------------------------

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "T") #'cj/transcribe-media-at-point))

;; Dirvish uses its own keymap, so bind T there too
(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "T") #'cj/transcribe-media-at-point))

;; Reach the transcription commands via M-x.  The previous `C-; T'
;; menu was retired so the top-level slot could go to telega (which
;; finally has a clean mnemonic at `C-; T' once nothing else is
;; fighting over the same key).

(provide 'transcription-config)
;;; transcription-config.el ends here
