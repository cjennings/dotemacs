;;; transcription-config.el --- Audio transcription workflow -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-04

;;; Commentary:
;;
;; Audio transcription workflow using OpenAI Whisper (API or local).
;;
;; USAGE:
;;   In dired: Press `T` on an audio file to transcribe
;;   Anywhere: M-x cj/transcribe-audio
;;   View active: M-x cj/transcriptions-buffer
;;
;; OUTPUT FILES:
;;   audio.m4a → audio.txt (transcript)
;;            → audio.log (process logs, conditionally kept)
;;
;; BACKENDS:
;;   - 'openai-api: Fast cloud transcription (requires OPENAI_API_KEY)
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

;; ----------------------------- Configuration ---------------------------------

(defvar cj/transcribe-backend 'local-whisper
  "Transcription backend to use.
- `openai-api': Fast cloud transcription via OpenAI API
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

;; ----------------------------- Pure Functions --------------------------------

(defun cj/--audio-file-p (file)
  "Return non-nil if FILE is an audio file based on extension."
  (when (and file (stringp file))
    (when-let ((ext (file-name-extension file)))
      (member (downcase ext) cj/audio-file-extensions))))

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
  "Return absolute path to transcription script based on backend."
  (let ((script-name (pcase cj/transcribe-backend
                       ('openai-api "oai-transcribe")
                       ('local-whisper "local-whisper"))))
    (expand-file-name (concat "scripts/" script-name) user-emacs-directory)))

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

(defun cj/--start-transcription-process (audio-file)
  "Start async transcription process for AUDIO-FILE.
Returns the process object."
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

    ;; Create log file
    (with-temp-file log-file
      (insert (format "Transcription started: %s\n" (current-time-string))
              (format "Backend: %s\n" cj/transcribe-backend)
              (format "Audio file: %s\n" audio-file)
              (format "Script: %s\n\n" script)))

    ;; Start process
    (let ((process (make-process
                    :name process-name
                    :buffer (get-buffer-create buffer-name)
                    :command (list script audio-file)
                    :sentinel (lambda (proc event)
                                (cj/--transcription-sentinel proc event audio-file txt-file log-file))
                    :stderr log-file)))

      ;; Track transcription
      (push (list process audio-file (current-time) 'running) cj/transcriptions-list)
      (force-mode-line-update t)

      ;; Notify user
      (cj/--notify "Transcription"
                   (format "Started on %s" (file-name-nondirectory audio-file)))

      process)))

(defun cj/--transcription-sentinel (process event audio-file txt-file log-file)
  "Sentinel for transcription PROCESS.
EVENT is the process event string.
AUDIO-FILE, TXT-FILE, and LOG-FILE are the associated files."
  (let* ((success-p (and (string-match-p "finished" event)
                         (= 0 (process-exit-status process))))
         (process-buffer (process-buffer process))
         (entry (assq process cj/transcriptions-list)))

    ;; Write process output to txt file
    (when (and success-p (buffer-live-p process-buffer))
      (with-current-buffer process-buffer
        (write-region (point-min) (point-max) txt-file nil 'silent)))

    ;; Append process output to log file
    (when (buffer-live-p process-buffer)
      (with-temp-buffer
        (insert-file-contents log-file)
        (goto-char (point-max))
        (insert "\n" (format-time-string "[%Y-%m-%d %H:%M:%S] ") event "\n")
        (insert-buffer-substring process-buffer)
        (write-region (point-min) (point-max) log-file nil 'silent)))

    ;; Update transcription status
    (when entry
      (setf (nth 3 entry) (if success-p 'complete 'error)))

    ;; Cleanup log file if successful and configured to do so
    (when (and success-p (not (cj/--should-keep-log t)))
      (delete-file log-file))

    ;; Kill process buffer
    (when (buffer-live-p process-buffer)
      (kill-buffer process-buffer))

    ;; Notify user
    (if success-p
        (cj/--notify "Transcription"
                     (format "Complete.  Transcript in %s" (file-name-nondirectory txt-file)))
      (cj/--notify "Transcription"
                   (format "Errored.  Logs in %s" (file-name-nondirectory log-file))
                   'critical))

    ;; Clean up completed transcriptions after 10 minutes
    (run-at-time 600 nil #'cj/--cleanup-completed-transcriptions)

    ;; Update modeline
    (force-mode-line-update t)))

(defun cj/--cleanup-completed-transcriptions ()
  "Remove completed/errored transcriptions from tracking list."
  (setq cj/transcriptions-list
        (seq-filter (lambda (entry)
                      (eq (nth 3 entry) 'running))
                    cj/transcriptions-list))
  (force-mode-line-update t))

(defun cj/--count-active-transcriptions ()
  "Return count of running transcriptions."
  (length (seq-filter (lambda (entry)
                        (eq (nth 3 entry) 'running))
                      cj/transcriptions-list)))

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
(defun cj/transcribe-audio (audio-file)
  "Transcribe AUDIO-FILE asynchronously.
Creates AUDIO.txt with transcript and AUDIO.log with process logs.
Uses backend specified by `cj/transcribe-backend'."
  (interactive (list (read-file-name "Audio file to transcribe: "
                                      nil nil t nil
                                      #'cj/--audio-file-p)))
  (cj/--start-transcription-process (expand-file-name audio-file)))

;;;###autoload
(defun cj/transcribe-audio-at-point ()
  "Transcribe audio file at point in dired."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in dired-mode"))
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file at point"))
    (cj/transcribe-audio file)))

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
            (let* ((process (nth 0 entry))
                   (audio-file (nth 1 entry))
                   (start-time (nth 2 entry))
                   (status (nth 3 entry))
                   (duration (cj/--transcription-duration start-time))
                   (status-face (pcase status
                                  ('running 'warning)
                                  ('complete 'success)
                                  ('error 'error))))
              (insert (propertize (format "%-10s" status) 'face status-face)
                      " "
                      (file-name-nondirectory audio-file)
                      (format " (%s)\n" duration))))))
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

;; ------------------------------- Dired Integration ---------------------------

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "T") #'cj/transcribe-audio-at-point))

;; Dirvish inherits dired-mode-map, so T works automatically

;; ------------------------------- Global Keybindings --------------------------

;; Transcription keymap
(defvar-keymap cj/transcribe-map
  :doc "Keymap for transcription operations"
  "t" #'cj/transcribe-audio
  "b" #'cj/transcriptions-buffer
  "k" #'cj/transcription-kill)
(keymap-set cj/custom-keymap "t" cj/transcribe-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; t" "transcription menu"
    "C-; t t" "transcribe audio"
    "C-; t b" "show transcriptions buffer"
    "C-; t k" "kill transcription"))

(provide 'transcription-config)
;;; transcription-config.el ends here
