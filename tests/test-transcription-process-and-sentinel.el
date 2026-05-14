;;; test-transcription-process-and-sentinel.el --- Tests for transcription start/notify/sentinel -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling files cover sentinel helpers (write-transcript, append-to-log,
;; update-status, notify-completion) and start helpers (init-log-file,
;; track-transcription).  This file covers the three remaining bodies:
;;
;;   cj/--notify
;;   cj/--start-transcription-process
;;   cj/--transcription-sentinel

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'transcription-config)

(defvar cj/transcriptions-list nil)

;;; cj/--notify

(ert-deftest test-tx-notify-always-messages ()
  "Normal: notify echoes through `message' even without DISPLAY."
  (let (msg)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args))))
              ((symbol-function 'getenv) (lambda (_) nil)))
      (cj/--notify "Transcription" "started"))
    (should (equal msg "Transcription: started"))))

(ert-deftest test-tx-notify-sends-desktop-when-display-set ()
  "Normal: with DISPLAY set, notify calls `notifications-notify' with
the title, body, and urgency."
  (let (notify-kwargs)
    (cl-letf (((symbol-function 'message) #'ignore)
              ((symbol-function 'getenv)
               (lambda (var) (and (equal var "DISPLAY") ":0")))
              ((symbol-function 'notifications-notify)
               (lambda (&rest kwargs) (setq notify-kwargs kwargs))))
      (cj/--notify "Transcription" "done" 'critical))
    (should (equal (plist-get notify-kwargs :title) "Transcription"))
    (should (equal (plist-get notify-kwargs :body) "done"))
    (should (eq (plist-get notify-kwargs :urgency) 'critical))))

;;; cj/--start-transcription-process

(ert-deftest test-tx-start-process-errors-when-audio-missing ()
  "Error: missing audio file signals user-error."
  (should-error (cj/--start-transcription-process "/no/such/file.mp3")
                :type 'user-error))

(ert-deftest test-tx-start-process-errors-when-not-audio ()
  "Error: a real file with a non-audio extension signals user-error."
  (let ((notes (make-temp-file "cj-tx-notes-" nil ".txt")))
    (unwind-protect
        (should-error (cj/--start-transcription-process notes)
                      :type 'user-error)
      (delete-file notes))))

(ert-deftest test-tx-start-process-errors-when-script-not-executable ()
  "Error: a non-executable script path signals user-error."
  (let ((audio (make-temp-file "cj-tx-audio-" nil ".mp3")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--transcription-script-path)
                   (lambda () "/no/such/script.sh")))
          (should-error (cj/--start-transcription-process audio)
                        :type 'user-error))
      (delete-file audio))))

(ert-deftest test-tx-start-process-spawns-make-process ()
  "Normal: with audio + executable script, make-process is invoked with
the script and the audio path."
  (let* ((audio (make-temp-file "cj-tx-audio-" nil ".mp3"))
         (script (make-temp-file "cj-tx-script-"))
         (cj/transcriptions-list nil)
         make-process-args)
    (set-file-modes script #o755)
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--transcription-script-path)
                   (lambda () script))
                  ((symbol-function 'cj/--init-log-file) #'ignore)
                  ((symbol-function 'cj/--build-process-environment)
                   (lambda (_) '("FOO=bar")))
                  ((symbol-function 'make-process)
                   (lambda (&rest kwargs)
                     (setq make-process-args kwargs)
                     'fake-process))
                  ((symbol-function 'cj/--notify) #'ignore)
                  ((symbol-function 'force-mode-line-update) #'ignore))
          (cj/--start-transcription-process audio))
      (delete-file audio)
      (delete-file script))
    (should make-process-args)
    (should (equal (plist-get make-process-args :command)
                   (list script audio)))))

;;; cj/--transcription-sentinel

(ert-deftest test-tx-sentinel-success-writes-transcript-and-updates-status ()
  "Normal: a finished event with exit 0 writes the transcript, updates
the entry status to `complete', and fires a normal-urgency notification."
  (let* ((txt-file (make-temp-file "cj-tx-txt-" nil ".txt"))
         (log-file (make-temp-file "cj-tx-log-" nil ".log"))
         (process-buffer (generate-new-buffer " *cj-tx-test*"))
         (proc (list 'mock-process))
         (cj/transcriptions-list (list (list proc "/tmp/audio.mp3"
                                             (current-time) 'running)))
         notify-urgency)
    (with-current-buffer process-buffer (insert "transcript contents"))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer)
                   (lambda (_) process-buffer))
                  ((symbol-function 'process-exit-status) (lambda (_) 0))
                  ((symbol-function 'cj/--should-keep-log)
                   (lambda (_) t))
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'force-mode-line-update) #'ignore)
                  ((symbol-function 'cj/--notify)
                   (lambda (_t _m &optional u) (setq notify-urgency u))))
          (cj/--transcription-sentinel proc "finished\n"
                                        "/tmp/audio.mp3"
                                        txt-file log-file))
      (when (buffer-live-p process-buffer) (kill-buffer process-buffer))
      (delete-file txt-file)
      (delete-file log-file))
    ;; success notification uses default (nil/normal) urgency.
    (should-not notify-urgency)
    ;; entry status updated to complete.
    (let ((entry (car cj/transcriptions-list)))
      (should (eq (nth 3 entry) 'complete)))))

(ert-deftest test-tx-sentinel-failure-marks-error-and-uses-critical-urgency ()
  "Normal: a non-zero exit fires the critical-urgency notification and
marks the entry as `error'."
  (let* ((txt-file (make-temp-file "cj-tx-txt-" nil ".txt"))
         (log-file (make-temp-file "cj-tx-log-" nil ".log"))
         (process-buffer (generate-new-buffer " *cj-tx-fail*"))
         (proc (list 'mock-fail))
         (cj/transcriptions-list (list (list proc "/tmp/audio.mp3"
                                             (current-time) 'running)))
         notify-urgency)
    (with-current-buffer process-buffer (insert "stderr blob"))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer)
                   (lambda (_) process-buffer))
                  ((symbol-function 'process-exit-status) (lambda (_) 1))
                  ((symbol-function 'cj/--should-keep-log) (lambda (_) t))
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'force-mode-line-update) #'ignore)
                  ((symbol-function 'cj/--notify)
                   (lambda (_t _m &optional u) (setq notify-urgency u))))
          (cj/--transcription-sentinel proc "exited abnormally\n"
                                        "/tmp/audio.mp3"
                                        txt-file log-file))
      (when (buffer-live-p process-buffer) (kill-buffer process-buffer))
      (delete-file txt-file)
      (delete-file log-file))
    (should (eq notify-urgency 'critical))
    (let ((entry (car cj/transcriptions-list)))
      (should (eq (nth 3 entry) 'error)))))

(provide 'test-transcription-process-and-sentinel)
;;; test-transcription-process-and-sentinel.el ends here
