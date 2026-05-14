;;; test-transcription-status-and-commands.el --- Tests for transcription status helpers + commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the audio-file predicate, paths, duration, log
;; cleanup, sentinel helpers, process-environment builder, etc.  This
;; file fills in the status/list helpers and the interactive command
;; wrappers:
;;
;;   cj/--running-transcriptions
;;   cj/--cleanup-completed-transcriptions
;;   cj/--count-active-transcriptions
;;   cj/--transcription-modeline-string
;;   cj/transcriptions-buffer
;;   cj/transcription-kill
;;   cj/transcription-switch-backend
;;   cj/transcribe-audio-at-point
;;
;; Process and dired primitives are stubbed so the tests don't touch
;; real subprocesses or buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'transcription-config)

(defmacro test-tx--with-list (entries &rest body)
  "Bind `cj/transcriptions-list' to ENTRIES while evaluating BODY."
  (declare (indent 1))
  `(let ((cj/transcriptions-list ,entries))
     ,@body))

;;; cj/--running-transcriptions

(ert-deftest test-tx-running-transcriptions-filters-by-status ()
  "Normal: only entries with status `running' come back."
  (test-tx--with-list
      '((proc1 "/a/one.wav" 0 running)
        (proc2 "/a/two.wav" 0 complete)
        (proc3 "/a/three.wav" 0 error)
        (proc4 "/a/four.wav" 0 running))
    (let ((running (cj/--running-transcriptions)))
      (should (= (length running) 2))
      (should (memq 'proc1 (mapcar #'car running)))
      (should (memq 'proc4 (mapcar #'car running))))))

(ert-deftest test-tx-running-transcriptions-empty-list ()
  "Boundary: empty input -> empty output."
  (test-tx--with-list nil
    (should (null (cj/--running-transcriptions)))))

;;; cj/--cleanup-completed-transcriptions

(ert-deftest test-tx-cleanup-completed-mutates-list-to-running-only ()
  "Normal: cleanup retains running entries, drops completed/errored."
  (let ((cj/transcriptions-list
         '((p1 "/a/one.wav" 0 running)
           (p2 "/a/two.wav" 0 complete)
           (p3 "/a/three.wav" 0 error))))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (cj/--cleanup-completed-transcriptions))
    (should (equal cj/transcriptions-list
                   '((p1 "/a/one.wav" 0 running))))))

;;; cj/--count-active-transcriptions

(ert-deftest test-tx-count-active-zero-when-empty ()
  "Boundary: no entries -> zero count."
  (test-tx--with-list nil
    (should (= (cj/--count-active-transcriptions) 0))))

(ert-deftest test-tx-count-active-counts-only-running ()
  "Normal: counts running entries only."
  (test-tx--with-list
      '((p1 "/a/one.wav" 0 running)
        (p2 "/a/two.wav" 0 running)
        (p3 "/a/three.wav" 0 complete))
    (should (= (cj/--count-active-transcriptions) 2))))

;;; cj/--transcription-modeline-string

(ert-deftest test-tx-modeline-string-nil-when-no-active ()
  "Boundary: no active transcriptions -> nil modeline string."
  (test-tx--with-list nil
    (should (null (cj/--transcription-modeline-string)))))

(ert-deftest test-tx-modeline-string-includes-count-when-active ()
  "Normal: with N running transcriptions, the string shows N."
  (test-tx--with-list
      '((p1 "/a/one.wav" 0 running)
        (p2 "/a/two.wav" 0 running))
    (let ((s (cj/--transcription-modeline-string)))
      (should (stringp s))
      (should (string-match-p "2" s)))))

;;; cj/transcriptions-buffer

(ert-deftest test-tx-transcriptions-buffer-empty-list-renders-empty-message ()
  "Normal: with no entries, the buffer says \"No active transcriptions.\""
  (test-tx--with-list nil
    (cl-letf (((symbol-function 'display-buffer) (lambda (_ &rest _r) nil)))
      (cj/transcriptions-buffer))
    (let ((buf (get-buffer "*Transcriptions*")))
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p "No active transcriptions"
                                    (buffer-string))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest test-tx-transcriptions-buffer-lists-active-entries ()
  "Normal: with entries the buffer lists each by filename."
  (test-tx--with-list
      '((p1 "/a/foo.wav" 0 running)
        (p2 "/a/bar.wav" 0 complete))
    (cl-letf (((symbol-function 'display-buffer) (lambda (_ &rest _r) nil)))
      (cj/transcriptions-buffer))
    (let ((buf (get-buffer "*Transcriptions*")))
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p "foo.wav" (buffer-string)))
            (should (string-match-p "bar.wav" (buffer-string))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; cj/transcription-kill

(ert-deftest test-tx-transcription-kill-no-entries-errors ()
  "Error: with no transcriptions, kill signals user-error."
  (test-tx--with-list nil
    (should-error (call-interactively #'cj/transcription-kill)
                  :type 'user-error)))

(ert-deftest test-tx-transcription-kill-live-process-calls-kill-process ()
  "Normal: a live process gets `kill-process'd; user sees a message."
  (let ((killed nil)
        (msg nil))
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_) t))
              ((symbol-function 'kill-process)
               (lambda (p) (setq killed p)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/transcription-kill 'fake-process))
    (should (eq killed 'fake-process))
    (should (string-match-p "Killed" msg))))

;;; cj/transcription-switch-backend

(ert-deftest test-tx-switch-backend-sets-and-messages ()
  "Normal: completing-read picks one of the backends; the var changes."
  (let ((cj/transcribe-backend 'assemblyai)
        (msg nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "openai-api"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/transcription-switch-backend))
    (should (eq cj/transcribe-backend 'openai-api))
    (should (string-match-p "openai-api" msg))))

;;; cj/transcribe-audio-at-point

(ert-deftest test-tx-transcribe-audio-at-point-error-not-in-dired ()
  "Error: outside dired-mode the at-point wrapper signals user-error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (not (memq 'dired-mode modes)))))
      (should-error (cj/transcribe-audio-at-point) :type 'user-error))))

(ert-deftest test-tx-transcribe-audio-at-point-error-no-file ()
  "Error: no file at point signals user-error."
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (&rest modes) (memq 'dired-mode modes)))
            ((symbol-function 'dired-get-filename)
             (lambda (&rest _) nil)))
    (should-error (cj/transcribe-audio-at-point) :type 'user-error)))

(ert-deftest test-tx-transcribe-audio-at-point-normal-delegates ()
  "Normal: with a file at point, delegates to `cj/transcribe-audio'."
  (let ((handed-off nil))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'dired-mode modes)))
              ((symbol-function 'dired-get-filename)
               (lambda (&rest _) "/tmp/recording.wav"))
              ((symbol-function 'cj/transcribe-audio)
               (lambda (f) (setq handed-off f))))
      (cj/transcribe-audio-at-point))
    (should (equal handed-off "/tmp/recording.wav"))))

(provide 'test-transcription-status-and-commands)
;;; test-transcription-status-and-commands.el ends here
