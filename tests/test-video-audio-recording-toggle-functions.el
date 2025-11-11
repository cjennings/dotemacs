;;; test-video-audio-recording-toggle-functions.el --- Tests for toggle functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/video-recording-toggle and cj/audio-recording-toggle functions.
;; Tests start/stop toggle behavior for recording processes.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub directory variables
(defvar video-recordings-dir "/tmp/video-recordings/")
(defvar audio-recordings-dir "/tmp/audio-recordings/")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-toggle-setup ()
  "Reset process variables before each test."
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device "test-mic")
  (setq cj/recording-system-device "test-monitor"))

(defun test-toggle-teardown ()
  "Clean up process variables after each test."
  (when cj/video-recording-ffmpeg-process
    (ignore-errors (delete-process cj/video-recording-ffmpeg-process)))
  (when cj/audio-recording-ffmpeg-process
    (ignore-errors (delete-process cj/audio-recording-ffmpeg-process)))
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Video Toggle - Normal Cases

(ert-deftest test-video-audio-recording-video-toggle-normal-starts-when-not-recording ()
  "Test that video toggle starts recording when not currently recording."
  (test-toggle-setup)
  (unwind-protect
      (let ((start-called nil))
        (cl-letf (((symbol-function 'cj/ffmpeg-record-video)
                   (lambda (_dir) (setq start-called t))))
          (cj/video-recording-toggle nil)
          (should start-called)))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-video-toggle-normal-stops-when-recording ()
  "Test that video toggle stops recording when currently recording."
  (test-toggle-setup)
  (unwind-protect
      (let ((stop-called nil)
            (fake-process (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/video-recording-stop)
                   (lambda () (setq stop-called t))))
          (cj/video-recording-toggle nil)
          (should stop-called))
        (ignore-errors (delete-process fake-process)))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-video-toggle-normal-uses-default-directory ()
  "Test that video toggle uses default directory when no prefix arg."
  (test-toggle-setup)
  (unwind-protect
      (let ((recorded-dir nil))
        (cl-letf (((symbol-function 'cj/ffmpeg-record-video)
                   (lambda (dir) (setq recorded-dir dir))))
          (cj/video-recording-toggle nil)
          (should (equal video-recordings-dir recorded-dir))))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-video-toggle-normal-prompts-for-location-with-prefix ()
  "Test that video toggle prompts for location with prefix arg."
  (test-toggle-setup)
  (unwind-protect
      (let ((prompt-called nil)
            (recorded-dir nil))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (_prompt) (setq prompt-called t) "/custom/path/"))
                  ((symbol-function 'file-directory-p)
                   (lambda (_dir) t))  ; Directory exists
                  ((symbol-function 'cj/ffmpeg-record-video)
                   (lambda (dir) (setq recorded-dir dir))))
          (cj/video-recording-toggle t)
          (should prompt-called)
          (should (equal "/custom/path/" recorded-dir))))
    (test-toggle-teardown)))

;;; Audio Toggle - Normal Cases

(ert-deftest test-video-audio-recording-audio-toggle-normal-starts-when-not-recording ()
  "Test that audio toggle starts recording when not currently recording."
  (test-toggle-setup)
  (unwind-protect
      (let ((start-called nil))
        (cl-letf (((symbol-function 'cj/ffmpeg-record-audio)
                   (lambda (_dir) (setq start-called t))))
          (cj/audio-recording-toggle nil)
          (should start-called)))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-audio-toggle-normal-stops-when-recording ()
  "Test that audio toggle stops recording when currently recording."
  (test-toggle-setup)
  (unwind-protect
      (let ((stop-called nil)
            (fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/audio-recording-stop)
                   (lambda () (setq stop-called t))))
          (cj/audio-recording-toggle nil)
          (should stop-called))
        (ignore-errors (delete-process fake-process)))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-audio-toggle-normal-uses-default-directory ()
  "Test that audio toggle uses default directory when no prefix arg."
  (test-toggle-setup)
  (unwind-protect
      (let ((recorded-dir nil))
        (cl-letf (((symbol-function 'cj/ffmpeg-record-audio)
                   (lambda (dir) (setq recorded-dir dir))))
          (cj/audio-recording-toggle nil)
          (should (equal audio-recordings-dir recorded-dir))))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-audio-toggle-normal-prompts-for-location-with-prefix ()
  "Test that audio toggle prompts for location with prefix arg."
  (test-toggle-setup)
  (unwind-protect
      (let ((prompt-called nil)
            (recorded-dir nil))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (_prompt) (setq prompt-called t) "/custom/path/"))
                  ((symbol-function 'file-directory-p)
                   (lambda (_dir) t))  ; Directory exists
                  ((symbol-function 'cj/ffmpeg-record-audio)
                   (lambda (dir) (setq recorded-dir dir))))
          (cj/audio-recording-toggle t)
          (should prompt-called)
          (should (equal "/custom/path/" recorded-dir))))
    (test-toggle-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-video-toggle-boundary-creates-directory ()
  "Test that video toggle creates directory if it doesn't exist."
  (test-toggle-setup)
  (unwind-protect
      (let ((mkdir-called nil))
        (cl-letf (((symbol-function 'file-directory-p)
                   (lambda (_dir) nil))
                  ((symbol-function 'make-directory)
                   (lambda (_dir _parents) (setq mkdir-called t)))
                  ((symbol-function 'cj/ffmpeg-record-video)
                   (lambda (_dir) nil)))
          (cj/video-recording-toggle nil)
          (should mkdir-called)))
    (test-toggle-teardown)))

(ert-deftest test-video-audio-recording-audio-toggle-boundary-creates-directory ()
  "Test that audio toggle creates directory if it doesn't exist."
  (test-toggle-setup)
  (unwind-protect
      (let ((mkdir-called nil))
        (cl-letf (((symbol-function 'file-directory-p)
                   (lambda (_dir) nil))
                  ((symbol-function 'make-directory)
                   (lambda (_dir _parents) (setq mkdir-called t)))
                  ((symbol-function 'cj/ffmpeg-record-audio)
                   (lambda (_dir) nil)))
          (cj/audio-recording-toggle nil)
          (should mkdir-called)))
    (test-toggle-teardown)))

(provide 'test-video-audio-recording-toggle-functions)
;;; test-video-audio-recording-toggle-functions.el ends here
