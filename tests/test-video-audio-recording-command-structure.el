;;; test-video-audio-recording-command-structure.el --- Tests for recording command string structure -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that validate the structure and syntax of shell commands generated
;; for video and audio recording. These tests catch issues like invalid flags,
;; wrong encoder names, and incorrect output syntax that behavioral tests miss.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(defvar video-recordings-dir "/tmp/video-recordings/")
(defvar audio-recordings-dir "/tmp/audio-recordings/")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-command-structure-setup ()
  "Reset all variables before each test."
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device "test-mic-device")
  (setq cj/recording-system-device "test-monitor-device")
  (setq cj/recording-mic-boost 2.0)
  (setq cj/recording-system-volume 1.0))

(defun test-command-structure-teardown ()
  "Clean up after each test."
  (when cj/video-recording-ffmpeg-process
    (ignore-errors (delete-process cj/video-recording-ffmpeg-process)))
  (when cj/audio-recording-ffmpeg-process
    (ignore-errors (delete-process cj/audio-recording-ffmpeg-process)))
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Wayland Video Recording - Command Structure

(ert-deftest test-video-recording-wayland-command-contains-wf-recorder ()
  "Test that Wayland video recording uses wf-recorder."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "wf-recorder" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-command-has-auto-confirm-flag ()
  "Test that wf-recorder command includes -y flag to auto-confirm overwrite."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "wf-recorder -y" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-command-has-correct-encoder ()
  "Test that wf-recorder uses libx264 encoder (not h264)."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; Should use libx264, not h264
          (should (string-match-p "-c libx264" command))
          (should-not (string-match-p "-c h264[^a-z]" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-command-has-muxer-format ()
  "Test that wf-recorder specifies matroska muxer with -m flag."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "-m matroska" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-command-outputs-to-stdout ()
  "Test that wf-recorder outputs to stdout using -f /dev/stdout."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; Should use -f /dev/stdout, not -o -
          (should (string-match-p "-f /dev/stdout" command))
          (should-not (string-match-p " -o -" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-command-no-invalid-flags ()
  "Test that wf-recorder command doesn't contain invalid flags."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; These flags don't exist in wf-recorder
          (should-not (string-match-p "--no-audio" command))
          (should-not (string-match-p "--noaudio" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-command-pipes-to-ffmpeg ()
  "Test that wf-recorder output is piped to ffmpeg."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; Should pipe wf-recorder to ffmpeg
          (should (string-match-p "wf-recorder.*|.*ffmpeg" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-wayland-ffmpeg-reads-from-pipe ()
  "Test that ffmpeg reads from pipe input."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "ffmpeg -i pipe:0" command))))
    (test-command-structure-teardown)))

;;; X11 Video Recording - Command Structure

(ert-deftest test-video-recording-x11-command-uses-x11grab ()
  "Test that X11 video recording uses ffmpeg x11grab."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "x11grab" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-x11-command-no-wf-recorder ()
  "Test that X11 video recording doesn't use wf-recorder."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should-not (string-match-p "wf-recorder" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-x11-command-captures-display ()
  "Test that X11 video recording captures from display :0."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "-i :0" command))))
    (test-command-structure-teardown)))

;;; Audio Recording - Command Structure

(ert-deftest test-audio-recording-command-uses-ffmpeg ()
  "Test that audio recording uses ffmpeg directly."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-audio" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should (string-match-p "^ffmpeg " command))))
    (test-command-structure-teardown)))

(ert-deftest test-audio-recording-command-uses-pulse-input ()
  "Test that audio recording uses pulse audio input."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-audio" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should (string-match-p "-f pulse" command))))
    (test-command-structure-teardown)))

(ert-deftest test-audio-recording-command-outputs-m4a ()
  "Test that audio recording outputs to .m4a file."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-audio" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should (string-match-p "\\.m4a" command))))
    (test-command-structure-teardown)))

;;; Common Command Structure (Both Video and Audio)

(ert-deftest test-video-recording-command-has-audio-filter-complex ()
  "Test that video recording includes audio filter for mixing mic and system."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "-filter_complex" command))
          (should (string-match-p "amerge" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-command-maps-video-and-audio ()
  "Test that video recording maps both video and audio streams."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "-map 0:v" command))
          (should (string-match-p "-map.*\\[out\\]" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-command-copies-video-codec ()
  "Test that video recording copies video codec (no re-encoding)."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "-c:v copy" command))))
    (test-command-structure-teardown)))

(ert-deftest test-video-recording-command-outputs-mkv ()
  "Test that video recording outputs to .mkv file."
  (test-command-structure-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "\\.mkv" command))))
    (test-command-structure-teardown)))

(provide 'test-video-audio-recording-command-structure)
;;; test-video-audio-recording-command-structure.el ends here
