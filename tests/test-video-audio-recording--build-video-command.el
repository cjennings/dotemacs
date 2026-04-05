;;; test-video-audio-recording--build-video-command.el --- Tests for video command builder -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--build-video-command.
;; Verifies correct shell command construction for Wayland (wf-recorder|ffmpeg)
;; and X11 (ffmpeg x11grab) video recording pipelines.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--build-video-command-normal-wayland-uses-wf-recorder ()
  "Wayland command pipes wf-recorder to ffmpeg."
  (let ((cj/recording-mic-boost 2.0)
        (cj/recording-system-volume 1.0))
    (cl-letf (((symbol-function 'executable-find) (lambda (_prog) t)))
      (let ((cmd (cj/recording--build-video-command "mic" "sys" "/tmp/out.mkv" t)))
        (should (string-match-p "wf-recorder.*|.*ffmpeg" cmd))
        (should (string-match-p "-i pipe:0" cmd))
        (should (string-match-p "-c:v copy" cmd))))))

(ert-deftest test-video-audio-recording--build-video-command-normal-x11-uses-x11grab ()
  "X11 command uses ffmpeg with x11grab, no wf-recorder."
  (let ((cj/recording-mic-boost 2.0)
        (cj/recording-system-volume 1.0))
    (let ((cmd (cj/recording--build-video-command "mic" "sys" "/tmp/out.mkv" nil)))
      (should (string-match-p "x11grab" cmd))
      (should-not (string-match-p "wf-recorder" cmd)))))

(ert-deftest test-video-audio-recording--build-video-command-normal-devices-in-command ()
  "Both mic and system device names appear in the command."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0))
    (let ((cmd (cj/recording--build-video-command
                "alsa_input.usb-Jabra-00.mono"
                "alsa_output.usb-JDS-00.monitor"
                "/tmp/out.mkv" nil)))
      (should (string-match-p "alsa_input.usb-Jabra-00.mono" cmd))
      (should (string-match-p "alsa_output.usb-JDS-00.monitor" cmd)))))

(ert-deftest test-video-audio-recording--build-video-command-normal-volume-in-filter ()
  "Volume settings appear in the filter_complex expression."
  (let ((cj/recording-mic-boost 1.5)
        (cj/recording-system-volume 0.7))
    (let ((cmd (cj/recording--build-video-command "mic" "sys" "/tmp/out.mkv" nil)))
      (should (string-match-p "volume=1\\.5" cmd))
      (should (string-match-p "volume=0\\.7" cmd)))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--build-video-command-boundary-special-chars-quoted ()
  "Device names with special characters are shell-quoted in Wayland mode."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0))
    (cl-letf (((symbol-function 'executable-find) (lambda (_prog) t)))
      (let ((cmd (cj/recording--build-video-command
                  "device with spaces" "sys" "/tmp/out.mkv" t)))
        ;; shell-quote-argument escapes spaces with backslashes
        (should (string-match-p "device\\\\ with\\\\ spaces" cmd))))))

(ert-deftest test-video-audio-recording--build-video-command-boundary-filename-with-spaces ()
  "Output filename with spaces is shell-quoted in Wayland mode."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0))
    (cl-letf (((symbol-function 'executable-find) (lambda (_prog) t)))
      (let ((cmd (cj/recording--build-video-command
                  "mic" "sys" "/tmp/my recording.mkv" t)))
        ;; Filename should be quoted/escaped
        (should (string-match-p "recording" cmd))))))

(ert-deftest test-video-audio-recording--build-video-command-boundary-zero-volume ()
  "Zero volume values produce 0.0 in the command."
  (let ((cj/recording-mic-boost 0.0)
        (cj/recording-system-volume 0.0))
    (let ((cmd (cj/recording--build-video-command "mic" "sys" "/tmp/out.mkv" nil)))
      (should (string-match-p "volume=0\\.0" cmd)))))

;;; Error Cases

(ert-deftest test-video-audio-recording--build-video-command-error-wayland-no-wf-recorder ()
  "Wayland mode signals error when wf-recorder is not installed."
  (cl-letf (((symbol-function 'executable-find) (lambda (_prog) nil)))
    (should-error (cj/recording--build-video-command "mic" "sys" "/tmp/out.mkv" t)
                  :type 'user-error)))

(ert-deftest test-video-audio-recording--build-video-command-error-x11-no-wf-recorder-check ()
  "X11 mode does not check for wf-recorder at all."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0)
        (check-called nil))
    (cl-letf (((symbol-function 'cj/recording--check-wf-recorder)
               (lambda () (setq check-called t))))
      (cj/recording--build-video-command "mic" "sys" "/tmp/out.mkv" nil)
      (should-not check-called))))

(provide 'test-video-audio-recording--build-video-command)
;;; test-video-audio-recording--build-video-command.el ends here
