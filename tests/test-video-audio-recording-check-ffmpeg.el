;;; test-video-audio-recording-check-ffmpeg.el --- Tests for cj/recording-check-ffmpeg -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-check-ffmpeg function.
;; Tests detection of ffmpeg availability.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording-check-ffmpeg-normal-ffmpeg-found-returns-t ()
  "Test that function returns t when ffmpeg is found."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd)
               (when (equal cmd "ffmpeg") "/usr/bin/ffmpeg"))))
    (let ((result (cj/recording-check-ffmpeg)))
      (should (eq t result)))))

;;; Error Cases

(ert-deftest test-video-audio-recording-check-ffmpeg-error-ffmpeg-not-found-signals-error ()
  "Test that function signals user-error when ffmpeg is not found."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) nil)))
    (should-error (cj/recording-check-ffmpeg) :type 'user-error)))

(ert-deftest test-video-audio-recording-check-ffmpeg-error-message-mentions-pacman ()
  "Test that error message includes installation command."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) nil)))
    (condition-case err
        (cj/recording-check-ffmpeg)
      (user-error
       (should (string-match-p "pacman -S ffmpeg" (error-message-string err)))))))

(provide 'test-video-audio-recording-check-ffmpeg)
;;; test-video-audio-recording-check-ffmpeg.el ends here
