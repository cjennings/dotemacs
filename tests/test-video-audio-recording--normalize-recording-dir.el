;;; test-video-audio-recording--normalize-recording-dir.el --- Tests for recording-dir normalization -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--normalize-recording-dir.
;; The recording target is always a directory that ffmpeg writes a
;; timestamped file into.  Normalization must yield an absolute directory
;; path with a trailing slash so the *selected* directory (not its parent)
;; is the one created and recorded into.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--normalize-recording-dir-normal-adds-trailing-slash ()
  "Normal: a path without a trailing slash becomes a directory path."
  (should (equal "/tmp/foo/"
                 (cj/recording--normalize-recording-dir "/tmp/foo"))))

(ert-deftest test-video-audio-recording--normalize-recording-dir-normal-idempotent ()
  "Normal: a path that is already a directory is returned unchanged."
  (should (equal "/tmp/foo/"
                 (cj/recording--normalize-recording-dir "/tmp/foo/"))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--normalize-recording-dir-boundary-spaces-preserved ()
  "Boundary: spaces in the path are preserved (quoting happens at the shell)."
  (should (equal "/tmp/my recordings/"
                 (cj/recording--normalize-recording-dir "/tmp/my recordings"))))

(ert-deftest test-video-audio-recording--normalize-recording-dir-boundary-relative-expands-absolute ()
  "Boundary: a relative path expands to an absolute directory path."
  (let ((result (cj/recording--normalize-recording-dir "foo")))
    (should (file-name-absolute-p result))
    (should (string-suffix-p "/foo/" result))))

(provide 'test-video-audio-recording--normalize-recording-dir)
;;; test-video-audio-recording--normalize-recording-dir.el ends here
