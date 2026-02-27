;;; test-video-audio-recording--get-sink-apps.el --- Tests for sink app discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-sink-apps.
;; Verifies parsing of `pactl list sink-inputs' output to map
;; sink indices to application names.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Helpers

(defvar test-sink-apps--dir
  (file-name-directory (or load-file-name (locate-library "test-video-audio-recording--get-sink-apps")))
  "Directory containing this test file.")

(defun test-sink-apps--fixture (filename)
  "Read fixture FILENAME from the fixtures directory."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat "fixtures/" filename) test-sink-apps--dir))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-video-audio-recording--get-sink-apps-normal-single-app ()
  "Test parsing a single app on a single sink."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) (test-sink-apps--fixture "pactl-sink-inputs-active.txt"))))
    (let ((result (cj/recording--get-sink-apps)))
      (should (= 1 (length result)))
      (should (equal '("Firefox") (cdr (assoc "65" result)))))))

(ert-deftest test-video-audio-recording--get-sink-apps-normal-different-sink ()
  "Test that sink index is correctly parsed from different fixture."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) (test-sink-apps--fixture "pactl-sink-inputs-different-sink.txt"))))
    (let ((result (cj/recording--get-sink-apps)))
      (should (= 1 (length result)))
      (should (assoc "73" result))
      (should-not (assoc "65" result)))))

(ert-deftest test-video-audio-recording--get-sink-apps-normal-multiple-sinks ()
  "Test parsing apps across multiple sinks."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (concat (test-sink-apps--fixture "pactl-sink-inputs-active.txt")
                       "\n"
                       (test-sink-apps--fixture "pactl-sink-inputs-different-sink.txt")))))
    (let ((result (cj/recording--get-sink-apps)))
      (should (= 2 (length result)))
      (should (assoc "65" result))
      (should (assoc "73" result)))))

(ert-deftest test-video-audio-recording--get-sink-apps-normal-multiple-apps-same-sink ()
  "Test that multiple apps on the same sink are collected together."
  (let ((output "Sink Input #1\n\tSink: 65\n\tProperties:\n\t\tapplication.name = \"Firefox\"\nSink Input #2\n\tSink: 65\n\tProperties:\n\t\tapplication.name = \"Spotify\"\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording--get-sink-apps)))
        (should (= 1 (length result)))
        (should (equal '("Firefox" "Spotify") (cdr (assoc "65" result))))))))

(ert-deftest test-video-audio-recording--get-sink-apps-normal-deduplicates ()
  "Test that duplicate app names on the same sink are deduplicated."
  (let ((output "Sink Input #1\n\tSink: 65\n\tProperties:\n\t\tapplication.name = \"Firefox\"\nSink Input #2\n\tSink: 65\n\tProperties:\n\t\tapplication.name = \"Firefox\"\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording--get-sink-apps)))
        (should (equal '("Firefox") (cdr (assoc "65" result))))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--get-sink-apps-boundary-empty ()
  "Test that empty pactl output returns empty alist."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should (null (cj/recording--get-sink-apps)))))

(ert-deftest test-video-audio-recording--get-sink-apps-boundary-no-properties ()
  "Test sink input with no properties section returns empty."
  (let ((output "Sink Input #1\n\tSink: 65\n\tDriver: PipeWire\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (should (null (cj/recording--get-sink-apps))))))

(ert-deftest test-video-audio-recording--get-sink-apps-boundary-no-sink-line ()
  "Test sink input with no Sink: line does not crash."
  (let ((output "Sink Input #1\n\tDriver: PipeWire\n\tProperties:\n\t\tapplication.name = \"Firefox\"\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      ;; current-sink is nil so app name won't be stored
      (should (null (cj/recording--get-sink-apps))))))

;;; Error Cases

(ert-deftest test-video-audio-recording--get-sink-apps-error-garbled-output ()
  "Test that garbled output does not crash, returns empty."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "random garbage\nwith\nnewlines\n")))
    (should (null (cj/recording--get-sink-apps)))))

(ert-deftest test-video-audio-recording--get-sink-apps-error-missing-app-name ()
  "Test sink input with application.name missing value."
  (let ((output "Sink Input #1\n\tSink: 65\n\tProperties:\n\t\tapplication.name = \"\"\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      ;; Empty string between quotes won't match [^\"]+ so no app stored
      (should (null (cj/recording--get-sink-apps))))))

(provide 'test-video-audio-recording--get-sink-apps)
;;; test-video-audio-recording--get-sink-apps.el ends here
