;;; test-video-audio-recording--label-sinks.el --- Tests for sink labeling -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--label-sinks.
;; Verifies that sink labels include app names for active sinks,
;; e.g. "JDS Labs [in use] (Firefox)" and sort correctly.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Helpers

(defun test-label-sinks--stub (sink-apps-alist sinks-short-output)
  "Return a cl-letf binding list that stubs get-sink-apps and sinks short.
SINK-APPS-ALIST is the return value for `cj/recording--get-sink-apps'.
SINKS-SHORT-OUTPUT is the raw pactl list sinks short output."
  ;; We use cl-letf in each test to stub both get-sink-apps and
  ;; shell-command-to-string (for the sinks short call inside label-sinks).
  `(((symbol-function 'cj/recording--get-sink-apps)
     (lambda () ',sink-apps-alist))
    ((symbol-function 'shell-command-to-string)
     (lambda (_cmd) ,sinks-short-output))))

;;; Normal Cases

(ert-deftest test-video-audio-recording--label-sinks-normal-appends-app-names ()
  "Test that active sinks show app names in parentheses."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () '(("65" "Firefox"))))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "65\tjds-labs\tmodule\ts16le\tRUNNING\n")))
    (let ((result (cj/recording--label-sinks
                   '(("jds-labs" "JDS Labs" "RUNNING" "no")))))
      (should (= 1 (length result)))
      (should (string-match-p "JDS Labs \\[in use\\] (Firefox)" (caar result))))))

(ert-deftest test-video-audio-recording--label-sinks-normal-multiple-apps ()
  "Test that multiple apps are comma-separated."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () '(("65" "Firefox" "Spotify"))))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "65\tjds-labs\tmodule\ts16le\tRUNNING\n")))
    (let ((result (cj/recording--label-sinks
                   '(("jds-labs" "JDS Labs" "RUNNING" "no")))))
      (should (string-match-p "(Firefox, Spotify)" (caar result))))))

(ert-deftest test-video-audio-recording--label-sinks-normal-no-apps ()
  "Test that sinks without apps have no parenthesized suffix."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () nil))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "65\tjds-labs\tmodule\ts16le\tSUSPENDED\n")))
    (let ((result (cj/recording--label-sinks
                   '(("jds-labs" "JDS Labs" "SUSPENDED" "no")))))
      (should (string-match-p "JDS Labs \\[available\\]$" (caar result))))))

(ert-deftest test-video-audio-recording--label-sinks-normal-sort-order ()
  "Test that sinks sort: in use → ready → available → muted."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () '(("65" "Firefox"))))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               "65\tsink-run\tmodule\ts16le\tRUNNING\n73\tsink-idle\tmodule\ts16le\tIDLE\n80\tsink-sus\tmodule\ts16le\tSUSPENDED\n")))
    (let ((result (cj/recording--label-sinks
                   '(("sink-sus"  "Suspended" "SUSPENDED" "no")
                     ("sink-run"  "Running"   "RUNNING"   "no")
                     ("sink-idle" "Idle"      "IDLE"      "no")))))
      (should (equal '("sink-run" "sink-idle" "sink-sus")
                     (mapcar #'cdr result))))))

(ert-deftest test-video-audio-recording--label-sinks-normal-muted-sink ()
  "Test that muted sinks get [muted] label and no apps shown."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () nil))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "65\tmuted-sink\tmodule\ts16le\tSUSPENDED\n")))
    (let ((result (cj/recording--label-sinks
                   '(("muted-sink" "Muted Sink" "SUSPENDED" "yes")))))
      (should (string-match-p "\\[muted\\]" (caar result))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--label-sinks-boundary-empty ()
  "Test that empty sink list returns empty alist."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () nil))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should (null (cj/recording--label-sinks nil)))))

(ert-deftest test-video-audio-recording--label-sinks-boundary-sink-not-in-short ()
  "Test that sink not found in sinks short still gets label (no apps)."
  (cl-letf (((symbol-function 'cj/recording--get-sink-apps)
             (lambda () '(("65" "Firefox"))))
            ((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (let ((result (cj/recording--label-sinks
                   '(("unknown-sink" "Unknown" "IDLE" "no")))))
      (should (= 1 (length result)))
      ;; No app suffix since index lookup returned nil
      (should (string-match-p "Unknown \\[ready\\]$" (caar result))))))

(provide 'test-video-audio-recording--label-sinks)
;;; test-video-audio-recording--label-sinks.el ends here
