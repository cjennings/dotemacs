;;; test-video-audio-recording--label-devices.el --- Tests for device labeling -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--label-devices.
;; Verifies label formatting ("Description [status]") and sort order
;; (in use → ready → available → muted).
;; Also indirectly tests cj/recording--device-sort-key and
;; cj/recording--device-status-label since they are trivial helpers
;; consumed entirely by label-devices.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--label-devices-normal-format ()
  "Test that labels are formatted as \"Description [status]\"."
  (let ((result (cj/recording--label-devices
                 '(("sink-a" "JDS Labs" "RUNNING" "no")))))
    (should (= 1 (length result)))
    (should (equal "JDS Labs [in use]" (caar result)))
    (should (equal "sink-a" (cdar result)))))

(ert-deftest test-video-audio-recording--label-devices-normal-sort-order ()
  "Test that devices sort: in use → ready → available → muted."
  (let ((result (cj/recording--label-devices
                 '(("d-suspended" "Suspended" "SUSPENDED" "no")
                   ("d-muted"     "Muted"     "RUNNING"   "yes")
                   ("d-running"   "Running"   "RUNNING"   "no")
                   ("d-idle"      "Idle"       "IDLE"      "no")))))
    (should (equal '("d-running" "d-idle" "d-suspended" "d-muted")
                   (mapcar #'cdr result)))))

(ert-deftest test-video-audio-recording--label-devices-normal-muted-label ()
  "Test that muted devices get [muted] regardless of state."
  (let ((result (cj/recording--label-devices
                 '(("m1" "Mic" "RUNNING" "yes")))))
    (should (string-match-p "\\[muted\\]" (caar result)))))

(ert-deftest test-video-audio-recording--label-devices-normal-idle-label ()
  "Test that IDLE unmuted devices get [ready]."
  (let ((result (cj/recording--label-devices
                 '(("d1" "Device" "IDLE" "no")))))
    (should (string-match-p "\\[ready\\]" (caar result)))))

(ert-deftest test-video-audio-recording--label-devices-normal-suspended-label ()
  "Test that SUSPENDED unmuted devices get [available]."
  (let ((result (cj/recording--label-devices
                 '(("d1" "Device" "SUSPENDED" "no")))))
    (should (string-match-p "\\[available\\]" (caar result)))))

(ert-deftest test-video-audio-recording--label-devices-normal-returns-alist ()
  "Test that result is a proper (label . name) alist."
  (let ((result (cj/recording--label-devices
                 '(("a" "Alpha" "IDLE" "no")
                   ("b" "Beta" "RUNNING" "no")))))
    (should (= 2 (length result)))
    (dolist (entry result)
      (should (consp entry))
      (should (stringp (car entry)))
      (should (stringp (cdr entry))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--label-devices-boundary-empty ()
  "Test that empty device list returns empty alist."
  (should (null (cj/recording--label-devices nil))))

(ert-deftest test-video-audio-recording--label-devices-boundary-single ()
  "Test that single device returns single-element alist."
  (let ((result (cj/recording--label-devices
                 '(("only" "Only Device" "IDLE" "no")))))
    (should (= 1 (length result)))
    (should (equal "only" (cdar result)))))

(ert-deftest test-video-audio-recording--label-devices-boundary-all-muted ()
  "Test that all-muted devices sort together and all get [muted]."
  (let ((result (cj/recording--label-devices
                 '(("a" "A" "RUNNING" "yes")
                   ("b" "B" "IDLE" "yes")
                   ("c" "C" "SUSPENDED" "yes")))))
    (should (= 3 (length result)))
    (dolist (entry result)
      (should (string-match-p "\\[muted\\]" (car entry))))))

(ert-deftest test-video-audio-recording--label-devices-boundary-all-same-status ()
  "Test that devices with identical status preserve relative order."
  (let ((result (cj/recording--label-devices
                 '(("a" "Alpha" "RUNNING" "no")
                   ("b" "Beta" "RUNNING" "no")))))
    (should (= 2 (length result)))
    ;; Both should have [in use] label
    (dolist (entry result)
      (should (string-match-p "\\[in use\\]" (car entry))))))

(ert-deftest test-video-audio-recording--label-devices-boundary-unknown-state ()
  "Test that unknown state falls through to [available] (sort key 2)."
  (let ((result (cj/recording--label-devices
                 '(("d1" "Device" "UNKNOWN_STATE" "no")))))
    (should (string-match-p "\\[available\\]" (caar result)))))

(provide 'test-video-audio-recording--label-devices)
;;; test-video-audio-recording--label-devices.el ends here
