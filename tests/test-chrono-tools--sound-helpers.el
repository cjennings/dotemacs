;;; test-chrono-tools--sound-helpers.el --- Tests for the tmr sound-file helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/tmr--current-sound-name and cj/tmr--apply-sound-file were extracted from
;; the deeply-nested cj/tmr-select-sound-file so the "what's the current sound"
;; and "set the chosen sound" steps are unit-testable apart from the
;; completing-read UI.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'chrono-tools)

(defvar tmr-sound-file)
(defvar sounds-dir)
(defvar notification-sound)

(ert-deftest test-chrono-current-sound-name-existing ()
  "Normal: returns the basename when the current sound file exists."
  (let* ((f (make-temp-file "tmr-sound" nil ".wav"))
         (tmr-sound-file f))
    (unwind-protect
        (should (equal (cj/tmr--current-sound-name) (file-name-nondirectory f)))
      (delete-file f))))

(ert-deftest test-chrono-current-sound-name-missing-or-nil ()
  "Boundary: a missing file or nil yields nil."
  (let ((tmr-sound-file "/no/such/file.wav"))
    (should (null (cj/tmr--current-sound-name))))
  (let ((tmr-sound-file nil))
    (should (null (cj/tmr--current-sound-name)))))

(ert-deftest test-chrono-apply-sound-file-sets-and-messages ()
  "Normal: sets tmr-sound-file under sounds-dir and reports the choice."
  (let ((sounds-dir "/snd")
        (notification-sound "/snd/default.wav")
        (tmr-sound-file nil))
    (let ((msg (cj/tmr--apply-sound-file "chime.wav")))
      (should (equal tmr-sound-file "/snd/chime.wav"))
      (should (string-match-p "Timer sound set to: chime.wav" msg)))))

(ert-deftest test-chrono-apply-sound-file-default-branch ()
  "Boundary: choosing the notification sound reports it as the default."
  (let ((sounds-dir "/snd")
        (notification-sound "/snd/default.wav")
        (tmr-sound-file nil))
    (let ((msg (cj/tmr--apply-sound-file "default.wav")))
      (should (equal tmr-sound-file "/snd/default.wav"))
      (should (string-match-p "default: default.wav" msg)))))

(provide 'test-chrono-tools--sound-helpers)
;;; test-chrono-tools--sound-helpers.el ends here
