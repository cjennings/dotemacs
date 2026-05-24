;;; test-video-audio-recording--interrupt-child-wf-recorder.el --- Tests for scoped wf-recorder interrupt -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--interrupt-child-wf-recorder.
;; Verifies the producer-first stop signal targets only the wf-recorder
;; child of this module's own shell process, not every wf-recorder on the
;; system.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--interrupt-child-wf-recorder-normal-scopes-to-parent-pid ()
  "Normal: interrupt is scoped to the wf-recorder child of the given PID."
  (let ((captured nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured args) 0)))
      (cj/recording--interrupt-child-wf-recorder 12345)
      ;; Targets wf-recorder, by parent PID, with SIGINT — not a bare
      ;; system-wide name match.
      (should (member "-INT" captured))
      (should (member "-P" captured))
      (should (member "12345" captured))
      (should (member "wf-recorder" captured)))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--interrupt-child-wf-recorder-boundary-nil-pid-no-call ()
  "Boundary: a nil PID issues no kill at all (nothing to scope to)."
  (let ((called nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (setq called t) 0)))
      (cj/recording--interrupt-child-wf-recorder nil)
      (should-not called))))

;;; Error Cases

(ert-deftest test-video-audio-recording--interrupt-child-wf-recorder-error-never-bare-name ()
  "Error: must never call pkill with only the program name (system-wide kill)."
  (let ((captured nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured args) 0)))
      (cj/recording--interrupt-child-wf-recorder 999)
      ;; The whole point of the fix: a parent-scoping flag is always present.
      (should (member "-P" captured)))))

(provide 'test-video-audio-recording--interrupt-child-wf-recorder)
;;; test-video-audio-recording--interrupt-child-wf-recorder.el ends here
