;;; test-ai-term--shutdown-countdown.el --- Tests for the shutdown countdown -*- lexical-binding: t; -*-

;;; Commentary:
;; The "wrap it up and shutdown" countdown.  The testable logic is the safety
;; gate (abort when more than one aiv-* session is live) and the cancel/timer
;; bookkeeping; the tick rendering and the actual shutdown side effect are
;; manual (see the spec).  shell-command is stubbed throughout so no test can
;; power the machine off, and timers are cancelled rather than allowed to fire.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defmacro test-ai-term-shutdown--with (live-count shell-var &rest body)
  "Run BODY with `cj/ai-term-live-count' mocked to LIVE-COUNT and `shell-command'
recording its argument into SHELL-VAR; the timer is cleared before and after."
  (declare (indent 2))
  `(progn
     (cj/--ai-term-shutdown-clear-timer)
     (unwind-protect
         (cl-letf (((symbol-function 'cj/ai-term-live-count) (lambda () ,live-count))
                   ((symbol-function 'shell-command)
                    (lambda (cmd &rest _) (setq ,shell-var cmd) 0)))
           ,@body)
       (cj/--ai-term-shutdown-clear-timer))))

(ert-deftest test-ai-term-shutdown-aborts-when-other-sessions-live ()
  "Normal: more than one live session aborts -- no timer, no shutdown."
  (let ((shell nil))
    (test-ai-term-shutdown--with 2 shell
      (should-not (cj/ai-term-shutdown-countdown 3))
      (should-not cj/--ai-term-shutdown-timer)
      (should-not shell))))

(ert-deftest test-ai-term-shutdown-schedules-timer-when-sole-session ()
  "Normal: the sole live session schedules the countdown timer (does not fire here)."
  (let ((shell nil))
    (test-ai-term-shutdown--with 1 shell
      (cj/ai-term-shutdown-countdown 3)
      (should (timerp cj/--ai-term-shutdown-timer))
      ;; The timer has not ticked (no event loop in batch), so no shutdown yet.
      (should-not shell))))

(ert-deftest test-ai-term-shutdown-cancel-clears-the-timer ()
  "Normal: cancel stops an in-progress countdown."
  (let ((shell nil))
    (test-ai-term-shutdown--with 1 shell
      (cj/ai-term-shutdown-countdown 5)
      (should (timerp cj/--ai-term-shutdown-timer))
      (cj/ai-term-shutdown-cancel)
      (should-not cj/--ai-term-shutdown-timer)
      (should-not shell))))

(ert-deftest test-ai-term-shutdown-tick-fires-shutdown-at-zero ()
  "Boundary: invoking the timer function at zero remaining runs the shutdown
command and clears the timer.  Drives the tick directly rather than waiting."
  (let ((shell nil))
    (test-ai-term-shutdown--with 1 shell
      (cj/ai-term-shutdown-countdown 1)
      (let ((fn (timer--function cj/--ai-term-shutdown-timer)))
        ;; remaining starts at 1: first call renders, second call hits zero.
        (funcall fn)
        (should-not shell)
        (funcall fn)
        (should (equal shell cj/ai-term-shutdown-command))
        (should-not cj/--ai-term-shutdown-timer)))))

(provide 'test-ai-term--shutdown-countdown)
;;; test-ai-term--shutdown-countdown.el ends here
