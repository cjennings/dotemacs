;;; test-org-agenda-config--auto-refresh.el --- Tests for agenda auto-refresh -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the wall-clock-aligned auto-refresh behind the F8 agenda:
;; the next-mark arithmetic, the on-screen-agenda lookup, the timer body's
;; error containment, and start/stop timer ownership.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

;; -- Fixtures ----------------------------------------------------------------

(defun test-org-agenda-auto-refresh--time (hh mm ss)
  "Return an Emacs time value for today at HH:MM:SS local time."
  (let ((now (decode-time)))
    (encode-time (list ss mm hh
                       (nth 3 now) (nth 4 now) (nth 5 now)
                       nil -1 (nth 8 now)))))

(defmacro test-org-agenda-auto-refresh--with-agenda-window (&rest body)
  "Run BODY with a window displaying a buffer in `org-agenda-mode'.
Sets `major-mode' directly rather than calling the mode function: the
lookup under test only asks what mode the window's buffer is in, and
`org-agenda-mode' setup wants a real agenda build behind it."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*Org Agenda*")))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer buffer
             (setq major-mode 'org-agenda-mode))
           (set-window-buffer (selected-window) buffer)
           ,@body)
       (kill-buffer buffer))))

;; -- cj/--org-agenda-seconds-to-next-mark ------------------------------------

(ert-deftest test-org-agenda-config-next-mark-mid-interval ()
  "Normal: a time between marks returns the remainder to the next one."
  (should (= 120 (cj/--org-agenda-seconds-to-next-mark
                  (test-org-agenda-auto-refresh--time 9 3 0) 300))))

(ert-deftest test-org-agenda-config-next-mark-exactly-on-mark ()
  "Boundary: a time exactly on a mark returns a full period, never zero.
A zero delay would fire the timer immediately and again a period later."
  (should (= 300 (cj/--org-agenda-seconds-to-next-mark
                  (test-org-agenda-auto-refresh--time 9 5 0) 300))))

(ert-deftest test-org-agenda-config-next-mark-one-second-before ()
  "Boundary: one second short of a mark returns one second."
  (should (= 1 (cj/--org-agenda-seconds-to-next-mark
                (test-org-agenda-auto-refresh--time 9 4 59) 300))))

(ert-deftest test-org-agenda-config-next-mark-lands-on-a-multiple ()
  "Normal: TIME plus the result is always a whole multiple of PERIOD.
This is the property that makes the refresh land on :00/:05 rather than
five minutes after whenever the agenda happened to open."
  (dolist (seconds '(0 1 59 60 61 149 150 299))
    (let* ((time (test-org-agenda-auto-refresh--time 9 0 0))
           (start (+ (floor (float-time time)) seconds))
           (delay (cj/--org-agenda-seconds-to-next-mark start 300)))
      (should (zerop (mod (+ start delay) 300))))))

(ert-deftest test-org-agenda-config-next-mark-rejects-nonpositive-period ()
  "Error: a zero or negative period is a caller bug, not a silent no-op."
  (should-error (cj/--org-agenda-seconds-to-next-mark (current-time) 0))
  (should-error (cj/--org-agenda-seconds-to-next-mark (current-time) -300)))

;; -- cj/--org-agenda-refresh-window ------------------------------------------

(ert-deftest test-org-agenda-config-refresh-window-finds-displayed-agenda ()
  "Normal: the lookup returns the window showing an agenda buffer."
  (test-org-agenda-auto-refresh--with-agenda-window
    (should (eq (cj/--org-agenda-refresh-window) (selected-window)))))

(ert-deftest test-org-agenda-config-refresh-window-nil-when-not-displayed ()
  "Boundary: an agenda buffer that exists but is off-screen is not a target.
Rebuilding an invisible agenda costs time and shows nobody anything."
  (let ((buffer (get-buffer-create "*Org Agenda*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq major-mode 'org-agenda-mode))
          (should-not (cj/--org-agenda-refresh-window)))
      (kill-buffer buffer))))

(ert-deftest test-org-agenda-config-refresh-window-nil-with-no-agenda ()
  "Boundary: no agenda buffer anywhere returns nil rather than erroring."
  (should-not (cj/--org-agenda-refresh-window)))

;; -- cj/--org-agenda-auto-refresh (the timer body) ---------------------------

(ert-deftest test-org-agenda-config-auto-refresh-noop-without-agenda ()
  "Boundary: the tick does nothing, and signals nothing, with no agenda up."
  (let ((called nil))
    (cl-letf (((symbol-function 'org-agenda-redo)
               (lambda (&rest _) (setq called t))))
      (cj/--org-agenda-auto-refresh)
      (should-not called))))

(ert-deftest test-org-agenda-config-auto-refresh-redoes-visible-agenda ()
  "Normal: the tick redoes the agenda shown on screen."
  (let ((called nil))
    (cl-letf (((symbol-function 'org-agenda-redo)
               (lambda (&rest _) (setq called t))))
      (test-org-agenda-auto-refresh--with-agenda-window
        (cj/--org-agenda-auto-refresh))
      (should called))))

(ert-deftest test-org-agenda-config-auto-refresh-contains-errors ()
  "Error: a failing redo must not escape the timer body.
An unguarded signal in a repeating timer resignals on every tick, which is
how a five-minute timer turns into an endless backtrace."
  (cl-letf (((symbol-function 'org-agenda-redo)
             (lambda (&rest _) (error "simulated redo failure")))
            ((symbol-function 'cj/log-silently) (lambda (&rest _) nil)))
    (test-org-agenda-auto-refresh--with-agenda-window
      (should (progn (cj/--org-agenda-auto-refresh) t)))))

(ert-deftest test-org-agenda-config-auto-refresh-restores-point-line ()
  "Normal: the tick leaves point on the line it started on.
A refresh that scrolls the reader back to the top every five minutes is
worse than no refresh."
  (cl-letf (((symbol-function 'org-agenda-redo) (lambda (&rest _) nil)))
    (test-org-agenda-auto-refresh--with-agenda-window
      (with-current-buffer "*Org Agenda*"
        (erase-buffer)
        (dotimes (i 10) (insert (format "agenda line %d\n" i)))
        (goto-char (point-min))
        (forward-line 4))
      (cj/--org-agenda-auto-refresh)
      (with-current-buffer "*Org Agenda*"
        (should (= 5 (line-number-at-pos)))))))

;; -- start / stop ------------------------------------------------------------

(ert-deftest test-org-agenda-config-auto-refresh-start-owns-one-timer ()
  "Normal: starting twice leaves exactly one timer, not two.
Re-loading the module in a live daemon re-runs the arming form, so a
non-idempotent start would stack a second ticker on every reload."
  (let ((cj/--org-agenda-refresh-timer nil))
    (unwind-protect
        (progn
          (cj/org-agenda-auto-refresh-start)
          (let ((first cj/--org-agenda-refresh-timer))
            (cj/org-agenda-auto-refresh-start)
            (should (timerp cj/--org-agenda-refresh-timer))
            (should-not (memq first timer-list))
            (should (memq cj/--org-agenda-refresh-timer timer-list))))
      (cj/org-agenda-auto-refresh-stop))))

(ert-deftest test-org-agenda-config-auto-refresh-stop-clears-timer ()
  "Normal: stopping cancels the timer and clears the handle."
  (let ((cj/--org-agenda-refresh-timer nil))
    (cj/org-agenda-auto-refresh-start)
    (let ((timer cj/--org-agenda-refresh-timer))
      (cj/org-agenda-auto-refresh-stop)
      (should-not cj/--org-agenda-refresh-timer)
      (should-not (memq timer timer-list)))))

(ert-deftest test-org-agenda-config-auto-refresh-stop-is-safe-when-stopped ()
  "Boundary: stopping an already-stopped refresh is a no-op, not an error."
  (let ((cj/--org-agenda-refresh-timer nil))
    (should (progn (cj/org-agenda-auto-refresh-stop) t))
    (should-not cj/--org-agenda-refresh-timer)))

(provide 'test-org-agenda-config--auto-refresh)
;;; test-org-agenda-config--auto-refresh.el ends here
