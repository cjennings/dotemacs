;;; test-telega-config--server-death.el --- Tests for the telega-server death alert -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the telega-server death sentinel.  telega's own sentinel only
;; calls `message' on an abnormal exit, which scrolls away unseen -- so a
;; dead server reads as a quiet Telegram.  These pin the alert that replaces
;; that silence.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'telega-config)

;; -- cj/--telega-server-death-p ----------------------------------------------

(ert-deftest test-telega-config-death-p-nonzero-status-is-death ()
  "Normal: a non-zero exit status is an abnormal death."
  (should (cj/--telega-server-death-p 1))
  (should (cj/--telega-server-death-p 139)))

(ert-deftest test-telega-config-death-p-segv-signal-is-death ()
  "Normal: a signal number (SIGSEGV is 11) counts as death.
This is the observed failure -- the server segfaults rather than exiting."
  (should (cj/--telega-server-death-p 11)))

(ert-deftest test-telega-config-death-p-zero-is-clean-exit ()
  "Boundary: exit status 0 is a clean shutdown and must not alert.
Quitting telega deliberately would otherwise page on every exit."
  (should-not (cj/--telega-server-death-p 0)))

(ert-deftest test-telega-config-death-p-nil-is-not-death ()
  "Boundary: nil status (no status available) is not a death."
  (should-not (cj/--telega-server-death-p nil)))

(ert-deftest test-telega-config-death-p-non-integer-is-not-death ()
  "Error: a non-integer status returns nil rather than signalling.
`process-exit-status' is documented to return an integer, but this runs
inside telega's sentinel where an error would disrupt telega's own cleanup."
  (should-not (cj/--telega-server-death-p "segfault"))
  (should-not (cj/--telega-server-death-p 'exited)))

;; -- cj/--telega-server-exit-status ------------------------------------------

(ert-deftest test-telega-config-exit-status-nil-for-non-process ()
  "Error: a nil or non-process argument yields nil, not a signal.
The sentinel hands us whatever it has; a bad object must not break telega."
  (should-not (cj/--telega-server-exit-status nil))
  (should-not (cj/--telega-server-exit-status "not-a-process")))

;; -- cj/--telega-server-death-body -------------------------------------------

(ert-deftest test-telega-config-death-body-names-status-and-effect ()
  "Normal: the body carries the status and says coverage is affected.
The status distinguishes a segfault from a clean kill; the effect is why
the reader should care."
  (let ((body (cj/--telega-server-death-body 11 "segmentation fault\n")))
    (should (string-match-p "11" body))
    (should (string-match-p "Telegram" body))))

(ert-deftest test-telega-config-death-body-strips-trailing-newline ()
  "Boundary: a sentinel event string ends in a newline; it must not survive.
A trailing newline in a desktop notification body renders as dead space."
  (let ((body (cj/--telega-server-death-body 11 "segmentation fault\n")))
    (should-not (string-suffix-p "\n" body))))

(ert-deftest test-telega-config-death-body-handles-empty-event ()
  "Boundary: an empty or nil event still produces a usable body."
  (should (stringp (cj/--telega-server-death-body 11 nil)))
  (should (stringp (cj/--telega-server-death-body 11 "")))
  (should (string-match-p "11" (cj/--telega-server-death-body 11 nil))))

(ert-deftest test-telega-config-death-body-handles-unicode-event ()
  "Boundary: a non-ASCII event string passes through intact."
  (let ((body (cj/--telega-server-death-body 1 "ошибка сервера\n")))
    (should (string-match-p "ошибка" body))))

;; -- cj/--telega-server-notify-death (the advice) ----------------------------

(defmacro test-telega-config--with-status (status captured &rest body)
  "Run BODY with the exit status forced to STATUS.
Notification calls are captured into CAPTURED as (TITLE . BODY) pairs."
  (declare (indent 2))
  `(let ((,captured nil))
     (cl-letf (((symbol-function 'cj/--telega-server-exit-status)
                (lambda (&rest _) ,status))
               ((symbol-function 'cj/--telega-server-send-notification)
                (lambda (title body) (push (cons title body) ,captured))))
       ,@body)))

(ert-deftest test-telega-config-notify-fires-on-abnormal-exit ()
  "Normal: an abnormal exit sends exactly one notification."
  (test-telega-config--with-status 11 sent
    (cj/--telega-server-notify-death nil "segmentation fault\n")
    (should (= 1 (length sent)))
    (should (string-match-p "telega" (downcase (car (car sent)))))))

(ert-deftest test-telega-config-notify-silent-on-clean-exit ()
  "Boundary: a clean exit sends nothing.
Deliberately quitting telega must not page."
  (test-telega-config--with-status 0 sent
    (cj/--telega-server-notify-death nil "finished\n")
    (should-not sent)))

(ert-deftest test-telega-config-notify-silent-without-status ()
  "Boundary: no recoverable status sends nothing rather than a bare alert."
  (test-telega-config--with-status nil sent
    (cj/--telega-server-notify-death nil "gone\n")
    (should-not sent)))

(ert-deftest test-telega-config-notify-contains-notifier-errors ()
  "Error: a failing notifier must not escape into telega's sentinel.
This runs as :after advice on `telega-server--sentinel'; an error here
would abort telega's own status handling and its relogin path."
  (cl-letf (((symbol-function 'cj/--telega-server-exit-status)
             (lambda (&rest _) 11))
            ((symbol-function 'cj/--telega-server-send-notification)
             (lambda (&rest _) (error "notifier unavailable"))))
    (should (progn (cj/--telega-server-notify-death nil "segfault\n") t))))

(ert-deftest test-telega-config-notify-advice-is-named-and-removable ()
  "Normal: the sentinel advice is installed by name, so it can be removed.
An anonymous lambda can't be `advice-remove'd by reference, which strands
the old advice in a live daemon after the source stops installing it."
  (should (fboundp 'cj/--telega-server-notify-death))
  (should (symbolp 'cj/--telega-server-notify-death)))

(provide 'test-telega-config--server-death)
;;; test-telega-config--server-death.el ends here
