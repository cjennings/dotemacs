;;; test-signal-config-notify.el --- Tests for the signal-config notification slice -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for the notification slice of `signal-config': the pure
;; body formatter (whitespace collapse + truncation to
;; `cj/signal--notify-body-max') and `cj/signel--notify' routing (the
;; suppression gate, the notify-script path with the sound flag, and
;; the `notifications-notify' fallback).  Spec: the "Notification
;; slice" addendum in docs/specs/signal-client-spec-doing.org.  No signal-cli or
;; linked account needed.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; signel is the fork at ~/code/signel; signal-config wires it via
;; use-package but these tests need the symbols available directly.
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/code/signel")))
(require 'signel)

(require 'signal-config)

;;; cj/signal--format-notify-body

(ert-deftest test-signal-config-format-notify-body-passthrough ()
  "Normal: short single-line text passes through unchanged."
  (should (equal (cj/signal--format-notify-body "lunch at noon?")
                 "lunch at noon?")))

(ert-deftest test-signal-config-format-notify-body-collapses-whitespace ()
  "Normal: newlines and whitespace runs collapse to single spaces."
  (should (equal (cj/signal--format-notify-body "two\nlines\n\nhere")
                 "two lines here"))
  (should (equal (cj/signal--format-notify-body "tabs\t\tand   spaces")
                 "tabs and spaces")))

(ert-deftest test-signal-config-format-notify-body-trims ()
  "Boundary: leading and trailing whitespace is trimmed."
  (should (equal (cj/signal--format-notify-body "  hi  ") "hi")))

(ert-deftest test-signal-config-format-notify-body-empty ()
  "Boundary: the empty string stays empty."
  (should (equal (cj/signal--format-notify-body "") "")))

(ert-deftest test-signal-config-format-notify-body-exact-limit ()
  "Boundary: a body exactly at the limit is untouched."
  (let ((s (make-string cj/signal--notify-body-max ?x)))
    (should (equal (cj/signal--format-notify-body s) s))))

(ert-deftest test-signal-config-format-notify-body-truncates-over-limit ()
  "Boundary: over-limit text truncates to the limit, ending in an ellipsis."
  (let* ((s (make-string (1+ cj/signal--notify-body-max) ?x))
         (out (cj/signal--format-notify-body s)))
    (should (= (length out) cj/signal--notify-body-max))
    (should (string-suffix-p "…" out))))

(ert-deftest test-signal-config-format-notify-body-unicode ()
  "Boundary: multibyte text truncates by characters, not bytes."
  (let* ((s (make-string (+ cj/signal--notify-body-max 10) ?é))
         (out (cj/signal--format-notify-body s)))
    (should (= (length out) cj/signal--notify-body-max))
    (should (string-suffix-p "…" out))))

;;; cj/signel--notify routing

(ert-deftest test-signal-config-notify-suppressed-when-viewing ()
  "Normal: nothing fires when the suppression predicate says no."
  (let (script-calls fallback-calls)
    (cl-letf (((symbol-function 'cj/signal--should-notify-p)
               (lambda (_chat-id) nil))
              ((symbol-function 'start-process)
               (lambda (&rest args) (push args script-calls) nil))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (push args fallback-calls) nil)))
      (cj/signel--notify "+15551234567" "Alice" "hi"))
    (should-not script-calls)
    (should-not fallback-calls)))

(ert-deftest test-signal-config-notify-script-silent-by-default ()
  "Normal: with the script present and sound off, runs notify info --silent."
  (let (script-calls)
    (cl-letf (((symbol-function 'cj/signal--should-notify-p)
               (lambda (_chat-id) t))
              ((symbol-function 'executable-find)
               (lambda (p &optional _remote)
                 (when (equal p "notify") "/usr/bin/notify")))
              ((symbol-function 'start-process)
               (lambda (&rest args) (push args script-calls) nil))
              ((symbol-function 'notifications-notify)
               (lambda (&rest _)
                 (error "Fallback must not fire when the script is present"))))
      (let ((cj/signel-notify-sound nil))
        (cj/signel--notify "+15551234567" "Alice" "hi")))
    (should (= (length script-calls) 1))
    ;; start-process args: (NAME BUFFER PROGRAM &rest PROGRAM-ARGS);
    ;; PROGRAM is the path executable-find resolved, not the bare name.
    (should (equal (nthcdr 2 (car script-calls))
                   '("/usr/bin/notify" "info" "Signal: Alice" "hi" "--silent")))))

(ert-deftest test-signal-config-notify-sound-enabled-drops-silent ()
  "Normal: with `cj/signel-notify-sound' non-nil, --silent is omitted."
  (let (script-calls)
    (cl-letf (((symbol-function 'cj/signal--should-notify-p)
               (lambda (_chat-id) t))
              ((symbol-function 'executable-find)
               (lambda (p &optional _remote)
                 (when (equal p "notify") "/usr/bin/notify")))
              ((symbol-function 'start-process)
               (lambda (&rest args) (push args script-calls) nil)))
      (let ((cj/signel-notify-sound t))
        (cj/signel--notify "+15551234567" "Alice" "hi")))
    (should (equal (nthcdr 2 (car script-calls))
                   '("/usr/bin/notify" "info" "Signal: Alice" "hi")))))

(ert-deftest test-signal-config-notify-fallback-when-script-missing ()
  "Error: without the script on PATH, falls back to notifications-notify."
  (let (script-calls fallback-calls)
    (cl-letf (((symbol-function 'cj/signal--should-notify-p)
               (lambda (_chat-id) t))
              ((symbol-function 'executable-find)
               (lambda (_p &optional _remote) nil))
              ((symbol-function 'start-process)
               (lambda (&rest args) (push args script-calls) nil))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (push args fallback-calls) nil)))
      (cj/signel--notify "+15551234567" "Alice" "hi"))
    (should-not script-calls)
    (should (= (length fallback-calls) 1))
    (let ((args (car fallback-calls)))
      (should (equal (plist-get args :title) "Signal: Alice"))
      (should (equal (plist-get args :body) "hi")))))

(ert-deftest test-signal-config-notify-formats-body-before-send ()
  "Normal: the body runs through the formatter before reaching the script."
  (let (script-calls)
    (cl-letf (((symbol-function 'cj/signal--should-notify-p)
               (lambda (_chat-id) t))
              ((symbol-function 'executable-find)
               (lambda (p &optional _remote)
                 (when (equal p "notify") "/usr/bin/notify")))
              ((symbol-function 'start-process)
               (lambda (&rest args) (push args script-calls) nil)))
      (let ((cj/signel-notify-sound nil))
        (cj/signel--notify "+15551234567" "Alice" "first line\nsecond line")))
    (should (equal (nth 5 (car script-calls)) "first line second line"))))

(provide 'test-signal-config-notify)
;;; test-signal-config-notify.el ends here
