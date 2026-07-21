;;; test-system-commands-resolve-and-run.el --- Tests for system-commands helpers + wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-system-commands-keymap.el' locks the keymap shape.
;; This file covers the runtime helpers and commands:
;;
;;   cj/system-cmd--resolve
;;   cj/system-cmd                       (quick + strong confirm)
;;   cj/system-cmd--emacs-service-available-p
;;   cj/system-cmd-exit-emacs
;;   cj/system-cmd-restart-emacs         (daemon + service guards)
;;   cj/system-command-menu
;;
;; Process and prompt primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-commands)

;; Top-level defvars so `let' bindings reach `symbol-value' under
;; lexical scope.
(defvar test-sc-my-cmd nil
  "Dynamic test var for `cj/system-cmd--resolve' symbol-resolution test.")
(defvar test-sc-empty-cmd nil
  "Dynamic test var for the empty-value test.")

;;; cj/system-cmd--resolve

(ert-deftest test-system-cmd-resolve-string-trims-and-returns ()
  "Normal: a non-empty string comes back trimmed with a nil symbol slot."
  (let ((result (cj/system-cmd--resolve "  echo hi  ")))
    (should (null (nth 0 result)))
    (should (equal (nth 1 result) "echo hi"))
    (should (equal (nth 2 result) "command"))))

(ert-deftest test-system-cmd-resolve-empty-string-errors ()
  "Error: an empty string signals user-error."
  (should-error (cj/system-cmd--resolve "   ") :type 'user-error))

(ert-deftest test-system-cmd-resolve-symbol-uses-its-value ()
  "Normal: a symbol whose value is a non-empty string round-trips."
  (let ((test-sc-my-cmd "echo hello"))
    (let ((result (cj/system-cmd--resolve 'test-sc-my-cmd)))
      (should (eq (nth 0 result) 'test-sc-my-cmd))
      (should (equal (nth 1 result) "echo hello"))
      (should (equal (nth 2 result) "test-sc-my-cmd")))))

(ert-deftest test-system-cmd-resolve-symbol-with-empty-value-errors ()
  "Error: a symbol whose value isn't a non-empty string signals user-error."
  (let ((test-sc-empty-cmd ""))
    (should-error (cj/system-cmd--resolve 'test-sc-empty-cmd) :type 'user-error)))

(ert-deftest test-system-cmd-resolve-non-string-non-symbol-errors ()
  "Error: passing a number signals user-error."
  (should-error (cj/system-cmd--resolve 42) :type 'user-error))

;;; cj/system-cmd

(ert-deftest test-system-cmd-string-runs-shell-process ()
  "Normal: a plain string is wrapped in nohup and handed to
`start-process-shell-command'."
  (let (cmd-line)
    (cl-letf (((symbol-function 'start-process-shell-command)
               (lambda (_name _buf c) (setq cmd-line c) 'fake-proc))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore))
      (cj/system-cmd "echo hi"))
    (should (string-match-p "^nohup echo hi" cmd-line))
    (should (string-match-p "&$" cmd-line))))

(ert-deftest test-system-cmd-confirm-decline-aborts ()
  "Boundary: a quick-confirm var with N response signals user-error."
  (defvar test-sc-confirm-cmd "test-confirm-cmd")
  (put 'test-sc-confirm-cmd 'cj/system-confirm t)
  (unwind-protect
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n))
                ((symbol-function 'start-process-shell-command)
                 (lambda (&rest _) (error "shouldn't run"))))
        (should-error (cj/system-cmd 'test-sc-confirm-cmd) :type 'user-error))
    (put 'test-sc-confirm-cmd 'cj/system-confirm nil)))

(ert-deftest test-system-cmd-strong-confirm-decline-aborts ()
  "Boundary: a strong-confirm var uses yes-or-no-p; declining aborts and
does not run the command."
  (defvar test-sc-strong-cmd "test-strong-cmd")
  (put 'test-sc-strong-cmd 'cj/system-confirm 'strong)
  (unwind-protect
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'read-char-choice)
                 (lambda (&rest _) (error "strong confirm must not use read-char-choice")))
                ((symbol-function 'start-process-shell-command)
                 (lambda (&rest _) (error "shouldn't run"))))
        (should-error (cj/system-cmd 'test-sc-strong-cmd) :type 'user-error))
    (put 'test-sc-strong-cmd 'cj/system-confirm nil)))

(ert-deftest test-system-cmd-strong-confirm-accept-runs ()
  "Normal: a strong-confirm var runs the command when yes-or-no-p returns t."
  (defvar test-sc-strong-cmd-2 "echo strong")
  (put 'test-sc-strong-cmd-2 'cj/system-confirm 'strong)
  (let (cmd-line)
    (unwind-protect
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buf c) (setq cmd-line c) 'fake-proc))
                  ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (cj/system-cmd 'test-sc-strong-cmd-2))
      (put 'test-sc-strong-cmd-2 'cj/system-confirm nil))
    (should (string-match-p "echo strong" cmd-line))))

;;; cj/system-cmd--emacs-service-available-p

(ert-deftest test-system-cmd-service-available-true-on-zero-exit ()
  "Normal: service is available when systemctl exists and `cat' exits 0."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p &rest _) "/usr/bin/systemctl"))
            ((symbol-function 'call-process) (lambda (&rest _) 0)))
    (should (cj/system-cmd--emacs-service-available-p))))

(ert-deftest test-system-cmd-service-available-false-on-nonzero-exit ()
  "Boundary: a nonzero exit (no such unit) means not available."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p &rest _) "/usr/bin/systemctl"))
            ((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should-not (cj/system-cmd--emacs-service-available-p))))

(ert-deftest test-system-cmd-service-available-false-when-systemctl-absent ()
  "Error: with no systemctl on PATH the service can't be available."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p &rest _) nil))
            ((symbol-function 'call-process)
             (lambda (&rest _) (error "must not shell out without systemctl"))))
    (should-not (cj/system-cmd--emacs-service-available-p))))

;;; cj/system-cmd-exit-emacs

(ert-deftest test-system-cmd-exit-emacs-decline-aborts ()
  "Boundary: declining the prompt signals user-error; kill-emacs is not called."
  (let ((killed nil))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (setq killed t))))
      (should-error (cj/system-cmd-exit-emacs) :type 'user-error))
    (should-not killed)))

(ert-deftest test-system-cmd-exit-emacs-accept-calls-kill-emacs ()
  "Normal: accepting the prompt calls `kill-emacs'."
  (let ((killed nil))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (setq killed t))))
      (cj/system-cmd-exit-emacs))
    (should killed)))

;;; cj/system-cmd-restart-emacs

(ert-deftest test-system-cmd-restart-emacs-not-daemon-aborts ()
  "Error: a non-daemon Emacs refuses to restart-via-service and never prompts."
  (let ((prompted nil) (ran nil))
    (cl-letf (((symbol-function 'daemonp) (lambda () nil))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (setq prompted t) ?y))
              ((symbol-function 'call-process-shell-command)
               (lambda (&rest _) (setq ran t))))
      (should-error (cj/system-cmd-restart-emacs) :type 'user-error))
    (should-not prompted)
    (should-not ran)))

(ert-deftest test-system-cmd-restart-emacs-no-service-aborts ()
  "Error: when no emacs.service exists, restart aborts without running anything."
  (let ((ran nil))
    ;; Drive the real service check to nil at its boundary (no systemctl on
    ;; PATH) rather than mocking cj/system-cmd--emacs-service-available-p
    ;; itself: cj/system-cmd-restart-emacs reaches that helper through a
    ;; native-comp intra-file direct call that bypasses a symbol-function
    ;; redefinition, so the helper-level mock silently no-ops and the real
    ;; check passes on a machine that has emacs.service.  executable-find is a
    ;; subr the helper calls, and its trampoline honors the cl-letf swap.
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'read-char-choice) (lambda (&rest _) ?y))
              ((symbol-function 'call-process-shell-command)
               (lambda (&rest _) (setq ran t))))
      (should-error (cj/system-cmd-restart-emacs) :type 'user-error))
    (should-not ran)))

(ert-deftest test-system-cmd-restart-emacs-decline-aborts ()
  "Boundary: declining the prompt aborts before running the restart."
  (let ((ran nil))
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'cj/system-cmd--emacs-service-available-p)
               (lambda () t))
              ((symbol-function 'read-char-choice) (lambda (&rest _) ?n))
              ((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'call-process-shell-command)
               (lambda (&rest _) (setq ran t))))
      (should-error (cj/system-cmd-restart-emacs) :type 'user-error))
    (should-not ran)))

(ert-deftest test-system-cmd-restart-emacs-accept-runs-service-restart ()
  "Normal: accepting runs the systemctl restart line and never calls
kill-emacs directly (the service owns the daemon lifecycle)."
  (let (cmd-line (killed nil))
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'cj/system-cmd--emacs-service-available-p)
               (lambda () t))
              ((symbol-function 'read-char-choice) (lambda (&rest _) ?y))
              ((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (setq killed t)))
              ((symbol-function 'call-process-shell-command)
               (lambda (c &rest _) (setq cmd-line c))))
      (cj/system-cmd-restart-emacs))
    (should (string-match-p "systemctl --user restart emacs.service" cmd-line))
    (should-not killed)))

;;; cj/system-command-menu

(ert-deftest test-system-command-menu-dispatches-by-name ()
  "Normal: the completing-read selection routes through `call-interactively'."
  (let ((called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Lock Screen"))
              ((symbol-function 'call-interactively)
               (lambda (cmd &rest _) (setq called cmd))))
      (cj/system-command-menu))
    (should (eq called 'cj/system-cmd-lock))))

;;; Lock command resolves the locker at call time

(defun test-system-cmd--run-lock-capturing ()
  "Run the lock command; return the shell command line it launched."
  (let (cmd-line)
    (cl-letf (((symbol-function 'start-process-shell-command)
               (lambda (_name _buf c) (setq cmd-line c) 'fake-proc))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore))
      (cj/system-cmd-lock))
    cmd-line))

(ert-deftest test-system-cmd-lock-follows-session-type-at-call-time ()
  "Normal: the locker tracks the live session type, not the load-time bake.
A daemon started before WAYLAND_DISPLAY was imported used to freeze the
locker to slock forever; Lock then failed silently on Wayland."
  (let ((lockscreen-cmd nil))
    (cl-letf (((symbol-function 'env-wayland-p) (lambda () t)))
      (should (string-match-p "loginctl lock-session"
                              (test-system-cmd--run-lock-capturing))))
    (cl-letf (((symbol-function 'env-wayland-p) (lambda () nil)))
      (should (string-match-p "slock"
                              (test-system-cmd--run-lock-capturing))))))

(ert-deftest test-system-cmd-lock-explicit-override-wins ()
  "Boundary: a user-set lockscreen-cmd overrides the session-type resolution."
  (let ((lockscreen-cmd "my-locker --now"))
    (cl-letf (((symbol-function 'env-wayland-p) (lambda () t)))
      (should (string-match-p "my-locker --now"
                              (test-system-cmd--run-lock-capturing))))))

(provide 'test-system-commands-resolve-and-run)
;;; test-system-commands-resolve-and-run.el ends here
