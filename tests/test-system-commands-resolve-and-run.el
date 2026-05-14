;;; test-system-commands-resolve-and-run.el --- Tests for system-commands helpers + wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-system-commands-keymap.el' locks the keymap shape.
;; This file covers the runtime helpers and commands:
;;
;;   cj/system-cmd--resolve
;;   cj/system-cmd
;;   cj/system-cmd-exit-emacs
;;   cj/system-cmd-restart-emacs
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
  "Boundary: a confirm-tagged var with N response signals user-error."
  (defvar test-sc-confirm-cmd "test-confirm-cmd")
  (put 'test-sc-confirm-cmd 'cj/system-confirm t)
  (unwind-protect
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n))
                ((symbol-function 'start-process-shell-command)
                 (lambda (&rest _) (error "shouldn't run"))))
        (should-error (cj/system-cmd 'test-sc-confirm-cmd) :type 'user-error))
    (put 'test-sc-confirm-cmd 'cj/system-confirm nil)))

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

(ert-deftest test-system-cmd-restart-emacs-decline-aborts ()
  "Boundary: declining the prompt signals user-error before scheduling."
  (let ((scheduled nil))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n))
              ((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled t))))
      (should-error (cj/system-cmd-restart-emacs) :type 'user-error))
    (should-not scheduled)))

(ert-deftest test-system-cmd-restart-emacs-accept-schedules-restart ()
  "Normal: accepting the prompt schedules the restart + kill timers."
  (let ((schedules 0))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y))
              ((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (cl-incf schedules)))
              ((symbol-function 'message) #'ignore))
      (cj/system-cmd-restart-emacs))
    (should (= schedules 2))))

;;; cj/system-command-menu

(ert-deftest test-system-command-menu-dispatches-by-name ()
  "Normal: the completing-read selection routes through `call-interactively'."
  (let ((called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Lock Screen"))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called cmd))))
      (cj/system-command-menu))
    (should (eq called 'cj/system-cmd-lock))))

(provide 'test-system-commands-resolve-and-run)
;;; test-system-commands-resolve-and-run.el ends here
