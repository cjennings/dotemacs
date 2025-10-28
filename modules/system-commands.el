;;; system-commands.el --- System power and session management -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; System commands for logout, lock, suspend, shutdown, reboot, and Emacs
;; exit/restart. Provides both a keymap (C-; !) and a completing-read menu.
;;
;; Commands include:
;; - Logout (terminate user session)
;; - Lock screen (slock)
;; - Suspend (systemctl suspend)
;; - Shutdown (systemctl poweroff)
;; - Reboot (systemctl reboot)
;; - Exit Emacs (kill-emacs)
;; - Restart Emacs (via systemctl --user restart emacs.service)
;;
;; Dangerous commands (logout, suspend, shutdown, reboot) require confirmation.
;;
;;; Code:

(eval-when-compile (require 'keybindings))
(eval-when-compile (require 'subr-x))
(require 'rx)

;; ------------------------------ System Commands ------------------------------

(defun cj/system-cmd--resolve (cmd)
  "Return (values symbol-or-nil command-string label) for CMD."
  (cond
   ((symbolp cmd)
    (let ((val (and (boundp cmd) (symbol-value cmd))))
      (unless (and (stringp val) (not (string-empty-p val)))
        (user-error "Variable %s is not a non-empty string" cmd))
      (list cmd val (symbol-name cmd))))
   ((stringp cmd)
    (let ((s (string-trim cmd)))
      (when (string-empty-p s) (user-error "Command string is empty"))
      (list nil s "command")))
   (t (user-error "Error: cj/system-cmd expects a string or a symbol"))))

(defun cj/system-cmd (cmd)
  "Run CMD (string or symbol naming a string) detached via the shell.
Shell expansions like $(...) are supported. Output is silenced.
If CMD is deemed dangerous, ask for confirmation."
  (interactive (list (read-shell-command "System command: ")))
  (pcase-let ((`(,sym ,cmdstr ,label) (cj/system-cmd--resolve cmd)))
    (when (and sym (get sym 'cj/system-confirm)
               (memq (read-char-choice
                      (format "Run %s now (%s)? (Y/n) " label cmdstr)
                      '(?y ?Y ?n ?N ?\r ?\n ?\s))
                     '(?n ?N)))
      (user-error "Aborted"))
    (let ((proc (start-process-shell-command "cj/system-cmd" nil
                                             (format "nohup %s >/dev/null 2>&1 &" cmdstr))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'ignore)
      (message "Running %s..." label))))

(defmacro cj/defsystem-command (name var cmdstr &optional confirm)
  "Define VAR with CMDSTR and interactive command NAME to run it.
If CONFIRM is non-nil, mark VAR to always require confirmation."
  (declare (indent defun))
  `(progn
     (defvar ,var ,cmdstr)
     ,(when confirm `(put ',var 'cj/system-confirm t))
     (defun ,name ()
       ,(format "Run %s via `cj/system-cmd'." var)
       (interactive)
       (cj/system-cmd ',var))))

;; Define system commands
(cj/defsystem-command cj/system-cmd-logout   logout-cmd "loginctl terminate-user $(whoami)" t)
(cj/defsystem-command cj/system-cmd-lock     lockscreen-cmd "slock")
(cj/defsystem-command cj/system-cmd-suspend  suspend-cmd "systemctl suspend" t)
(cj/defsystem-command cj/system-cmd-shutdown shutdown-cmd "systemctl poweroff" t)
(cj/defsystem-command cj/system-cmd-reboot   reboot-cmd "systemctl reboot" t)

(defun cj/system-cmd-exit-emacs ()
  "Exit Emacs server and all clients."
  (interactive)
  (when (memq (read-char-choice
               "Exit Emacs? (Y/n) "
               '(?y ?Y ?n ?N ?\r ?\n ?\s))
              '(?n ?N))
    (user-error "Aborted"))
  (kill-emacs))

(defun cj/system-cmd-restart-emacs ()
  "Restart Emacs server after saving buffers."
  (interactive)
  (when (memq (read-char-choice
               "Restart Emacs? (Y/n) "
               '(?y ?Y ?n ?N ?\r ?\n ?\s))
              '(?n ?N))
    (user-error "Aborted"))
  (save-some-buffers)
  ;; Start the restart process before killing Emacs
  (run-at-time 0.5 nil
               (lambda ()
                 (call-process-shell-command
                  "systemctl --user restart emacs.service && emacsclient -c"
                  nil 0)))
  (run-at-time 1 nil #'kill-emacs)
  (message "Restarting Emacs..."))

(defvar-keymap cj/system-command-map
  :doc "Keymap for system commands."
  "L" #'cj/system-cmd-logout
  "r" #'cj/system-cmd-reboot
  "s" #'cj/system-cmd-shutdown
  "S" #'cj/system-cmd-suspend
  "l" #'cj/system-cmd-lock
  "E" #'cj/system-cmd-exit-emacs
  "e" #'cj/system-cmd-restart-emacs)
(keymap-set cj/custom-keymap "!" cj/system-command-map)

(defun cj/system-command-menu ()
  "Present system commands via \='completing-read\='."
  (interactive)
  (let* ((commands '(("Logout System"   . cj/system-cmd-logout)
                     ("Lock Screen"     . cj/system-cmd-lock)
                     ("Suspend System"  . cj/system-cmd-suspend)
                     ("Shutdown System" . cj/system-cmd-shutdown)
                     ("Reboot System"   . cj/system-cmd-reboot)
                     ("Exit Emacs"      . cj/system-cmd-exit-emacs)
                     ("Restart Emacs"   . cj/system-cmd-restart-emacs)))
         (choice (completing-read "System command: " commands nil t)))
    (when-let ((cmd (alist-get choice commands nil nil #'equal)))
      (call-interactively cmd))))

(keymap-set cj/custom-keymap "!" #'cj/system-command-menu)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; !" "system commands"))

(provide 'system-commands)
;;; system-commands.el ends here
