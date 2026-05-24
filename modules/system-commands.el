;;; system-commands.el --- System power and session management -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: eager.
;; Eager reason: registers the C-; ! system-command keymap; high-impact commands
;;   that should run only by command (command-loaded target).
;; Top-level side effects: defines a system-command keymap under cj/custom-keymap.
;; Runtime requires: keybindings, rx.
;; Direct test load: yes (requires keybindings explicitly).
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

;; `keybindings' provides `cj/custom-keymap', which is referenced at load
;; time by the `keymap-set' call at the tail of this file.  An
;; `eval-when-compile' require would silence the byte-compiler but leave
;; the load-time reference void if anything required `system-commands'
;; before `keybindings'.  Make the dependency explicit.
(require 'keybindings)
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
  (interactive "sSystem command: ")
  ;; Plain `let*' + `nth' instead of `pcase-let' with a backquote
  ;; destructure: edebug-based coverage tools (undercover.el) don't
  ;; instrument inside backquote-destructured `pcase-let' bindings,
  ;; so the body shows as uncovered even when tests exercise it.
  (let* ((resolved (cj/system-cmd--resolve cmd))
         (sym (nth 0 resolved))
         (cmdstr (nth 1 resolved))
         (label (nth 2 resolved)))
    (let ((confirm (and sym (get sym 'cj/system-confirm))))
      (cond
       ;; Strong confirm for irreversible actions (shutdown, reboot):
       ;; require an explicit "yes", so a stray RET/space can't trigger them.
       ((eq confirm 'strong)
        (unless (yes-or-no-p (format "Really run %s (%s)? " label cmdstr))
          (user-error "Aborted")))
       ;; Quick (Y/n) confirm for recoverable actions (logout, suspend).
       (confirm
        (when (memq (read-char-choice
                     (format "Run %s now (%s)? (Y/n) " label cmdstr)
                     '(?y ?Y ?n ?N ?\r ?\n ?\s))
                    '(?n ?N))
          (user-error "Aborted")))))
    (let ((proc (start-process-shell-command "cj/system-cmd" nil
                                             (format "nohup %s >/dev/null 2>&1 &" cmdstr))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'ignore)
      (message "Running %s..." label))))

(defmacro cj/defsystem-command (name var cmdstr &optional confirm)
  "Define VAR with CMDSTR and interactive command NAME to run it.
CONFIRM controls the confirmation prompt: t for a quick (Y/n) prompt,
the symbol `strong' for an explicit yes-or-no-p (used for irreversible
actions like shutdown and reboot), nil for no confirmation."
  (declare (indent defun))
  `(progn
     (defvar ,var ,cmdstr)
     ,(when confirm `(put ',var 'cj/system-confirm ',confirm))
     (defun ,name ()
       ,(format "Run %s via `cj/system-cmd'." var)
       (interactive)
       (cj/system-cmd ',var))))

;; Define system commands
(cj/defsystem-command cj/system-cmd-logout   logout-cmd "loginctl terminate-user $(whoami)" t)
(cj/defsystem-command cj/system-cmd-lock     lockscreen-cmd "slock")
(cj/defsystem-command cj/system-cmd-suspend  suspend-cmd "systemctl suspend" t)
(cj/defsystem-command cj/system-cmd-shutdown shutdown-cmd "systemctl poweroff" strong)
(cj/defsystem-command cj/system-cmd-reboot   reboot-cmd "systemctl reboot" strong)

(defun cj/system-cmd-exit-emacs ()
  "Exit Emacs server and all clients."
  (interactive)
  (when (memq (read-char-choice
               "Exit Emacs? (Y/n) "
               '(?y ?Y ?n ?N ?\r ?\n ?\s))
              '(?n ?N))
    (user-error "Aborted"))
  (kill-emacs))

(defun cj/system-cmd--emacs-service-available-p ()
  "Return non-nil if a systemd --user emacs.service unit is present.
Used to decide whether `cj/system-cmd-restart-emacs' can restart via the
service before relying on it to cycle the daemon.  `systemctl --user cat'
exits 0 when the unit exists, nonzero otherwise."
  (and (executable-find "systemctl")
       (eq 0 (call-process "systemctl" nil nil nil
                           "--user" "cat" "emacs.service"))))

(defun cj/system-cmd-restart-emacs ()
  "Restart the Emacs daemon via its systemd --user service, then reconnect.
Aborts without terminating anything when not running as a daemon or when
no emacs.service is present, so a missing or failed service can't leave
you with no Emacs running.  The service owns the daemon lifecycle, so
there is no separate `kill-emacs': a failed restart leaves the current
daemon alive rather than killing the session blindly."
  (interactive)
  (unless (daemonp)
    (user-error "Not running as an Emacs daemon; restart Emacs manually"))
  (unless (cj/system-cmd--emacs-service-available-p)
    (user-error "No systemd --user emacs.service found; restart it manually"))
  (when (memq (read-char-choice
               "Restart Emacs? (Y/n) "
               '(?y ?Y ?n ?N ?\r ?\n ?\s))
              '(?n ?N))
    (user-error "Aborted"))
  (save-some-buffers)
  ;; Hand the whole restart+reconnect to a detached shell.  `systemctl
  ;; restart' tears down this daemon itself; the detached `emacsclient -c'
  ;; reconnects to the fresh one.
  (call-process-shell-command
   (concat "nohup sh -c 'systemctl --user restart emacs.service "
           "&& sleep 1 && emacsclient -c' >/dev/null 2>&1 &")
   nil 0)
  (message "Restarting Emacs via emacs.service..."))

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

(defvar-keymap cj/system-command-map
  :doc "Keymap for system commands."
  "!" #'cj/system-command-menu
  "L" #'cj/system-cmd-logout
  "r" #'cj/system-cmd-reboot
  "s" #'cj/system-cmd-shutdown
  "S" #'cj/system-cmd-suspend
  "l" #'cj/system-cmd-lock
  "E" #'cj/system-cmd-exit-emacs
  "e" #'cj/system-cmd-restart-emacs)
(keymap-set cj/custom-keymap "!" cj/system-command-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; !" "system commands"
    "C-; ! !" "system command menu"
    "C-; ! L" "logout"
    "C-; ! E" "exit Emacs"
    "C-; ! S" "suspend"
    "C-; ! e" "restart Emacs"
    "C-; ! l" "lock screen"
    "C-; ! r" "reboot"
    "C-; ! s" "shutdown"))

(provide 'system-commands)
;;; system-commands.el ends here
