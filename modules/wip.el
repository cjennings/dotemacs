;;; wip.el --- test code -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Work-in-progress and experimental code testing area. This file contains
;; code that is being actively developed or tested before being promoted
;; to stable configuration modules. Functions here may be incomplete,
;; buggy, or subject to significant changes. Include this file at the end
;; of init.el so that if something breaks, most of the Emacs config has
;; already loaded. Once code has been tested and proven stable, graduate
;; it into the appropriate configuration module. Do not rely on this code
;; for production use.
;;
;;; Code:

(eval-when-compile (require 'user-constants))
(eval-when-compile (require 'keybindings))
(eval-when-compile (require 'subr-x)) ;; for system commands
(require 'rx) ;; for system commands

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
                      (format "Run %s now (%s)? (Y/n) " label camdstr)
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

;; (defvar-keymap cj/system-command-map
;;   :doc "Keymap for system commands."
;;   "L" #'cj/system-cmd-logout
;;   "r" #'cj/system-cmd-reboot
;;   "s" #'cj/system-cmd-shutdown
;;   "S" #'cj/system-cmd-suspend
;;   "l" #'cj/system-cmd-lock
;;   "E" #'cj/system-cmd-exit-emacs
;;   "e" #'cj/system-cmd-restart-emacs)
;; (keymap-set cj/custom-keymap "!" cj/system-command-map)

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


;; --------------------------- Org Upcoming Modeline ---------------------------

;; (use-package org-upcoming-modeline
;;   :after org
;;   :load-path "~/code/org-upcoming-modeline/org-upcoming-modeline.el"
;;   :config
;;   (setq org-upcoming-modeline-keep-late 300)
;;   (setq org-upcoming-modeline-ignored-keywords '("DONE" "CANCELLED" "FAILED"))
;;   (setq org-upcoming-modeline-trim 30)
;;   (setq org-upcoming-modeline-days-ahead 5)
;;   (setq org-upcoming-modeline-format (lambda (ms mh) (format "📅 %s %s" ms mh)))
;;   (org-upcoming-modeline-mode))

;; ----------------------------------- Efrit -----------------------------------
;; not working as of Wednesday, September 03, 2025 at 12:44:09 AM CDT

;; (add-to-list 'load-path "~/code/efrit/lisp")
;; (require 'efrit)

;; ------------------------------ Buffer Same Mode -----------------------------

;; (defun cj/buffer-same-mode (&rest modes)
;;   "Pop to a buffer with a mode among MODES, or the current one if not given."
;;   (interactive)
;;   (let* ((modes (or modes (list major-mode)))
;;          (pred (lambda (b)
;;                  (let ((b (get-buffer (if (consp b) (car b) b))))
;;                    (member (buffer-local-value 'major-mode b) modes)))))
;;     (pop-to-buffer (read-buffer "Buffer: " nil t pred))))
;; (keymap-global-set "C-x B" #'cj/buffer-same-mode)

;; ;; --------------------------------- Easy Hugo ---------------------------------

;; (use-package easy-hugo
;;   :defer .5
;;   :init
;;   (setq easy-hugo-basedir "~/code/cjennings-net/")
;;   (setq easy-hugo-url "https://cjennings.net")
;;   (setq easy-hugo-sshdomain "cjennings.net")
;;   (setq easy-hugo-root "/var/www/cjennings/")
;;   (setq easy-hugo-previewtime "300")
;;   (setq easy-hugo-postdir "content")
;;   (setq easy-hugo-server-flags "-D --noHTTPCache --disableFastRender")
;;   (setq easy-hugo-default-ext ".md")
;;   :bind ("C-c H" . easy-hugo)
;;   :config
;;   (easy-hugo-enable-menu))

;; ------------------------------------ Pomm -----------------------------------

(use-package pomm
  :defer .5
  :bind ("M-p" . pomm)
  :commands (pomm pomm-third-time))

;; ----------------------------------- Popper ----------------------------------

;; (use-package popper
;;   :bind (("C-`"   . popper-toggle)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :custom
;;   (popper-display-control-nil)
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           "\\*Async Shell Command\\*"
;; ;;          "\\*scratch\\*"
;;           help-mode
;;           compilation-mode))
;;   (add-to-list 'display-buffer-alist
;;                '(popper-display-control-p  ; Predicate to match popper buffers
;;                  (display-buffer-in-side-window)
;;                  (side . bottom)
;;                  (slot . 0)
;;                  (window-height . 0.5)))  ; Half the frame height
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

(provide 'wip)
;;; wip.el ends here.
