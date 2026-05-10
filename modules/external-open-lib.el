;;; external-open-lib.el --- Pure helpers for OS open-with dispatch -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Pure helpers for resolving the OS-default "open" command and
;; recognizing desktop launchers.  No side effects, no state.  The
;; feature module (`external-open.el') uses these helpers; consumers
;; that only need the dispatch (system-utils' "open with command",
;; dirvish's "open file manager here") require this library directly
;; instead of the feature module.
;;
;; Pulled out of `external-open.el' as part of utility-consolidation
;; Phase 4.  See `docs/design/utility-consolidation.org'.

;;; Code:

(require 'host-environment)

(defun cj/external-open-command ()
  "Return the OS-default \"open\" command for this host, or nil if unsupported.
Returns one of \"xdg-open\" (Linux), \"open\" (macOS), \"start\" (Windows).
Callers that require a command should error on nil with a contextual
message so the user sees what feature is unavailable."
  (cond
   ((env-linux-p)   "xdg-open")
   ((env-macos-p)   "open")
   ((env-windows-p) "start")
   (t nil)))

(defun cj/external-open-launcher-p (command)
  "Return non-nil when COMMAND is a desktop launcher.
Launchers (xdg-open, open, start) need to be called with `call-process'
and a zero BUFFER argument so they fully detach from Emacs.  Other
commands get `start-process-shell-command' so their output is visible."
  (and (stringp command)
       (member command '("xdg-open" "open" "start"))
       t))

(provide 'external-open-lib)
;;; external-open-lib.el ends here
