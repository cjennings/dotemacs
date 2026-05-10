;;; system-lib.el --- System utility library functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This module provides low-level system utility functions for checking
;; the availability of external programs and system capabilities.
;;
;; Functions include:
;; - Checking if external programs are available in PATH
;; - Silent logging to *Messages* buffer
;;
;;; Code:

(defun cj/executable-exists-p (program)
  "Return non-nil if PROGRAM is available in PATH.
PROGRAM should be a string naming an executable program."
  (and (stringp program)
       (not (string-empty-p program))
       (executable-find program)))

(defun cj/executable-find-or-warn (program feature &optional group)
  "Return PROGRAM's executable path, or warn that FEATURE is unavailable.

When PROGRAM is in PATH, return its absolute path.  When it isn't,
emit a `display-warning' naming PROGRAM and FEATURE so the user gets
a clear hint about what won't work, and return nil.

GROUP is the symbol passed to `display-warning' for filtering and
defaults to `cj/system-lib'.  Callers should pass their own module
symbol (for example `mail-config') so per-feature warning filters
keep working."
  (or (executable-find program)
      (progn
        (display-warning
         (or group 'cj/system-lib)
         (format "%s not found; %s unavailable" program feature)
         :warning)
        nil)))

(defconst cj/shell-safe-argument-regexp "\\`[[:alnum:]_./=+@%:,^-]+\\'"
  "Regexp matching shell arguments safe to interpolate unchanged.
Members of this character set survive shell parsing without quoting,
so a command line containing only these characters in each argument
remains both safe and readable.")

(defun cj/shell-quote-argument-readable (argument)
  "Quote ARGUMENT for shell command interpolation when needed.

When ARGUMENT consists only of characters in `cj/shell-safe-argument-regexp'
it is returned unchanged so the surrounding command stays human-readable
(useful for compile/test command lines you'll inspect in *compilation*).
Otherwise falls back to `shell-quote-argument' so the result is safe to
interpolate."
  (if (string-match-p cj/shell-safe-argument-regexp argument)
      argument
    (shell-quote-argument argument)))

(defun cj/process-output-or-error (program &rest args)
  "Run PROGRAM with ARGS via `process-file' and return stdout, or signal error.

On zero exit, returns the program's stdout as a string (including any
trailing newline -- callers that need a trimmed value should call
`string-trim' themselves).  On non-zero exit, signals `user-error' with
a message naming the program, the exit status, and the (trimmed) output
so a user inspecting *Messages* can see what went wrong."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args))
          (output (buffer-string)))
      (unless (zerop status)
        (user-error "%s %s failed with status %s: %s"
                    program
                    (string-join args " ")
                    status
                    (string-trim output)))
      output)))

(defun cj/git-output-or-error (&rest args)
  "Run git with ARGS and return stdout, or signal `user-error' on failure.

Thin wrapper around `cj/process-output-or-error' with `git' as the
program."
  (apply #'cj/process-output-or-error "git" args))

(defun cj/log-silently (format-string &rest args)
  "Append formatted message (FORMAT-STRING with ARGS) to *Messages* buffer.
This does so without echoing in the minibuffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (apply #'format format-string args))
      (unless (bolp) (insert "\n")))))

(provide 'system-lib)
;;; system-lib.el ends here
