;;; system-lib.el --- System utility library functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Layer: 1 (Foundation).
;; Category: F/L.
;; Load shape: eager.
;; Eager reason: low-level helpers (executable lookup, process output, silent
;;   logging) used by many eager modules during startup.
;; Top-level side effects: none.
;; Runtime requires: none (auth-source loaded on demand inside the helper).
;; Direct test load: yes (pure helpers; batch-safe).
;;
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

(defun cj/file-from-context (&optional explicit-filename)
  "Return a file path from the current context, or nil.

Resolves in priority order:
  1. EXPLICIT-FILENAME, if non-nil.
  2. `buffer-file-name' of the current buffer.
  3. The file at point if the current buffer is in `dired-mode'.

Returns nil when none of these yield a file.  Useful for any command
that operates on \"the current file\" -- buffer commands, dired
commands, and external-open dispatchers all want this resolution."
  (or explicit-filename
      buffer-file-name
      (and (derived-mode-p 'dired-mode)
           (dired-file-name-at-point))))

(defun cj/log-silently (format-string &rest args)
  "Append formatted message (FORMAT-STRING with ARGS) to *Messages* buffer.
This does so without echoing in the minibuffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (apply #'format format-string args))
      (unless (bolp) (insert "\n")))))

;; ------------------------------ Auth Source ----------------------------------

(declare-function auth-source-search "auth-source")

(defun cj/auth-source-secret-value (host &optional user)
  "Return the auth-source secret for HOST, or nil when none is found.
With USER, also match on the login.  Resolves a function-valued secret
\(the netrc backend returns the secret as a function\) by calling it.
Callers that must have a secret layer their own error on top."
  (let* ((spec (append (list :host host :require '(:secret) :max 1)
                       (when user (list :user user))))
         (secret (plist-get (car (apply #'auth-source-search spec)) :secret)))
    (if (functionp secret) (funcall secret) secret)))

;; ---------------------------- Strong Confirmation ----------------------------

(defun cj/confirm-strong (prompt)
  "Ask PROMPT, requiring a full typed \"yes\" or \"no\" answer.
For irreversible actions -- file destruction, overwrites, power-off.  The
global default makes `yes-or-no-p' a single keystroke (`use-short-answers'
is t); this binds it to nil for the one call so the prompt demands the
long-form answer, keeping a stray RET or space from confirming."
  (let ((use-short-answers nil))
    (yes-or-no-p prompt)))

(defun cj/--font-lock-global-modes-excluding (current mode)
  "Return CURRENT `font-lock-global-modes' with MODE added to the exclusion.
CURRENT has one of three shapes: t (font-lock on in all modes), a
\(not M...) exclusion list, or an (M...) inclusion list.  Pure: returns
the new value and mutates nothing."
  (cond
   ((eq current t) (list 'not mode))
   ((and (consp current) (eq (car current) 'not))
    (if (memq mode (cdr current)) current
      (cons 'not (cons mode (cdr current)))))
   ((consp current) (delq mode (copy-sequence current)))
   (t current)))

(defun cj/exclude-from-global-font-lock (&rest modes)
  "Exclude MODES from `global-font-lock-mode'.
Some major modes (dashboard, mu4e) paint their buffers with manual `face'
text properties; global font-lock then strips those, leaving the buffer
unthemed.  Excluding the mode keeps its faces.  Additive, so each caller
contributes its own modes regardless of load order."
  (dolist (mode modes)
    (setq font-lock-global-modes
          (cj/--font-lock-global-modes-excluding font-lock-global-modes mode))))

(defun cj/completion-table (category collection)
  "Return a completion table over COLLECTION tagged with completion CATEGORY.
COLLECTION is anything `completing-read' accepts (list, alist, obarray, hash
table, or another table).  The table reports CATEGORY in its metadata so
marginalia (and embark, consult, sorting) can recognize and annotate the
candidates.  Use a standard category (file, buffer, function, theme, ...) when
the candidates match one; marginalia then annotates them with no further work."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action collection string predicate))))

(defun cj/completion-table-annotated (category annotate collection)
  "Like `cj/completion-table' but also attach ANNOTATE as the annotation function.
ANNOTATE is called with a candidate string and returns its annotation suffix, or
nil.  Use this for a custom CATEGORY that marginalia has no built-in annotator
for: marginalia falls back to the table's own annotation function."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (annotation-function . ,annotate))
      (complete-with-action action collection string predicate))))

(defun cj/completion-file-annotator (candidate->path)
  "Return an annotation function for completion candidates backed by files.
CANDIDATE->PATH maps a candidate string to its absolute file path, or nil when
the candidate has no backing file.  The returned function, suitable as a
completion table's annotation function (see `cj/completion-table-annotated'),
yields a suffix with the file size and modification date for a regular file,
the marker \"dir\" plus the date for a directory, or nil when the path is nil
or the file is missing -- so marginalia then shows no suffix for that
candidate."
  (lambda (cand)
    (let ((path (funcall candidate->path cand)))
      (when (and path (file-exists-p path))
        (let* ((attrs (file-attributes path))
               (dirp  (eq t (file-attribute-type attrs)))
               (size  (if dirp "dir"
                        (file-size-human-readable (file-attribute-size attrs))))
               (date  (format-time-string
                       "%Y-%m-%d"
                       (file-attribute-modification-time attrs))))
          (format "  %8s  %s" size date))))))

(defun cj/format-region-with-program (program &rest args)
  "Replace the current buffer with PROGRAM ARGS run over its contents, via argv.
Runs PROGRAM (with ARGS) on the whole buffer through `call-process-region'
-- no shell, so no quoting or word-splitting.  The buffer is replaced only
when PROGRAM exits zero; on a non-zero exit the buffer is left untouched and
a `user-error' is signalled with the program's stderr text.  Point is
preserved as closely as the reformatted size allows.  Returns t on success."
  (let* ((point (point))
         (src (current-buffer))
         (out (generate-new-buffer " *format-out*"))
         (status (apply #'call-process-region
                        (point-min) (point-max) program
                        nil out nil args)))
    (unwind-protect
        (if (and (integerp status) (zerop status))
            (progn
              (with-current-buffer src
                (replace-buffer-contents out)
                (goto-char (min point (point-max))))
              t)
          (user-error "%s failed: %s" program
                      (string-trim (with-current-buffer out (buffer-string)))))
      (kill-buffer out))))

(provide 'system-lib)
;;; system-lib.el ends here
