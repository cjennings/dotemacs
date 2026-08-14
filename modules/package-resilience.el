;;; package-resilience.el --- Survive failed package installs at startup -*- lexical-binding: t -*-

;;; Commentary:
;; A transient package download must not abort init.
;;
;; `use-package-ensure-elpa' already handles a failed install correctly: it
;; wraps `package-install' in `condition-case-unless-debug', and on error it
;; warns and carries on.  That guard does nothing whenever `debug-on-error' is
;; non-nil, and early-init.el sets `debug-on-error' for the whole of startup so
;; my own config errors are loud.  The two settings collide.  On a fresh
;; install one dead download — a file-error from an ELPA host — escaped into
;; the debugger and stopped init in place, leaving a third of the config
;; loaded and hooks pointing at packages that were never installed.
;;
;; I keep both behaviors by narrowing the loud-errors setting rather than
;; dropping it: package installation runs with the debugger inhibited,
;; everything else in init still gets it.  A package that will not install is
;; recorded and reported at the end of startup instead of stopping it.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'use-package-ensure)

(defgroup cj/package-resilience nil
  "Keep a failed package install from aborting Emacs startup."
  :group 'cj
  :prefix "cj/package-")

(defcustom cj/package-install-retries 2
  "How many extra attempts a failed package install gets.
Retries exist for transient network failures, which is the common case on a
fresh install pulling every package over the wire."
  :type 'integer
  :group 'cj/package-resilience)

(defcustom cj/package-install-retry-delay 2
  "Seconds to wait between package install attempts."
  :type 'number
  :group 'cj/package-resilience)

(defcustom cj/package-install-retry-budget 60
  "Seconds this session may spend retrying installs, in total.
Retrying is worth it for a transient failure, which fails alone.  A machine
that is simply offline fails every package instead, and without a ceiling the
per-package retry cost would be paid ~190 times over — trading the abort this
module removes for a startup that appears to hang.  Once the budget is spent
each package still gets its one attempt, and still gets recorded."
  :type 'number
  :group 'cj/package-resilience)

(defcustom cj/package-install-failure-limit 5
  "Consecutive failed installs after which this session stops attempting more.
The retry budget bounds retrying, but not the first attempt, and the first
attempt is where the cost lives when a machine is entirely offline: nothing
populates `package-archive-contents', so `use-package-ensure-elpa' runs a full
`package-refresh-contents' across every configured archive before each install
fails.  Paid once per package across ~190 packages, that is a startup that
looks hung.  Failures this many times in a row mean the network is gone rather
than one package being unlucky, so the rest are recorded without being tried."
  :type 'integer
  :group 'cj/package-resilience)

(defvar cj/failed-package-installs nil
  "Archive packages that did not install during this session.")

(defvar cj/failed-source-package-installs nil
  "Packages declared with `:vc' that did not install during this session.
Kept apart from `cj/failed-package-installs' because `package-install' cannot
recover them: some are on no archive at all, and one that happens to be on an
archive would be recovered as the archive build rather than the source
checkout that was asked for, silently and permanently.")

(defvar cj/--package-retry-spent 0.0
  "Seconds spent retrying package installs so far this session.")

(defvar cj/--package-consecutive-failures 0
  "How many packages have failed to install in a row.")

;; ------------------------------ Resolving names ------------------------------

(defun cj/--package-as-symbol (name)
  "Return NAME as a symbol, whether it arrives as a symbol or a string.
This mirrors `use-package-as-symbol' without depending on use-package-core
being loaded at the point early-init installs this."
  (if (symbolp name) name (intern name)))

(defun cj/--package-ensure-packages (name args)
  "Return the package symbols a use-package form requests.
NAME is the form's name and ARGS the values of its :ensure keywords, in the
shape `use-package-ensure-elpa' receives them: t means the form's own name, a
symbol names another package, a cons cell is a pinned (PACKAGE . ARCHIVE), and
nil requests nothing."
  (delq nil
        (mapcar (lambda (ensure)
                  (let ((package (if (eq ensure t)
                                     (cj/--package-as-symbol name)
                                   ensure)))
                    (if (consp package) (car package) package)))
                args)))

(defun cj/--package-ensure-missing (name args)
  "Return the packages NAME's :ensure ARGS request that are not installed."
  (seq-remove #'package-installed-p (cj/--package-ensure-packages name args)))

(defun cj/--package-any-retryable-p (packages)
  "Return non-nil when some of PACKAGES is one an archive actually carries.
A name no archive has heard of will not appear on a retry either, so retrying
it only spends another refresh on a typo."
  (seq-some (lambda (package) (assq package package-archive-contents)) packages))

;; -------------------------------- Installing ---------------------------------

(defun cj/--package-ensure-once (name args state no-refresh)
  "Make one install attempt for NAME's :ensure ARGS, with STATE and NO-REFRESH.
Binding `debug-on-error' to nil re-arms the `condition-case-unless-debug'
inside `use-package-ensure-elpa', which early-init's loud-errors setting
otherwise disables.  The editing hooks are silenced because installing a
package generates autoloads by visiting .el files: a hook belonging to a
package that failed earlier would run there and break unrelated installs."
  (let ((debug-on-error nil)
        (find-file-hook nil)
        (prog-mode-hook nil)
        (lisp-data-mode-hook nil)
        (emacs-lisp-mode-hook nil))
    (use-package-ensure-elpa name args state no-refresh)))

(defun cj/--package-retry-budget-left-p ()
  "Return non-nil while this session may still spend time retrying installs."
  (< cj/--package-retry-spent cj/package-install-retry-budget))

(defun cj/--package-ensure-retry (name args state no-refresh)
  "Retry NAME's missing :ensure ARGS, passing STATE and NO-REFRESH through.
Stops once the session's retry budget is spent, or once nothing still missing
is carried by an archive."
  (let ((left cj/package-install-retries))
    (while (and (> left 0)
                (cj/--package-retry-budget-left-p)
                (cj/--package-any-retryable-p (cj/--package-ensure-missing name args)))
      (setq left (1- left))
      (let ((start (float-time)))
        (sleep-for cj/package-install-retry-delay)
        (cj/--package-ensure-once name args state no-refresh)
        (setq cj/--package-retry-spent
              (+ cj/--package-retry-spent (- (float-time) start)))))))

(defun cj/--package-record-one (package)
  "Record PACKAGE as one that did not install."
  (when package
    (cl-pushnew package cj/failed-package-installs)))

(defun cj/--package-record-source-one (package)
  "Record PACKAGE as a source install that did not complete."
  (when package
    (cl-pushnew package cj/failed-source-package-installs)))

(defun cj/--package-record-failures (name args)
  "Record any of NAME's :ensure ARGS that are still not installed."
  (dolist (package (cj/--package-ensure-missing name args))
    (cj/--package-record-one package)))

(defun cj/--package-giving-up-p ()
  "Return non-nil once enough installs have failed in a row to stop trying."
  (>= cj/--package-consecutive-failures cj/package-install-failure-limit))

(defun cj/--package-note-outcome (name args)
  "Count NAME's :ensure ARGS outcome toward the consecutive-failure run."
  (if (cj/--package-ensure-missing name args)
      (setq cj/--package-consecutive-failures
            (1+ cj/--package-consecutive-failures))
    (setq cj/--package-consecutive-failures 0)))

(defun cj/package-ensure (name args state &optional no-refresh)
  "Install NAME's :ensure ARGS without letting a failure abort startup.
STATE and NO-REFRESH are passed through to `use-package-ensure-elpa'.  This is
the value of `use-package-ensure-function'; see this file's commentary for why
the stock one cannot survive `debug-on-error'.

A form whose packages are already present is left alone entirely, so it neither
costs anything nor tells us whether the network is up."
  (cond
   ((null (cj/--package-ensure-missing name args)) nil)
   ((cj/--package-giving-up-p) (cj/--package-record-failures name args))
   (t
    (cj/--package-ensure-once name args state no-refresh)
    (cj/--package-ensure-retry name args state no-refresh)
    (cj/--package-note-outcome name args)
    (cj/--package-record-failures name args))))

;; ------------------------- Packages installed from source --------------------

;; A `:vc' form routes around everything above: use-package nulls :ensure
;; whenever :vc is present (use-package-ensure.el, `use-package-handler/:ensure'),
;; so `use-package-ensure-function' is never consulted.  And
;; `use-package-vc-install' carries no error handling of its own, so a failed
;; clone signals straight into init under the loud-errors setting -- the
;; original bug, through a second door.  A fresh machine without credentials
;; for the git host yet is exactly the case this module exists for, so the
;; clone gets the same treatment: quiet context, recorded, counted.

(defun cj/--package-vc-install-once (orig arg local-path)
  "Call ORIG with ARG and LOCAL-PATH, surviving a failed clone.
Returns non-nil when the clone worked.  Unlike the :ensure path there is no
upstream `condition-case' to re-arm, so this supplies one."
  (let ((debug-on-error nil)
        (find-file-hook nil)
        (prog-mode-hook nil)
        (lisp-data-mode-hook nil)
        (emacs-lisp-mode-hook nil))
    (condition-case err
        (progn (funcall orig arg local-path) t)
      (error
       (display-warning
        'cj/package-resilience
        (format "Failed to install %s from source: %s"
                (car arg) (error-message-string err))
        :error)
       nil))))

(defun cj/--package-vc-install-guard (orig arg &optional local-path)
  "Around-advice for `use-package-vc-install', called as ORIG.
ARG is (NAME OPTIONS REVISION) and LOCAL-PATH is passed through."
  (let ((package (car arg)))
    (cond
     ;; Already present: ORIG no-ops, and it would tell us nothing about
     ;; whether the host is reachable, so the failure run is left alone.
     ((and package (package-installed-p package))
      (funcall orig arg local-path))
     ((cj/--package-giving-up-p)
      (cj/--package-record-source-one package))
     (t
      (cj/--package-vc-install-once orig arg local-path)
      (if (and package (package-installed-p package))
          (setq cj/--package-consecutive-failures 0)
        (cj/--package-record-source-one package)
        (setq cj/--package-consecutive-failures
              (1+ cj/--package-consecutive-failures)))))))

;; --------------------------------- Recovery ----------------------------------

(defun cj/package-still-missing ()
  "Return the recorded failures that are still not installed.
A package that failed on its own `use-package' form is often installed a
moment later as some other package's dependency, so the recorded list
overstates the damage until it is re-checked against reality."
  ;; `append' does not copy its last argument and `delete-dups' splices
  ;; destructively, so without the copy this read would edit
  ;; `cj/failed-source-package-installs' in place -- and it runs from the
  ;; startup report, where losing a record silently is the worst place for it.
  (seq-remove #'package-installed-p
              (delete-dups
               (append cj/failed-package-installs
                       (copy-sequence cj/failed-source-package-installs)))))

(defun cj/--package-install-quietly (package)
  "Attempt to install PACKAGE.  Return non-nil if it is installed afterward."
  (unless (package-installed-p package)
    (let ((debug-on-error nil)
          (find-file-hook nil)
          (prog-mode-hook nil)
          (lisp-data-mode-hook nil)
          (emacs-lisp-mode-hook nil))
      (condition-case err
          (package-install package)
        (error (message "package-resilience: %s still failing: %s"
                        package (error-message-string err))))))
  (package-installed-p package))

(defun cj/--package-retry-pass ()
  "Try every package in `cj/failed-package-installs' once.
Return how many were installed on this pass."
  (let ((installed 0))
    ;; Only the archive list.  Source packages are kept out of it entirely, so
    ;; no filter is needed here -- and a filter would be actively wrong: on a
    ;; first boot before the network came up nothing has populated
    ;; `package-archive-contents', so screening on it would skip every recorded
    ;; package and make this command a silent no-op in the case it exists for.
    ;; `package-install' populates the archives itself when it needs to.
    (dolist (package (copy-sequence cj/failed-package-installs))
      (when (cj/--package-install-quietly package)
        (setq cj/failed-package-installs
              (delq package cj/failed-package-installs))
        (setq installed (1+ installed))))
    installed))

(defun cj/retry-failed-package-installs ()
  "Install everything that failed earlier, passing over the set until it settles.
A failed package leaves hooks that break other installs, so one package
succeeding can unblock others.  Passes repeat while any pass installs
something, which also terminates: a pass that installs nothing ends it."
  (interactive)
  ;; Asking for a retry asserts the network may be back, so clear the run that
  ;; stopped this session attempting installs in the first place.
  (setq cj/--package-consecutive-failures 0)
  (while (> (cj/--package-retry-pass) 0))
  (when (called-interactively-p 'interactive)
    (let ((missing (cj/package-still-missing)))
      (message (if missing
                   (format "Still missing: %s"
                           (mapconcat #'symbol-name missing " "))
                 "All packages installed.")))))

(defun cj/report-failed-package-installs ()
  "Warn about packages that failed to install, naming every one of them.
Only packages that are still absent are named; one that arrived later as
another package's dependency is not a failure the user needs to act on."
  (let* ((missing (cj/package-still-missing))
         (source (seq-filter (lambda (p)
                               (memq p cj/failed-source-package-installs))
                             missing))
         (archive (seq-difference missing source)))
    (when missing
      (display-warning
       'cj/package-resilience
       (concat
        (format "%d package(s) are missing: %s
Startup continued without them, so features they back are missing."
                (length missing) (mapconcat #'symbol-name missing ", "))
        ;; Two different recoveries, so name which packages each one covers.
        ;; Sending the user to the retry command for a source package wastes
        ;; their time every startup: it cannot install one.
        (when archive
          (format "
Run M-x cj/retry-failed-package-installs for: %s"
                  (mapconcat #'symbol-name archive ", ")))
        (when source
          (format "
These install from source, so they need working credentials for the git host
and then 'make bootstrap': %s"
                  (mapconcat #'symbol-name source ", ")))
        (when (cj/--package-giving-up-p)
          (format "
Installing stopped after %d failures in a row, so most of these were never
attempted.  Check the network and your credentials for the git host."
                  cj/package-install-failure-limit)))
       :error))))

;; -------------------------------- Bootstrap ----------------------------------

(defun cj/package-bootstrap-batch ()
  "Entry point for the bootstrap script: retry, report, and exit.
Loading init.el in batch installs whatever `use-package' asks for; this retries
anything that pass missed and turns the outcome into an exit status the shell
can loop on.  Exits 0 when nothing is missing, 1 otherwise."
  (cj/retry-failed-package-installs)
  (let ((missing (cj/package-still-missing)))
    (if missing
        (progn
          (message "package-bootstrap: %d missing: %s"
                   (length missing)
                   (mapconcat #'symbol-name missing " "))
          (kill-emacs 1))
      (message "package-bootstrap: all packages installed")
      (kill-emacs 0))))

;; --------------------------------- Wiring ------------------------------------

(setq use-package-ensure-function #'cj/package-ensure)

;; Named function, never a lambda: an anonymous advice cannot be removed by
;; reference, so a live daemon would keep running it after the form is deleted.
(advice-add 'use-package-vc-install :around #'cj/--package-vc-install-guard)

(add-hook 'emacs-startup-hook #'cj/report-failed-package-installs 90)

(provide 'package-resilience)
;;; package-resilience.el ends here
