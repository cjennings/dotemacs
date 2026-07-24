;;; test-system-defaults-functions.el --- Tests for the helper functions in system-defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; system-defaults.el is mostly `setq' configuration but ships a handful
;; of small interactive / hook helpers:
;;
;;   cj/disabled                  -- no-op stub used by `defalias' for
;;                                   commands we don't want surfaced
;;   cj/minibuffer-setup-hook     -- inflate gc-cons-threshold while
;;                                   typing in the minibuffer
;;   cj/minibuffer-exit-hook      -- restore gc-cons-threshold on exit
;;   cj/--unpropertize-kill-ring       -- strip text properties from
;;                                   kill-ring at shutdown
;;   cj/log-comp-warning          -- route native-comp warnings to a
;;                                   file rather than the *Warnings*
;;                                   buffer
;;
;; The module has startup side effects, so we wrap a single
;; top-level `require' in `cl-letf' stubs for the side-effecting
;; primitives.  Per-test reload via `(load ...)' would technically
;; work but undercover.el's instrumentation only sees the first
;; load, so the function bodies show up as uncovered even though
;; the tests run them.  Loading once at top level fixes that.

;;; Code:

(require 'cl-lib)
(require 'autorevert)
(require 'bookmark)
(require 'ert)
(require 'server)
(require 'vc-hooks)

;; user-constants supplies `user-home-dir' and `org-dir', and
;; host-environment supplies `env-bsd-p', both of which system-defaults
;; reads at load.  Required first so the symbols are defined before the
;; require fires the side effects we don't stub away.  (system-defaults
;; itself declares these only via `eval-when-compile', so the compiled
;; module cannot resolve them standalone — tracked as a Phase 2 fix.)
(add-to-list 'load-path
             (file-name-concat
              (file-name-directory
               (directory-file-name
                (file-name-directory (or load-file-name buffer-file-name))))
              "modules"))
(require 'user-constants)
(require 'host-environment)

;; Load system-defaults ONCE with side-effecting primitives stubbed.
;; This pattern lets undercover see and instrument the function
;; bodies.  Stubs deliberately scope only to the require so the
;; real primitives remain available for unrelated tests in the
;; same Emacs.
;; Contain system-defaults' load-time `(setq default-directory user-home-dir)'
;; so it doesn't leak into a shared batch session.  `make test-name' loads
;; every test file into one Emacs; a leaked cwd there breaks the relative
;; loads of every file that follows.
(let ((default-directory default-directory)
      (use-package-always-ensure nil))
  (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
            ((symbol-function 'server-start) #'ignore)
            ((symbol-function 'set-locale-environment) #'ignore)
            ((symbol-function 'prefer-coding-system) #'ignore)
            ((symbol-function 'set-default-coding-systems) #'ignore)
            ((symbol-function 'set-terminal-coding-system) #'ignore)
            ((symbol-function 'set-keyboard-coding-system) #'ignore)
            ((symbol-function 'set-selection-coding-system) #'ignore)
            ((symbol-function 'set-charset-priority) #'ignore)
            ((symbol-function 'global-auto-revert-mode) #'ignore)
            ((symbol-function 'recentf-mode) #'ignore))
    (unless (fboundp 'use-package)
      (defmacro use-package (&rest _args) nil))
    (require 'system-defaults)))

;;; cj/disabled

(ert-deftest test-system-defaults-disabled-normal-returns-nil ()
  "Normal: `cj/disabled' is a silent interactive no-op."
  (should (eq (cj/disabled) nil))
  (should (commandp #'cj/disabled)))

;;; cj/--unpropertize-kill-ring

(ert-deftest test-system-defaults-unpropertize-kill-ring-strips-properties ()
  "Normal: every kill-ring entry comes back with no text properties."
  (let ((kill-ring (list (propertize "alpha" 'face 'bold)
                         (propertize "beta" 'face 'underline))))
    (cj/--unpropertize-kill-ring)
    (should (equal kill-ring '("alpha" "beta")))
    (should-not (text-properties-at 0 (nth 0 kill-ring)))
    (should-not (text-properties-at 0 (nth 1 kill-ring)))))

(ert-deftest test-system-defaults-unpropertize-kill-ring-boundary-empty-ring ()
  "Boundary: an empty `kill-ring' stays empty after the strip pass."
  (let ((kill-ring nil))
    (cj/--unpropertize-kill-ring)
    (should (null kill-ring))))

;;; cj/log-comp-warning

(ert-deftest test-system-defaults-log-comp-warning-writes-log-line ()
  "Normal: a TYPE containing `comp' writes a timestamped line to the log."
  (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
    (unwind-protect
        (progn
          (cj/log-comp-warning 'comp "hello %s" "world")
          (with-temp-buffer
            (insert-file-contents comp-warnings-log)
            (let ((contents (buffer-string)))
              (should (string-match-p "hello world" contents))
              (should (string-match-p "^\\[" contents)))))
      (delete-file comp-warnings-log))))

(ert-deftest test-system-defaults-log-comp-warning-list-type-includes-comp ()
  "Boundary: a list TYPE matches when `comp' is one of its elements."
  (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
    (unwind-protect
        (progn
          (cj/log-comp-warning '(comp warning) "alpha")
          (with-temp-buffer
            (insert-file-contents comp-warnings-log)
            (should (string-match-p "alpha" (buffer-string)))))
      (delete-file comp-warnings-log))))

(ert-deftest test-system-defaults-log-comp-warning-non-comp-type-is-noop ()
  "Boundary: a TYPE that doesn't include `comp' returns nil and writes nothing.

`display-warning' interprets nil as \"I didn't handle it\" -- so the
default *Warnings* buffer path keeps working for unrelated warnings."
  (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
    (unwind-protect
        (progn
          (should (null (cj/log-comp-warning 'unrelated "ignored")))
          (with-temp-buffer
            (insert-file-contents comp-warnings-log)
            (should (string-empty-p (buffer-string)))))
      (delete-file comp-warnings-log))))

(ert-deftest test-system-defaults-log-comp-warning-list-type-without-comp ()
  "Boundary: a list TYPE that doesn't contain `comp' returns nil and
exercises the `(when (memq ...))' guard with a non-matching list."
  (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
    (unwind-protect
        (progn
          (should (null (cj/log-comp-warning '(unrelated warning) "ignored")))
          (with-temp-buffer
            (insert-file-contents comp-warnings-log)
            (should (string-empty-p (buffer-string)))))
      (delete-file comp-warnings-log))))

(ert-deftest test-system-defaults-log-comp-warning-non-string-message ()
  "Boundary: a non-string MESSAGE falls into the `format \"%S %S\"' branch
and the rendered S-expression lands in the log."
  (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
    (unwind-protect
        (progn
          (cj/log-comp-warning 'comp '(some-symbol :slot 42))
          (with-temp-buffer
            (insert-file-contents comp-warnings-log)
            (let ((contents (buffer-string)))
              (should (string-match-p "some-symbol" contents))
              (should (string-match-p ":slot" contents)))))
      (delete-file comp-warnings-log))))

(ert-deftest test-system-defaults-log-comp-warning-unwritable-log-does-not-signal ()
  "Error: an unwritable log path must not signal.
The function is `:before-until' advice on `display-warning'; a signal here
propagates out of `display-warning' and breaks warning display for every
async native-comp notice.  It swallows the write failure and still returns
t (the warning stays suppressed), rather than crashing."
  (let ((comp-warnings-log "/proc/nonexistent-dir/cannot-write.log"))
    (should (eq t (cj/log-comp-warning 'comp "boom")))))

(ert-deftest test-system-defaults-log-comp-warning-caps-log-growth ()
  "Boundary: the log is bounded — once it exceeds the cap, a further write
resets it (deletes the old file, keeping only the new entry) rather than
growing without limit.  A hard reset, not a tail-trim: on overflow the old
history is discarded, which is fine for a transient diagnostic log."
  (let ((comp-warnings-log (make-temp-file "comp-warnings-" nil ".log")))
    (unwind-protect
        (progn
          ;; Seed the file well over the cap.
          (with-temp-file comp-warnings-log
            (insert (make-string (1+ cj/comp-warnings-log-max-bytes) ?x)))
          (should (> (file-attribute-size (file-attributes comp-warnings-log))
                     cj/comp-warnings-log-max-bytes))
          (cj/log-comp-warning 'comp "after the cap")
          (should (<= (file-attribute-size (file-attributes comp-warnings-log))
                      cj/comp-warnings-log-max-bytes))
          ;; The newest entry survives the trim.
          (with-temp-buffer
            (insert-file-contents comp-warnings-log)
            (should (string-match-p "after the cap" (buffer-string)))))
      (delete-file comp-warnings-log))))

(provide 'test-system-defaults-functions)
;;; test-system-defaults-functions.el ends here
