;;; test-package-resilience.el --- Tests for surviving failed package installs -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for package-resilience.el, which keeps a failed package download from
;; aborting init.  The regression these guard is concrete: early-init.el sets
;; `debug-on-error' during startup so config errors are loud, and that disarms
;; the `condition-case-unless-debug' inside `use-package-ensure-elpa', so one
;; transient download error dropped a fresh install into the debugger with two
;; thirds of the config unloaded.
;;
;; The fakes below stand in for the package archive so no test touches the
;; network or the real elpa directory.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package-resilience)

;;; ------------------------------- Fake registry -------------------------------

(defvar test-pkg-res--installed nil
  "Package symbols the fake registry considers installed.")

(defvar test-pkg-res--install-log nil
  "Packages `package-install' was called with, newest first.")

(defvar test-pkg-res--failures nil
  "Alist of (PACKAGE . N): the next N install attempts for PACKAGE signal.")

(defvar test-pkg-res--dynamic-state nil
  "Captured dynamic state at each `package-install' call, newest first.")

(defun test-pkg-res--should-fail-p (package)
  "Return non-nil when this attempt at PACKAGE should signal, and count it."
  (let ((cell (assq package test-pkg-res--failures)))
    (when (and cell (> (cdr cell) 0))
      (setcdr cell (1- (cdr cell)))
      t)))

(defun test-pkg-res--install (package)
  "Fake `package-install' for PACKAGE: record the call, then fail or install."
  (push package test-pkg-res--install-log)
  (push (list :debug-on-error debug-on-error
              :find-file-hook find-file-hook
              :prog-mode-hook prog-mode-hook
              :lisp-data-mode-hook lisp-data-mode-hook
              :emacs-lisp-mode-hook emacs-lisp-mode-hook)
        test-pkg-res--dynamic-state)
  (if (test-pkg-res--should-fail-p package)
      (signal 'file-error (list "https://elpa.example.invalid/x.tar" "No Data"))
    (push package test-pkg-res--installed)))

(defmacro test-pkg-res--with-registry (available installed failures &rest body)
  "Run BODY against a fake package registry.
AVAILABLE lists package symbols the archives carry, INSTALLED those already
installed, and FAILURES is an alist of (PACKAGE . N) attempts that signal."
  (declare (indent 3) (debug t))
  `(let ((test-pkg-res--installed (copy-sequence ,installed))
         (test-pkg-res--install-log nil)
         (test-pkg-res--failures (copy-tree ,failures))
         (test-pkg-res--dynamic-state nil)
         (cj/failed-package-installs nil)
         (cj/failed-source-package-installs nil)
         (cj/package-install-retry-delay 0)
         ;; Both of these accumulate across a whole session by design, so a
         ;; test that leaves them set changes what a later test does: an
         ;; unbound failure counter tripped the circuit breaker and three
         ;; install tests stopped installing anything at all.
         (cj/--package-retry-spent 0.0)
         (cj/--package-consecutive-failures 0)
         (package-archive-contents (mapcar #'list ,available)))
     (cl-letf (((symbol-function 'package-installed-p)
                (lambda (pkg &rest _) (and (memq pkg test-pkg-res--installed) t)))
               ((symbol-function 'package-install)
                (lambda (pkg &rest _) (test-pkg-res--install pkg)))
               ((symbol-function 'package-refresh-contents) (lambda (&rest _) nil))
               ((symbol-function 'package-read-all-archive-contents) (lambda (&rest _) nil))
               ((symbol-function 'sleep-for) (lambda (&rest _) nil)))
       ,@body)))

;;; --------------------------- Resolving :ensure args --------------------------

(ert-deftest test-package-resilience-packages-resolves-t-to-name ()
  "Normal: an :ensure of t resolves to the use-package form's own name."
  (should (equal '(foo) (cj/--package-ensure-packages 'foo '(t)))))

(ert-deftest test-package-resilience-packages-resolves-explicit-symbol ()
  "Normal: an explicit :ensure symbol names a different package."
  (should (equal '(bar) (cj/--package-ensure-packages 'foo '(bar)))))

(ert-deftest test-package-resilience-packages-nil-ensure-is-empty ()
  "Boundary: :ensure nil requests no package at all."
  (should (equal '() (cj/--package-ensure-packages 'foo '(nil)))))

(ert-deftest test-package-resilience-packages-unwraps-pinned-cons ()
  "Boundary: a pinned (PACKAGE . ARCHIVE) cell resolves to the package symbol."
  (should (equal '(bar) (cj/--package-ensure-packages 'foo '((bar . "melpa"))))))

(ert-deftest test-package-resilience-packages-accepts-string-name ()
  "Boundary: a use-package form named with a string still resolves to a symbol."
  (should (equal '(foo) (cj/--package-ensure-packages "foo" '(t)))))

(ert-deftest test-package-resilience-packages-handles-several-ensures ()
  "Boundary: several :ensure keywords resolve to several packages."
  (should (equal '(bar baz) (cj/--package-ensure-packages 'foo '(bar baz)))))

;;; ------------------------------ Installing ----------------------------------

(ert-deftest test-package-resilience-installs-missing-package ()
  "Normal: a missing package is installed and nothing is recorded as failed."
  (test-pkg-res--with-registry '(foo) '() '()
    (cj/package-ensure 'foo '(t) nil)
    (should (equal '(foo) test-pkg-res--install-log))
    (should (memq 'foo test-pkg-res--installed))
    (should-not cj/failed-package-installs)))

(ert-deftest test-package-resilience-skips-installed-package ()
  "Normal: an already-installed package is never downloaded again."
  (test-pkg-res--with-registry '(foo) '(foo) '()
    (cj/package-ensure 'foo '(t) nil)
    (should-not test-pkg-res--install-log)
    (should-not cj/failed-package-installs)))

(ert-deftest test-package-resilience-survives-failure-under-debug-on-error ()
  "Error: a failed install is recorded, not signalled, even with debug-on-error.
This is the regression: `condition-case-unless-debug' inside use-package does
not catch while `debug-on-error' is non-nil, so a transient download error
aborted init in place."
  (test-pkg-res--with-registry '(foo) '() '((foo . 999))
    (let ((debug-on-error t))
      (cj/package-ensure 'foo '(t) nil)
      (should (memq 'foo cj/failed-package-installs))
      (should-not (memq 'foo test-pkg-res--installed)))))

(ert-deftest test-package-resilience-retries-transient-failure ()
  "Error: a download that fails once and then succeeds installs on the retry."
  (test-pkg-res--with-registry '(foo) '() '((foo . 1))
    (cj/package-ensure 'foo '(t) nil)
    (should (= 2 (length test-pkg-res--install-log)))
    (should (memq 'foo test-pkg-res--installed))
    (should-not cj/failed-package-installs)))

(ert-deftest test-package-resilience-stops-after-configured-retries ()
  "Boundary: a package that always fails is attempted retries-plus-one times."
  (test-pkg-res--with-registry '(foo) '() '((foo . 999))
    (let ((cj/package-install-retries 2))
      (cj/package-ensure 'foo '(t) nil)
      (should (= 3 (length test-pkg-res--install-log)))
      (should (memq 'foo cj/failed-package-installs)))))

(ert-deftest test-package-resilience-does-not-retry-unknown-package ()
  "Boundary: a package no archive carries is attempted once, then recorded.
Retrying a name the archives have never heard of only burns refreshes."
  (test-pkg-res--with-registry '() '() '((foo . 999))
    (let ((cj/package-install-retries 2))
      (cj/package-ensure 'foo '(t) nil)
      (should (= 1 (length test-pkg-res--install-log)))
      (should (memq 'foo cj/failed-package-installs)))))

(ert-deftest test-package-resilience-inhibits-editing-hooks-during-install ()
  "Error: editing hooks are silenced while a package installs.
Installation generates autoloads by visiting .el files, so a hook belonging to
a package that failed earlier would otherwise break unrelated installs."
  (test-pkg-res--with-registry '(foo) '() '()
    (let ((find-file-hook '(ignore))
          (prog-mode-hook '(ignore))
          (lisp-data-mode-hook '(ignore))
          (emacs-lisp-mode-hook '(ignore)))
      (cj/package-ensure 'foo '(t) nil)
      (let ((seen (car test-pkg-res--dynamic-state)))
        (should-not (plist-get seen :find-file-hook))
        (should-not (plist-get seen :prog-mode-hook))
        (should-not (plist-get seen :lisp-data-mode-hook))
        (should-not (plist-get seen :emacs-lisp-mode-hook))
        (should-not (plist-get seen :debug-on-error))))))

(ert-deftest test-package-resilience-records-each-failure-once ()
  "Boundary: repeated ensure calls for one package record it a single time."
  (test-pkg-res--with-registry '(foo) '() '((foo . 999))
    (cj/package-ensure 'foo '(t) nil)
    (cj/package-ensure 'foo '(t) nil)
    (should (equal '(foo) cj/failed-package-installs))))

;;; ------------------------------ Retry budget ---------------------------------

(ert-deftest test-package-resilience-budget-caps-retrying ()
  "Boundary: with the retry budget spent, a failure gets its one attempt only.
An offline machine fails every package, so an uncapped per-package retry would
turn the abort this module removes into a startup that appears to hang."
  (test-pkg-res--with-registry '(foo) '() '((foo . 999))
    (let ((cj/package-install-retries 2)
          (cj/--package-retry-spent 999.0))
      (cj/package-ensure 'foo '(t) nil)
      (should (= 1 (length test-pkg-res--install-log)))
      (should (memq 'foo cj/failed-package-installs)))))

(ert-deftest test-package-resilience-budget-still-records-failures ()
  "Boundary: a package skipped for budget is still recorded and reportable."
  (test-pkg-res--with-registry '(foo) '() '((foo . 999))
    (let ((cj/--package-retry-spent 999.0))
      (cj/package-ensure 'foo '(t) nil)
      (should (equal '(foo) (cj/package-still-missing))))))

(ert-deftest test-package-resilience-budget-accrues-across-packages ()
  "Error: retry time spent on one package counts against the next one's budget.
The budget is per session, not per package, which is what bounds a machine
offline for all ~190 of them.  The clock is advanced ten seconds per reading so
the accrual is real rather than an artifact of a mocked sleep."
  (test-pkg-res--with-registry '(foo bar) '() '((foo . 999) (bar . 999))
    (let ((cj/package-install-retries 2)
          (cj/package-install-retry-budget 15.0)
          (cj/--package-retry-spent 0.0)
          (clock 0.0))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&rest _) (setq clock (+ clock 10.0)))))
        (cj/package-ensure 'foo '(t) nil)
        (cj/package-ensure 'bar '(t) nil))
      ;; foo: one attempt plus two retries, spending 20s.  bar: one attempt,
      ;; because foo already overspent the shared budget.
      (should (= 4 (length test-pkg-res--install-log)))
      (should (> cj/--package-retry-spent cj/package-install-retry-budget)))))

;;; ----------------------------- Circuit breaker -------------------------------

(ert-deftest test-package-resilience-stops-attempting-after-failure-run ()
  "Error: enough failures in a row and later packages are recorded untried.
Offline, nothing populates the archive list, so every single attempt pays a
full `package-refresh-contents' before failing.  Across ~190 packages that is
the dominant cost, and no retry ceiling bounds it."
  (test-pkg-res--with-registry '(a b c) '() '((a . 999) (b . 999) (c . 999))
    (let ((cj/package-install-retries 0)
          (cj/package-install-failure-limit 2))
      (cj/package-ensure 'a '(t) nil)
      (cj/package-ensure 'b '(t) nil)
      (cj/package-ensure 'c '(t) nil)
      ;; a and b were tried; c was not, because the run had already reached 2.
      (should (equal '(b a) test-pkg-res--install-log))
      (should (memq 'c cj/failed-package-installs)))))

(ert-deftest test-package-resilience-failure-run-resets-on-success ()
  "Boundary: one success clears the run, so an unlucky package is not fatal.
The breaker exists to detect a dead network, not to give up after N scattered
failures across an otherwise healthy install."
  (test-pkg-res--with-registry '(a b c) '() '((a . 999) (c . 999))
    (let ((cj/package-install-retries 0)
          (cj/package-install-failure-limit 2))
      (cj/package-ensure 'a '(t) nil)   ; fails, run = 1
      (cj/package-ensure 'b '(t) nil)   ; succeeds, run = 0
      (cj/package-ensure 'c '(t) nil)   ; fails, run = 1, still under the limit
      (should (equal '(c b a) test-pkg-res--install-log)))))

(ert-deftest test-package-resilience-installed-package-does-not-clear-run ()
  "Boundary: a package that was already present tells us nothing about the net.
Counting it as a success would reset the run on every built-in-backed form and
the breaker would never trip on an offline machine."
  (test-pkg-res--with-registry '(a b c) '(b) '((a . 999) (c . 999))
    (let ((cj/package-install-retries 0)
          (cj/package-install-failure-limit 2))
      (cj/package-ensure 'a '(t) nil)   ; fails, run = 1
      (cj/package-ensure 'b '(t) nil)   ; already installed, untouched
      (cj/package-ensure 'c '(t) nil)   ; fails, run = 2
      (should (equal '(c a) test-pkg-res--install-log))
      (should (cj/--package-giving-up-p)))))

;;; --------------------- Packages installed from source (:vc) ------------------

(defun test-pkg-res--vc-orig (fails)
  "Return a fake `use-package-vc-install' that signals when FAILS is non-nil."
  (lambda (arg &optional _local-path)
    (push (car arg) test-pkg-res--install-log)
    (if fails
        (signal 'error (list "Cloning failed: Permission denied (publickey)"))
      (push (car arg) test-pkg-res--installed))))

(ert-deftest test-package-resilience-vc-install-succeeds-quietly ()
  "Normal: a working source install is not recorded as a failure."
  (test-pkg-res--with-registry '() '() '()
    (cj/--package-vc-install-guard (test-pkg-res--vc-orig nil) '(gloss nil nil))
    (should (memq 'gloss test-pkg-res--installed))
    (should-not cj/failed-package-installs)))

(ert-deftest test-package-resilience-vc-install-survives-failed-clone ()
  "Error: a failed clone is recorded, not signalled, even with debug-on-error.
`:vc' forms route around `use-package-ensure-function' entirely and
`use-package-vc-install' has no error handling, so without this guard a fresh
machine lacking credentials for the git host aborts init exactly as before."
  (test-pkg-res--with-registry '() '() '()
    (let ((debug-on-error t))
      (cj/--package-vc-install-guard (test-pkg-res--vc-orig t) '(gloss nil nil))
      (should (memq 'gloss cj/failed-source-package-installs))
      (should (memq 'gloss (cj/package-still-missing)))
      ;; Never the archive list: `package-install' cannot recover a source
      ;; package, and for one that also exists on an archive it would install
      ;; the archive build instead of the checkout that was asked for.
      (should-not (memq 'gloss cj/failed-package-installs))
      (should-not (memq 'gloss test-pkg-res--installed)))))

(ert-deftest test-package-resilience-vc-failure-counts-toward-breaker ()
  "Error: a failed clone counts toward the consecutive-failure run.
No credentials means every source package fails, the same shape as no network."
  (test-pkg-res--with-registry '() '() '()
    (let ((cj/package-install-failure-limit 2))
      (cj/--package-vc-install-guard (test-pkg-res--vc-orig t) '(gloss nil nil))
      (cj/--package-vc-install-guard (test-pkg-res--vc-orig t) '(chime nil nil))
      (should (cj/--package-giving-up-p)))))

(ert-deftest test-package-resilience-vc-skipped-once-breaker-tripped ()
  "Boundary: with the breaker tripped a source install is recorded untried."
  (test-pkg-res--with-registry '() '() '()
    (let ((cj/--package-consecutive-failures 99))
      (cj/--package-vc-install-guard (test-pkg-res--vc-orig t) '(gloss nil nil))
      (should-not test-pkg-res--install-log)
      (should (memq 'gloss cj/failed-source-package-installs)))))

(ert-deftest test-package-resilience-vc-installed-package-passes-through ()
  "Boundary: an already-installed source package neither counts nor records.
It says nothing about whether the git host is reachable, so treating it as a
success would reset the run and stop the breaker ever tripping."
  (test-pkg-res--with-registry '() '(gloss) '()
    (let ((cj/--package-consecutive-failures 3))
      (cj/--package-vc-install-guard (test-pkg-res--vc-orig nil) '(gloss nil nil))
      (should (= 3 cj/--package-consecutive-failures))
      (should-not cj/failed-package-installs))))

(ert-deftest test-package-resilience-vc-success-clears-failure-run ()
  "Boundary: a clone that works clears the run, like any other install."
  (test-pkg-res--with-registry '() '() '()
    (let ((cj/--package-consecutive-failures 3))
      (cj/--package-vc-install-guard (test-pkg-res--vc-orig nil) '(gloss nil nil))
      (should (= 0 cj/--package-consecutive-failures)))))

;;; --------------------------------- Retrying ----------------------------------

(ert-deftest test-package-resilience-retry-clears-recovered-package ()
  "Normal: retrying installs a package that is now reachable and clears it."
  (test-pkg-res--with-registry '(foo) '() '()
    (setq cj/failed-package-installs '(foo))
    (cj/retry-failed-package-installs)
    (should (memq 'foo test-pkg-res--installed))
    (should-not cj/failed-package-installs)))

(ert-deftest test-package-resilience-retry-converges-on-cascade ()
  "Boundary: a package installable only on a later pass still converges.
A failed package leaves hooks that break other installs, so recovery has to
keep passing over the set until a pass installs nothing new."
  (test-pkg-res--with-registry '(foo bar) '() '((bar . 1))
    (setq cj/failed-package-installs '(foo bar))
    (cj/retry-failed-package-installs)
    (should (memq 'foo test-pkg-res--installed))
    (should (memq 'bar test-pkg-res--installed))
    (should-not cj/failed-package-installs)))

(ert-deftest test-package-resilience-retry-leaves-source-packages-alone ()
  "Boundary: retrying never runs `package-install' on a source package.
It cannot recover one, and for a source package that also exists on an archive
it would install the archive build instead of the checkout that was declared,
leaving `package-installed-p' true and the source install permanently skipped."
  (test-pkg-res--with-registry '(gloss) '() '()
    (setq cj/failed-source-package-installs '(gloss))
    (cj/retry-failed-package-installs)
    (should-not test-pkg-res--install-log)
    (should (equal '(gloss) (cj/package-still-missing)))))

(ert-deftest test-package-resilience-retry-works-with-empty-archive-list ()
  "Error: recovery still attempts installs when no archive list is loaded yet.
This is the case the command exists for: a laptop that booted before its wifi
came up has an empty `package-archive-contents', and screening recorded
packages against it would make the command a silent no-op right when the user
finally has a network.  `package-install' populates the archives itself."
  (test-pkg-res--with-registry '() '() '()
    (setq cj/failed-package-installs '(foo bar))
    (should-not package-archive-contents)
    (cj/retry-failed-package-installs)
    (should (equal '(bar foo) test-pkg-res--install-log))
    (should-not (cj/package-still-missing))))

(ert-deftest test-package-resilience-retry-terminates-when-impossible ()
  "Error: a package that can never install terminates the loop and stays listed."
  (test-pkg-res--with-registry '(foo) '() '((foo . 999))
    (setq cj/failed-package-installs '(foo))
    (cj/retry-failed-package-installs)
    (should (equal '(foo) cj/failed-package-installs))))

(ert-deftest test-package-resilience-retry-with-nothing-failed-is-quiet ()
  "Boundary: retrying an empty failure set installs nothing."
  (test-pkg-res--with-registry '(foo) '() '()
    (setq cj/failed-package-installs nil)
    (cj/retry-failed-package-installs)
    (should-not test-pkg-res--install-log)))

;;; -------------------------------- Reporting ----------------------------------

(ert-deftest test-package-resilience-report-is-silent-when-clean ()
  "Normal: a run with no failed installs raises no warning."
  (test-pkg-res--with-registry '() '() '()
    (let ((warned nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (setq warned t))))
        (cj/report-failed-package-installs)
        (should-not warned)))))

(ert-deftest test-package-resilience-still-missing-does-not-mutate-records ()
  "Error: reading the missing set leaves both record lists intact.
`append' does not copy its last argument and `delete-dups' splices, so the
obvious spelling edits `cj/failed-source-package-installs' in place.  Reading a
value must not destroy it, least of all from the startup report."
  (test-pkg-res--with-registry '() '() '()
    (setq cj/failed-package-installs '(foo shared))
    (setq cj/failed-source-package-installs '(gloss shared chime))
    (cj/package-still-missing)
    (should (equal '(foo shared) cj/failed-package-installs))
    (should (equal '(gloss shared chime) cj/failed-source-package-installs))))

(ert-deftest test-package-resilience-report-omits-package-installed-since ()
  "Boundary: a package that arrived later as a dependency is not reported.
It failed on its own use-package form, so it is on the recorded list, but it is
present now and there is nothing for the user to do about it."
  (test-pkg-res--with-registry '(foo bar) '(foo) '()
    (setq cj/failed-package-installs '(foo bar))
    (should (equal '(bar) (cj/package-still-missing)))
    (let ((message-text nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type msg &rest _) (setq message-text msg))))
        (cj/report-failed-package-installs)
        (should (string-match-p "bar" message-text))
        (should-not (string-match-p "foo" message-text))))))

(ert-deftest test-package-resilience-report-silent-when-all-arrived-since ()
  "Boundary: recorded failures that are all installed now raise no warning."
  (test-pkg-res--with-registry '(foo) '(foo) '()
    (setq cj/failed-package-installs '(foo))
    (let ((warned nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (setq warned t))))
        (cj/report-failed-package-installs)
        (should-not warned)))))

(ert-deftest test-package-resilience-report-says-when-it-stopped-early ()
  "Error: a tripped breaker is said out loud, so untried is not read as failed.
Most of a long list would never have been attempted, and reporting those as
install failures would send the user hunting for ~185 individual problems."
  (test-pkg-res--with-registry '(foo) '() '()
    (setq cj/failed-package-installs '(foo))
    (let ((cj/--package-consecutive-failures 99)
          (message-text nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type msg &rest _) (setq message-text msg))))
        (cj/report-failed-package-installs)
        (should (string-match-p "never" message-text))
        ;; Names both causes: a run of failures is a dead network or missing
        ;; credentials, and the message should not pick one.
        (should (string-match-p "network" message-text))
        (should (string-match-p "credentials" message-text))))))

(ert-deftest test-package-resilience-report-omits-early-stop-when-not-tripped ()
  "Boundary: an ordinary failure is not dressed up as a machine being offline."
  (test-pkg-res--with-registry '(foo) '() '()
    (setq cj/failed-package-installs '(foo))
    (let ((cj/--package-consecutive-failures 0)
          (message-text nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type msg &rest _) (setq message-text msg))))
        (cj/report-failed-package-installs)
        (should-not (string-match-p "never" message-text))))))

(ert-deftest test-package-resilience-report-warns-and-names-failures ()
  "Error: failed installs raise one warning that names every package."
  (test-pkg-res--with-registry '(foo bar) '() '()
    (setq cj/failed-package-installs '(foo bar))
    (let ((message-text nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type msg &rest _) (setq message-text msg))))
        (cj/report-failed-package-installs)
        (should message-text)
        (should (string-match-p "foo" message-text))
        (should (string-match-p "bar" message-text))))))

;;; ---------------------------------- Wiring -----------------------------------

;; The guard tests above call the functions directly, which says nothing about
;; whether they are actually reachable from a real startup.  If use-package
;; renamed either seam, every test above would still pass while the whole
;; module sat dead -- this repo's recurring failure, a gate that was green
;; because it never ran.

(ert-deftest test-package-resilience-is-wired-to-use-package ()
  "Normal: loading the module actually takes over both use-package seams.
`advice-member-p' answers yes for advice attached to a symbol that was never
defined, so it alone would still pass if upstream renamed the function and left
the advice on a dead symbol.  That rename is the whole scenario this test
exists for, hence the `fboundp'."
  (should (eq use-package-ensure-function #'cj/package-ensure))
  (should (fboundp 'use-package-vc-install))
  (should (advice-member-p #'cj/--package-vc-install-guard
                           'use-package-vc-install)))

(ert-deftest test-package-resilience-reports-at-startup ()
  "Normal: the end-of-startup report is on `emacs-startup-hook'."
  (should (memq #'cj/report-failed-package-installs
                (default-value 'emacs-startup-hook))))

(provide 'test-package-resilience)
;;; test-package-resilience.el ends here
