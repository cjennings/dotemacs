;;; test-undead-buffers--native-comp-log-undead.el --- the native-comp log survives the sweep -*- lexical-binding: t; -*-

;;; Commentary:
;; Async native compilation parks every worker process on one buffer,
;; `comp-async-buffer-name' (*Async-native-compile-log*), and the worker's
;; sentinel reads that buffer back before it starts the next job.  Killing
;; the buffer sends SIGHUP to every worker under it (they are :noquery, so
;; nothing asks), each sentinel then dies in `with-current-buffer' on the
;; dead buffer, and `comp--run-async-workers' is never called again: the
;; queue is stranded for the life of the daemon and nothing is ever cached.
;;
;; `cj/dashboard-only' on `emacs-startup-hook' runs
;; `cj/kill-all-other-buffers-and-windows', which is exactly such a sweep,
;; and in a real daemon `dashboard-insert-startupify-lists' has already
;; created *dashboard* on `after-init-hook', so the sweep branch is the one
;; that runs.  These tests pin the log buffer to the undead list so the
;; sweep buries it and the workers live.  The fixture puts a live :noquery
;; process on the buffer, because that is the state the bug needs; a plain
;; buffer would survive a kill-and-recreate just the same.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'undead-buffers)

(defconst test-undead--comp-log "*Async-native-compile-log*")

(defun test-undead--make-sleeper (buffer)
  "Start a quiet, long-lived process attached to BUFFER and return it."
  (make-process :name "test-undead-sleeper" :buffer buffer
                :command '("sleep" "30") :noquery t))

(defun test-undead--settle ()
  "Let any signal the sweep sent land before liveness is observed."
  (let ((deadline (+ (float-time) 0.3)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

;;; Normal Cases

(ert-deftest test-undead-buffers-native-comp-log-is-undead-by-default ()
  "Normal: the module's default list makes the async-compile log bury-only."
  (should (member test-undead--comp-log cj/undead-buffer-list))
  (should (cj/--buffer-undead-p test-undead--comp-log)))

(ert-deftest test-undead-buffers-native-comp-log-name-matches-comp-run ()
  "Normal: the pinned name is the one comp-run actually uses.
A rename upstream would silently reopen the bug, so pin it to the variable."
  (skip-unless (require 'comp-run nil t))
  (should (equal comp-async-buffer-name test-undead--comp-log)))

(ert-deftest test-undead-buffers-native-comp-log-workers-survive-sweep ()
  "Normal: a worker parked on the log buffer is still running after the sweep.
The positive control is an ordinary process buffer, which the sweep kills
out from under its process -- that is what happened to the workers without
the undead entry.  The control's process is not asserted dead: killing the
buffer sends SIGHUP, and a launching shell that ignores SIGHUP (nohup) hands
that disposition down, so its death is not deterministic across harnesses."
  (skip-unless (executable-find "sleep"))
  (delete-other-windows)
  (let* ((main (current-buffer))
         (existing (get-buffer test-undead--comp-log))
         (log (or existing (get-buffer-create test-undead--comp-log)))
         (victim (generate-new-buffer "*test-sweep-victim*"))
         (worker (test-undead--make-sleeper log))
         (control (test-undead--make-sleeper victim)))
    (unwind-protect
        (progn
          (cj/kill-all-other-buffers-and-windows)
          (test-undead--settle)
          (should (buffer-live-p main))
          (should (buffer-live-p log))
          (should (process-live-p worker))
          (should-not (buffer-live-p victim)))
      (when (process-live-p worker) (delete-process worker))
      (when (process-live-p control) (delete-process control))
      (when (buffer-live-p victim) (kill-buffer victim))
      ;; Only remove what this test created.  `kill-buffer' the function is
      ;; not the remapped command, so the undead list doesn't apply.
      (when (and (not existing) (buffer-live-p log)) (kill-buffer log))
      (delete-other-windows))))

;;; Boundary Cases

(ert-deftest test-undead-buffers-native-comp-log-match-is-exact ()
  "Boundary: only the exact name is undead; a uniquified copy is not."
  (should-not (cj/--buffer-undead-p "*Async-native-compile-log*<2>"))
  (should-not (cj/--buffer-undead-p " *Async-native-compile-log*")))

(provide 'test-undead-buffers--native-comp-log-undead)
;;; test-undead-buffers--native-comp-log-undead.el ends here
