;;; gcmh-config.el --- Garbage collection strategy via gcmh -*- lexical-binding: t -*-

;;; Commentary:
;; gcmh (the Garbage Collector Magic Hack) owns `gc-cons-threshold' for the
;; session.  It keeps the threshold very high while you are active so GC never
;; pauses mid-edit, then drops it and collects on idle, when a pause is
;; invisible.  This replaces the old hand-rolled scheme -- a stock-800KB restore
;; in early-init.el plus a minibuffer setup/exit bump -- which pinned GC at
;; 800000 (Emacs's bare-editor default), far too low for a config this size and
;; the cause of frequent GC pauses during completion, agenda builds, and LSP/AI
;; activity.
;;
;; Kept in its own module, not system-defaults.el: that module is pre-loaded by
;; the comp-errors test harness, which has no package system, so an `:ensure'
;; package there errors at load time.  early-init.el bumps the threshold to
;; `most-positive-fixnum' for startup and deliberately does not restore it;
;; `gcmh-mode' takes ownership from here on.

;;; Code:

(use-package gcmh
  :ensure t
  :demand t
  :config
  (setq gcmh-idle-delay 'auto                          ; scale the idle GC delay to GC cost
        gcmh-high-cons-threshold (* 1 1024 1024 1024)) ; 1 GB during activity
  (gcmh-mode 1))

(provide 'gcmh-config)
;;; gcmh-config.el ends here
