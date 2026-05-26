;;; test-auto-dim-config.el --- Tests for the auto-dim-other-buffers config -*- lexical-binding: t; -*-

;;; Commentary:
;; auto-dim-config configures the local auto-dim-other-buffers fork: dim only
;; non-selected windows within Emacs (not the whole frame on focus-out), drop
;; fringe from the dimmed faces to avoid flicker on this non-pgtk build, and
;; enable the global mode.  Guarded with `skip-unless' because the fork lives
;; in ~/code and may be absent on a clean checkout.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defconst test-auto-dim--fork
  (expand-file-name "~/code/auto-dim-other-buffers.el")
  "Local fork directory the module loads via `:load-path'.")

(ert-deftest test-auto-dim-config-applies-settings ()
  "Normal: loading the module enables the mode with the chosen settings."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (unwind-protect
      (progn
        (should (bound-and-true-p auto-dim-other-buffers-mode))
        (should (null auto-dim-other-buffers-dim-on-focus-out))
        (should (eq t auto-dim-other-buffers-dim-on-switch-to-minibuffer))
        (should-not (assq 'fringe auto-dim-other-buffers-affected-faces)))
    (when (fboundp 'auto-dim-other-buffers-mode)
      (auto-dim-other-buffers-mode -1))))

(ert-deftest test-auto-dim-config-vterm-dimmed-p-all-windows-dimmed ()
  "Normal: a vterm buffer is dimmed when all displayed windows are dimmed."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((major-mode 'vterm-mode))
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(left right)))
              ((symbol-function 'window-parameter)
               (lambda (window parameter)
                 (and (eq parameter 'adob--dim)
                      (memq window '(left right))))))
      (should (cj/auto-dim--vterm-buffer-dimmed-p)))))

(ert-deftest test-auto-dim-config-vterm-dimmed-p-undimmed-window-keeps-buffer-bright ()
  "Normal: a selected/undimmed vterm window keeps the buffer bright."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((major-mode 'vterm-mode))
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(left right)))
              ((symbol-function 'window-parameter)
               (lambda (window parameter)
                 (and (eq parameter 'adob--dim)
                      (eq window 'right)))))
      (should-not (cj/auto-dim--vterm-buffer-dimmed-p)))))

(ert-deftest test-auto-dim-config-vterm-get-color-dims-only-dimmed-vterm-buffers ()
  "Normal: vterm color advice dims only buffers marked dimmed."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((major-mode 'vterm-mode)
        (cj/auto-dim-vterm-foreground-blend 1.0))
    (cl-letf (((symbol-function 'cj/auto-dim--vterm-buffer-dimmed-p)
               (lambda () t))
              ((symbol-function 'cj/auto-dim--face-color)
               (lambda (&rest _) "#555555")))
      (should (equal "#555555"
                     (cj/auto-dim--vterm-get-color
                      (lambda (&rest _) "#ffffff") 7 :foreground))))
    (cl-letf (((symbol-function 'cj/auto-dim--vterm-buffer-dimmed-p)
               (lambda () nil)))
      (should (equal "#ffffff"
                     (cj/auto-dim--vterm-get-color
                      (lambda (&rest _) "#ffffff") 7 :foreground))))))

(ert-deftest test-auto-dim-config-vterm-post-command-schedules-refresh-on-window-change ()
  "Normal: post-command vterm refresh schedules only after selection changes."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((cj/auto-dim--last-selected-window 'old)
        (calls 0))
    (cl-letf (((symbol-function 'selected-window)
               (lambda () 'new))
              ((symbol-function 'cj/auto-dim--schedule-vterm-refresh)
               (lambda (&optional _) (setq calls (1+ calls)))))
      (cj/auto-dim--refresh-vterm-on-command)
      (cj/auto-dim--refresh-vterm-on-command))
    (should (eq cj/auto-dim--last-selected-window 'new))
    (should (= calls 1))))

(ert-deftest test-auto-dim-config-vterm-refresh-runs-auto-dim-before-invalidate ()
  "Normal: deferred vterm refresh updates auto-dim before invalidating vterm."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let (events)
    (cl-letf (((symbol-function 'adob--update)
               (lambda () (push 'adob events)))
              ((symbol-function 'cj/auto-dim--refresh-vterm-windows)
               (lambda (&optional _) (push 'vterm events))))
      (cj/auto-dim--refresh-vterm-after-auto-dim))
    (should (equal events '(vterm adob)))))

(ert-deftest test-auto-dim-config-vterm-refresh-nudges-size-for-full-redraw ()
  "Normal: vterm refresh nudges size to force full-grid redraw."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((calls nil)
        (vterm-min-window-width 80))
    (with-temp-buffer
      (setq major-mode 'vterm-mode)
      (setq-local vterm--term 'term)
      (let ((buffer (current-buffer)))
        (cl-letf (((symbol-function 'window-list)
                   (lambda (&rest _) '(vterm-window)))
                  ((symbol-function 'window-buffer)
                   (lambda (_) buffer))
                  ((symbol-function 'window-live-p)
                   (lambda (_) t))
                  ((symbol-function 'window-body-height)
                   (lambda (_) 24))
                  ((symbol-function 'window-body-width)
                   (lambda (_) 100))
                  ((symbol-function 'vterm--get-margin-width)
                   (lambda () 3))
                  ((symbol-function 'vterm--set-size)
                   (lambda (term height width)
                     (push (list term height width) calls))))
          (cj/auto-dim--refresh-vterm-windows))))
    (should (equal (nreverse calls)
                   '((term 25 97)
                     (term 24 97))))))

(ert-deftest test-auto-dim-config-never-dim-dashboard-exempts-dashboard ()
  "Normal: the *dashboard* buffer is exempt from dimming."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let* ((existed (get-buffer "*dashboard*"))
         (buffer (or existed (get-buffer-create "*dashboard*"))))
    (unwind-protect
        (should (cj/auto-dim--never-dim-dashboard-p buffer))
      (unless existed (kill-buffer buffer)))))

(ert-deftest test-auto-dim-config-never-dim-dashboard-near-miss-name-dims ()
  "Boundary: a buffer whose name only resembles the dashboard is not exempt."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((buffer (get-buffer-create "dashboard")))
    (unwind-protect
        (should-not (cj/auto-dim--never-dim-dashboard-p buffer))
      (kill-buffer buffer))))

(ert-deftest test-auto-dim-config-never-dim-dashboard-other-buffer-dims ()
  "Error: an ordinary buffer is not exempt from dimming."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (with-temp-buffer
    (should-not (cj/auto-dim--never-dim-dashboard-p (current-buffer)))))

(provide 'test-auto-dim-config)
;;; test-auto-dim-config.el ends here
