;;; test-auto-dim-config.el --- Tests for the auto-dim-other-buffers config -*- lexical-binding: t; -*-

;;; Commentary:
;; auto-dim-config configures the local auto-dim-other-buffers fork: dim only
;; non-selected windows within Emacs (not the whole frame on focus-out), drop
;; fringe from the dimmed faces to avoid flicker on this non-pgtk build, and
;; enable the global mode.  Guarded with `skip-unless' because the fork lives
;; in ~/code and may be absent on a clean checkout.
;;
;; The vterm dim-integration tests were removed when the terminal engine moved
;; off vterm.  EAT (the current engine) renders in real Emacs faces and uses the
;; `default' face for its background, so terminal buffers dim like any other
;; buffer with no dedicated integration.

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

(defconst test-auto-dim--flat-dimmed-org-faces
  (append (mapcar (lambda (n) (intern (format "org-level-%d" n)))
                  (number-sequence 1 8))
          '(org-link org-tag))
  "Org faces that must flat-dim to the `auto-dim-other-buffers' face.
These carry no signal that a dedicated -dim variant would preserve: the
theme gives `org-level-1' through `org-level-8' one shared foreground and
no height or weight, `org-link' keeps its underline through a relative
remap, and `org-tag' keeps its surrounding colons.")

(ert-deftest test-auto-dim-config-org-structure-faces-flat-dim ()
  "Normal: org heading, link, and tag faces remap to the flat dim face."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (face test-auto-dim--flat-dimmed-org-faces)
    (let ((entry (assq face auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq 'auto-dim-other-buffers (car (cdr entry))))
      (should (null (cdr (cdr entry)))))))

(ert-deftest test-auto-dim-config-keyword-faces-keep-dim-variants ()
  "Boundary: org TODO-keyword faces keep dedicated -dim variants, not flat dim.
Keyword status is scanned across unfocused windows, so it earns a variant;
heading colour does not.  Guards the flat-dim change from over-reaching."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (pair '((org-faces-todo       . org-faces-todo-dim)
                  (org-faces-doing      . org-faces-doing-dim)
                  (org-faces-priority-a . org-faces-priority-a-dim)))
    (let ((entry (assq (car pair) auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq (cdr pair) (car (cdr entry)))))))

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
