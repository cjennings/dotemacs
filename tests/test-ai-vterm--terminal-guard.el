;;; test-ai-vterm--terminal-guard.el --- Tests for the terminal-frame guard -*- lexical-binding: t; -*-

;;; Commentary:
;; AI-vterm launches a graphical vterm side window, so it is GUI-only.
;; `cj/--ai-vterm-refuse-in-terminal' signals a `user-error' when the
;; current frame is a terminal frame; each interactive entry point
;; (`cj/ai-vterm', `cj/ai-vterm-pick-project', `cj/ai-vterm-close')
;; calls it first so F9 and friends decline -- with a message -- in a
;; terminal frame instead of launching a vterm.  The check is per-frame
;; at command time, not at load, so a daemon serving both GUI and
;; terminal frames keeps the launcher working in its GUI frames.
;;
;; `env-terminal-p' is mocked so the tests are deterministic regardless
;; of whether the run itself is graphical (batch runs are terminal).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)

;; ---------------------------- the guard helper ----------------------------

(ert-deftest test-ai-vterm--refuse-in-terminal-errors-in-terminal-frame ()
  "Error: terminal frame -> `user-error', so the command declines."
  (cl-letf (((symbol-function 'env-terminal-p) (lambda () t)))
    (should-error (cj/--ai-vterm-refuse-in-terminal) :type 'user-error)))

(ert-deftest test-ai-vterm--refuse-in-terminal-passes-in-gui-frame ()
  "Normal: GUI frame -> returns nil, no error, command proceeds."
  (cl-letf (((symbol-function 'env-terminal-p) (lambda () nil)))
    (should-not (cj/--ai-vterm-refuse-in-terminal))))

;; ------------------- the three interactive entry points -------------------

(ert-deftest test-ai-vterm-f9-declines-in-terminal-without-dispatching ()
  "Error: F9 in a terminal frame errors and never reaches dispatch."
  (let ((dispatched nil))
    (cl-letf (((symbol-function 'env-terminal-p) (lambda () t))
              ((symbol-function 'cj/--ai-vterm-dispatch)
               (lambda () (setq dispatched t) '(pick-project))))
      (should-error (cj/ai-vterm) :type 'user-error)
      (should-not dispatched))))

(ert-deftest test-ai-vterm-pick-project-declines-in-terminal-without-prompting ()
  "Error: C-F9 in a terminal frame errors and never reaches the picker."
  (let ((prompted nil))
    (cl-letf (((symbol-function 'env-terminal-p) (lambda () t))
              ((symbol-function 'cj/--ai-vterm-pick-project)
               (lambda () (setq prompted t) "/tmp")))
      (should-error (cj/ai-vterm-pick-project) :type 'user-error)
      (should-not prompted))))

(ert-deftest test-ai-vterm-close-declines-in-terminal-without-targeting ()
  "Error: M-F9 in a terminal frame errors and never reaches close-target."
  (let ((targeted nil))
    (cl-letf (((symbol-function 'env-terminal-p) (lambda () t))
              ((symbol-function 'cj/--ai-vterm-close-target)
               (lambda () (setq targeted t) nil)))
      (should-error (cj/ai-vterm-close) :type 'user-error)
      (should-not targeted))))

(ert-deftest test-ai-vterm-f9-passes-guard-in-gui-frame ()
  "Normal: F9 in a GUI frame passes the guard and reaches dispatch."
  (let ((dispatched nil))
    (cl-letf (((symbol-function 'env-terminal-p) (lambda () nil))
              ((symbol-function 'cj/--ai-vterm-dispatch)
               (lambda () (setq dispatched t) '(pick-project)))
              ((symbol-function 'cj/ai-vterm-pick-project)
               (lambda (&optional _arg) nil)))
      (cj/ai-vterm)
      (should dispatched))))

(provide 'test-ai-vterm--terminal-guard)
;;; test-ai-vterm--terminal-guard.el ends here
