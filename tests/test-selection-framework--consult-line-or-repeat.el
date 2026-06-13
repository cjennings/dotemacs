;;; test-selection-framework--consult-line-or-repeat.el --- Tests for the C-s dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/consult-line-or-repeat' (bound to `C-s') runs `consult-line', or
;; `vertico-repeat' when it was also the previous command -- so a second press
;; in a row repeats the last search instead of starting a fresh one.  These
;; tests exercise both branches with `consult-line' / `vertico-repeat' stubbed.
;;
;; Like `test-selection-framework-keybindings.el', this shadows `use-package'
;; with a no-op so the module loads without pulling in vertico/consult/embark/
;; company/prescient and their load-time side effects.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defmacro use-package (&rest _args)
  "Ignore package configuration while loading selection-framework in tests."
  nil)

(defun consult-line (&rest _args)
  "Test stub for `consult-line'.")
(defun vertico-repeat (&rest _args)
  "Test stub for `vertico-repeat'.")

(require 'selection-framework)

(defmacro test-sf--with-spies (&rest body)
  "Run BODY with `consult-line' and `vertico-repeat' recording into
`consulted' and `repeated', both bound to nil first."
  `(let (consulted repeated)
     (cl-letf (((symbol-function 'consult-line) (lambda (&rest _) (setq consulted t)))
               ((symbol-function 'vertico-repeat) (lambda (&rest _) (setq repeated t))))
       ,@body)))

(ert-deftest test-selection-framework-consult-line-or-repeat-fresh-search ()
  "Normal: when the previous command was something else, do a fresh `consult-line'."
  (test-sf--with-spies
   (let ((last-command 'some-other-command))
     (cj/consult-line-or-repeat))
   (should consulted)
   (should-not repeated)))

(ert-deftest test-selection-framework-consult-line-or-repeat-repeats-on-second-call ()
  "Normal: a second invocation in a row repeats the last search."
  (test-sf--with-spies
   (let ((last-command 'cj/consult-line-or-repeat))
     (cj/consult-line-or-repeat))
   (should repeated)
   (should-not consulted)))

(ert-deftest test-selection-framework-consult-line-or-repeat-nil-last-command ()
  "Boundary: nil `last-command' is not a repeat -> fresh search."
  (test-sf--with-spies
   (let ((last-command nil))
     (cj/consult-line-or-repeat))
   (should consulted)
   (should-not repeated)))

(ert-deftest test-selection-framework-consult-line-or-repeat-is-a-command ()
  "Normal: `cj/consult-line-or-repeat' is an interactive command."
  (should (commandp #'cj/consult-line-or-repeat)))

(ert-deftest test-selection-framework-vertico-repeat-save-on-minibuffer-setup ()
  "Normal: loading the module registers `vertico-repeat-save' on
`minibuffer-setup-hook'.  Without it `vertico-repeat' has no saved session
and the second C-s signals \"No Vertico session\"."
  (should (memq 'vertico-repeat-save minibuffer-setup-hook)))

(provide 'test-selection-framework--consult-line-or-repeat)
;;; test-selection-framework--consult-line-or-repeat.el ends here
