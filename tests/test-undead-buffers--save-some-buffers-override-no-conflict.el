;;; test-undead-buffers--save-some-buffers-override-no-conflict.el --- Regression: save override vs undead -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression guard for a launch crash.  custom-buffer-file.el installs a
;; legible `save-some-buffers' override named `cj/save-some-buffers' (arity
;; arg + pred).  undead-buffers.el used to define its own 1-arg
;; `cj/save-some-buffers' that called `save-some-buffers' internally.  In prod
;; load order (custom-buffer-file before undead-buffers) the 1-arg version won
;; the symbol, so the override re-entered it with two args and signalled
;; wrong-number-of-arguments — crashing the kill-all-other-buffers startup path.
;;
;; The requires below reproduce that prod order on purpose.

;;; Code:

(require 'ert)
;; Prod order: override installed first, undead-buffers loaded second.
(require 'custom-buffer-file)
(require 'undead-buffers)

(ert-deftest test-undead-buffers-save-override-accepts-predicate ()
  "Normal: calling `save-some-buffers' with the undead predicate (the
kill-all-other-buffers path) goes through the legible override without a
wrong-number-of-arguments crash.  No modified file buffers exist in batch, so
the override returns a count rather than prompting."
  (should (numberp (save-some-buffers nil #'cj/undead-buffer-p))))

(ert-deftest test-undead-buffers-save-some-buffers-not-arity-shadowed ()
  "Boundary: `cj/save-some-buffers' must accept the PRED argument the override
forwards (max arity >= 2), so undead-buffers can't reintroduce a 1-arg shadow."
  (should (>= (cdr (func-arity #'cj/save-some-buffers)) 2)))

(provide 'test-undead-buffers--save-some-buffers-override-no-conflict)
;;; test-undead-buffers--save-some-buffers-override-no-conflict.el ends here
