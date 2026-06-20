;;; test-prog-general--deadgrep.el --- Tests for the deadgrep helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/deadgrep--initial-term (region text or symbol at point) and cj/--deadgrep-run
;; (the normalize-root + read-term + invoke tail shared by cj/deadgrep-here and
;; cj/deadgrep-in-dir) were lifted out of the deadgrep use-package :config.
;; deadgrep is mocked at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

(ert-deftest test-prg-deadgrep-initial-term-symbol-at-point ()
  "Normal: with no region, the symbol at point seeds the search."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (should (equal (cj/deadgrep--initial-term) "hello"))))

(ert-deftest test-prg-deadgrep-initial-term-region ()
  "Normal: an active region's text seeds the search."
  (with-temp-buffer
    (insert "needle")
    (transient-mark-mode 1)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (should (equal (cj/deadgrep--initial-term) "needle"))))

(ert-deftest test-prg-deadgrep-run-normalizes-root-and-passes-term ()
  "Normal: ROOT is normalized to a directory and TERM is passed through."
  (let (got-term got-root)
    (cl-letf (((symbol-function 'deadgrep)
               (lambda (term root) (setq got-term term got-root root))))
      (cj/--deadgrep-run "/tmp/foo" "needle"))
    (should (equal got-term "needle"))
    (should (equal got-root "/tmp/foo/"))))

(provide 'test-prog-general--deadgrep)
;;; test-prog-general--deadgrep.el ends here
