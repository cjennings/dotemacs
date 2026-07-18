;;; test-vc-config--timemachine-commands.el --- Tests for git-timemachine command wiring -*- lexical-binding: t -*-

;;; Commentary:
;; Guards the git-timemachine autoload surface in vc-config.el.  The
;; upstream package defines no `git-timemachine-show-selected-revision';
;; an autoload for it in :commands creates a phantom M-x command that
;; errors after loading the package.  The real selector lives in the
;; cj/ namespace.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'vc-config)

;;; Normal Cases

(ert-deftest test-vc-timemachine-selected-revision-cj-command-defined ()
  "Normal: the cj/ selected-revision selector is defined by vc-config."
  (should (fboundp 'cj/git-timemachine-show-selected-revision)))

(ert-deftest test-vc-timemachine-entry-command-defined ()
  "Normal: the cj/git-timemachine entry command is defined."
  (should (commandp 'cj/git-timemachine)))

;;; Error Cases

(ert-deftest test-vc-timemachine-no-phantom-package-autoload ()
  "Error: no autoload exists for a function the package never defines.
An autoload stub for `git-timemachine-show-selected-revision' would
surface in M-x and signal void-function after the package loads."
  (should-not (fboundp 'git-timemachine-show-selected-revision)))

(provide 'test-vc-config--timemachine-commands)
;;; test-vc-config--timemachine-commands.el ends here
