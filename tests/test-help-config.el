;;; test-help-config.el --- Tests for the Info-open decision logic -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/open-with-info-mode opens the current .info buffer in Info, prompting to
;; save first if the buffer is modified.  The save/cancel/open decision is
;; factored into the pure helper `cj/--info-open-plan' so it's testable without
;; driving find-file, Info, or the save prompt.  Declining the prompt must yield
;; `cancel' -- the original cl-return-from inside a plain defun signalled
;; "No catch for tag" instead of cancelling.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'help-config)

(ert-deftest test-info-open-plan-unmodified-opens ()
  "Normal: an unmodified buffer opens in Info directly."
  (should (eq (cj/--info-open-plan nil nil) 'open)))

(ert-deftest test-info-open-plan-modified-confirmed-saves-then-opens ()
  "Normal: a modified buffer whose save is confirmed saves, then opens."
  (should (eq (cj/--info-open-plan t t) 'save-then-open)))

(ert-deftest test-info-open-plan-modified-declined-cancels ()
  "Error/edge: a modified buffer whose save is declined cancels -- the path that
used to signal \"No catch for tag\" via cl-return-from in a plain defun."
  (should (eq (cj/--info-open-plan t nil) 'cancel)))

(provide 'test-help-config)
;;; test-help-config.el ends here
