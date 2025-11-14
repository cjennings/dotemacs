;;; test-flycheck-languagetool-setup.el --- Unit tests for LanguageTool setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests verifying LanguageTool installation and wrapper script setup.
;; Focus: Testing OUR code (wrapper script, file setup), not flycheck internals.
;;
;; We trust that flycheck works correctly (it's an external framework).
;; These tests verify:
;; - LanguageTool is installed and accessible
;; - Our wrapper script exists, is executable, and has correct structure
;; - Python 3 dependency is available
;;
;; Categories: Normal (installation checks), Boundary (script structure), Error (missing dependencies)

;;; Code:

(require 'ert)

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-flycheck-languagetool-setup-normal-wrapper-exists ()
  "Test that languagetool-flycheck wrapper script exists."
  (let ((wrapper-path (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck")))
    (should (file-exists-p wrapper-path))))

(ert-deftest test-flycheck-languagetool-setup-normal-wrapper-executable ()
  "Test that languagetool-flycheck wrapper script is executable."
  (let ((wrapper-path (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck")))
    (should (file-executable-p wrapper-path))))

(ert-deftest test-flycheck-languagetool-setup-normal-languagetool-installed ()
  "Test that languagetool command is available in PATH."
  (should (executable-find "languagetool")))

(ert-deftest test-flycheck-languagetool-setup-normal-python3-available ()
  "Test that python3 is available for wrapper script."
  (should (executable-find "python3")))


;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-flycheck-languagetool-setup-boundary-wrapper-script-format ()
  "Test that wrapper script has correct shebang and structure."
  (let ((wrapper-path (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck")))
    (with-temp-buffer
      (insert-file-contents wrapper-path)
      (goto-char (point-min))
      ;; Check shebang
      (should (looking-at "#!/usr/bin/env python3"))
      ;; Check it contains required imports
      (should (search-forward "import json" nil t))
      (should (search-forward "import subprocess" nil t)))))

;; ----------------------------- Error Cases -----------------------------------

(ert-deftest test-flycheck-languagetool-setup-error-missing-file-argument ()
  "Test that wrapper script requires file argument.
When called without arguments, wrapper should exit with error."
  (let* ((wrapper (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck"))
         (exit-code nil))
    (with-temp-buffer
      (setq exit-code (call-process wrapper nil t nil))
      ;; Should exit with non-zero status when no file provided
      (should-not (= 0 exit-code))
      ;; Should print usage message to stderr (captured in buffer)
      (goto-char (point-min))
      (should (or (search-forward "Usage:" nil t)
                  (search-forward "FILE" nil t))))))

(provide 'test-flycheck-languagetool-setup)
;;; test-flycheck-languagetool-setup.el ends here
