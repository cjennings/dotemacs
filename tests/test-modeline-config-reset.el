;;; test-modeline-config-reset.el --- cj/modeline-reset -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/modeline-reset', the repair command for buffers whose
;; `mode-line-format' was hijacked buffer-locally (two-column mode, ediff,
;; calc).  It kills the buffer-local value so the default format returns.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

(ert-deftest test-modeline-config-reset-kills-local-format ()
  "Normal: a hijacked buffer-local mode-line-format is removed."
  (with-temp-buffer
    (setq-local mode-line-format '("hijacked"))
    (should (local-variable-p 'mode-line-format))
    (cj/modeline-reset)
    (should-not (local-variable-p 'mode-line-format))
    (should (eq mode-line-format (default-value 'mode-line-format)))))

(ert-deftest test-modeline-config-reset-noop-without-local ()
  "Boundary: harmless when the buffer has no local mode-line-format."
  (with-temp-buffer
    (should-not (local-variable-p 'mode-line-format))
    (cj/modeline-reset)
    (should-not (local-variable-p 'mode-line-format))))

(ert-deftest test-modeline-config-reset-is-a-command ()
  "Normal: cj/modeline-reset is interactive."
  (should (commandp #'cj/modeline-reset)))

(provide 'test-modeline-config-reset)
;;; test-modeline-config-reset.el ends here
