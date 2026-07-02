;;; test-modeline-config-flycheck-segment.el --- Flycheck segment in modeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke test that the custom modeline's `mode-line-format' includes
;; a guarded reference to `cj/--modeline-flycheck-status', and that
;; the guard requires both `mode-line-window-selected-p' and
;; `bound-and-true-p flycheck-mode'.  See
;; docs/specs/flycheck-modeline-customization-spec-implemented.org for the design.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

(ert-deftest test-modeline-config-flycheck-segment-present ()
  "`mode-line-format' contains an :eval form invoking cj/--modeline-flycheck-status."
  (let ((printed (format "%S" (default-value 'mode-line-format))))
    (should (string-match-p "cj/--modeline-flycheck-status" printed))))

(ert-deftest test-modeline-config-flycheck-segment-guarded-by-active-window ()
  "Flycheck segment gates on `mode-line-window-selected-p'."
  (let ((printed (format "%S" (default-value 'mode-line-format))))
    (should (string-match-p "mode-line-window-selected-p" printed))))

(ert-deftest test-modeline-config-flycheck-segment-guarded-by-flycheck-mode ()
  "Flycheck segment gates on `bound-and-true-p flycheck-mode'.
The `bound-and-true-p' guard makes the form safe in buffers where
flycheck never loaded."
  (let ((printed (format "%S" (default-value 'mode-line-format))))
    (should (string-match-p "bound-and-true-p flycheck-mode" printed))))

(provide 'test-modeline-config-flycheck-segment)
;;; test-modeline-config-flycheck-segment.el ends here
