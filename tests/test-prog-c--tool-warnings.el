;;; test-prog-c--tool-warnings.el --- Load-time missing-tool warnings -*- lexical-binding: t; -*-

;;; Commentary:
;; The config audit flagged clangd for the load-time missing-tool
;; warning pyright/prettier already have.  clang-format is the same
;; class: its use-package block gates on `:if (executable-find ...)',
;; which evaluates once at startup, so an absent binary silently
;; disables the format key until the next restart — the warn is the
;; only visible trace.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-c)

(ert-deftest test-prog-c-warns-when-tools-missing ()
  "Error: loading without the C tools on PATH warns for each one."
  (let ((warned '()))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (push msg warned))))
      (load (expand-file-name "modules/prog-c.el" user-emacs-directory) nil t))
    (dolist (tool '("clangd" "clang-format"))
      (should (cl-some (lambda (m) (string-match-p (regexp-quote tool) m))
                       warned)))))

(provide 'test-prog-c--tool-warnings)
;;; test-prog-c--tool-warnings.el ends here
