;;; test-prog-shell--tool-warnings.el --- Load-time missing-tool warnings -*- lexical-binding: t; -*-

;;; Commentary:
;; The config audit flagged bash-language-server, shfmt, and shellcheck
;; for the load-time missing-tool warnings pyright/prettier already
;; have.  The shfmt and flycheck use-package blocks gate on `:if
;; (executable-find ...)', which evaluates once at startup — an absent
;; tool silently disables that setup until the next restart, so the
;; warn is the only visible trace.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-shell)

(ert-deftest test-prog-shell-warns-when-tools-missing ()
  "Error: loading without the shell tools on PATH warns for each one."
  (let ((warned '()))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (push msg warned))))
      (load (expand-file-name "modules/prog-shell.el" user-emacs-directory) nil t))
    (dolist (tool '("bash-language-server" "shfmt" "shellcheck"))
      (should (cl-some (lambda (m) (string-match-p (regexp-quote tool) m))
                       warned)))))

(provide 'test-prog-shell--tool-warnings)
;;; test-prog-shell--tool-warnings.el ends here
