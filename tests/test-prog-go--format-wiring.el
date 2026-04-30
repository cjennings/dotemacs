;;; test-prog-go--format-wiring.el --- Verify the Go formatter is wired -*- lexical-binding: t; -*-

;;; Commentary:
;; Go's binding is installed via `local-set-key' inside the
;; `cj/go-mode-keybindings' hook, not via use-package's `:bind'. The
;; test runs the hook directly in a temp buffer and asks the buffer-
;; local map what C-; f resolves to.
;;
;; Three checks:
;;   1. `gofmt' is fboundp after go-mode loads.
;;   2. The hook installs C-; f → `gofmt' in the buffer-local map.
;;   3. `gofmt' (or `goimports') is on PATH.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
(ignore-errors (require 'prog-go))
(require 'go-mode)

(ert-deftest test-prog-go-format-command-fboundp ()
  "Normal: `gofmt' is fboundp after `go-mode' loads."
  (should (fboundp 'gofmt)))

(ert-deftest test-prog-go-format-binding-resolves ()
  "Normal: `cj/go-mode-keybindings' installs C-; f → `gofmt' locally."
  (with-temp-buffer
    (cj/go-mode-keybindings)
    (should (eq 'gofmt (local-key-binding (kbd "C-; f"))))))

(ert-deftest test-prog-go-format-executable-on-path ()
  "Boundary: `gofmt' or `goimports' is on PATH (skipped if neither installed).
The Go config sets `gofmt-command' to \"goimports\" but falls back to
plain `gofmt' if goimports isn't installed."
  (unless (or (executable-find "gofmt") (executable-find "goimports"))
    (ert-skip "Neither gofmt nor goimports on PATH"))
  (should (or (executable-find "gofmt")
              (executable-find "goimports"))))

(provide 'test-prog-go--format-wiring)
;;; test-prog-go--format-wiring.el ends here
