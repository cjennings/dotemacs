;;; test-prog-webdev--format-wiring.el --- Verify the TS/JS formatter is wired -*- lexical-binding: t; -*-

;;; Commentary:
;; Webdev's binding is installed via `local-set-key' inside the
;; `cj/webdev-keybindings' hook, fired from typescript-ts-mode,
;; tsx-ts-mode, js-ts-mode, and web-mode. The format command itself
;; (`cj/webdev-format-buffer') is defined in prog-webdev.el and shells
;; out to the `prettier' binary.
;;
;; Three checks:
;;   1. `cj/webdev-format-buffer' is fboundp.
;;   2. The hook installs C-; f → `cj/webdev-format-buffer' in the
;;      buffer-local map.
;;   3. The `prettier' executable is on PATH.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
(ignore-errors (require 'prog-webdev))

(ert-deftest test-prog-webdev-format-command-fboundp ()
  "Normal: `cj/webdev-format-buffer' is fboundp from prog-webdev."
  (should (fboundp 'cj/webdev-format-buffer)))

(ert-deftest test-prog-webdev-format-binding-resolves ()
  "Normal: `cj/webdev-keybindings' installs C-; f → `cj/webdev-format-buffer'."
  (with-temp-buffer
    (cj/webdev-keybindings)
    (should (eq 'cj/webdev-format-buffer
                (local-key-binding (kbd "C-; f"))))))

(ert-deftest test-prog-webdev-format-executable-on-path ()
  "Boundary: `prettier' is on PATH (skipped if not installed)."
  (format-test--skip-unless-executable "prettier")
  (should (executable-find "prettier")))

(provide 'test-prog-webdev--format-wiring)
;;; test-prog-webdev--format-wiring.el ends here
