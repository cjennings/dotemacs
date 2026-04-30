;;; test-prog-python--format-wiring.el --- Verify the Python formatter is wired -*- lexical-binding: t; -*-

;;; Commentary:
;; Three checks for the Python formatter:
;;   1. `blacken-buffer' is fboundp after the prog-python config loads.
;;   2. C-; f in `python-ts-mode-map' resolves to `blacken-buffer'.
;;   3. The `black' executable is on PATH (skipped if not installed).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
;; use-package's `:bind (:map ...)' defers until the named map exists.
;; Loading python.el populates `python-ts-mode-map'; loading blacken
;; resolves the binding form's late-binding hook.
(require 'prog-python)
(require 'python)
(require 'blacken)

(ert-deftest test-prog-python-format-package-loaded ()
  "Normal: `blacken' is in `features' after the prog-python config loads."
  (should (featurep 'blacken)))

(ert-deftest test-prog-python-format-command-fboundp ()
  "Normal: `blacken-buffer' is fboundp after the package is loaded."
  (should (fboundp 'blacken-buffer)))

(ert-deftest test-prog-python-format-binding-resolves ()
  "Normal: C-; f in `python-ts-mode-map' is bound to `blacken-buffer'."
  (should (eq 'blacken-buffer
              (lookup-key python-ts-mode-map (kbd "C-; f")))))

(ert-deftest test-prog-python-format-executable-on-path ()
  "Boundary: the `black' executable is on PATH (skipped if not installed)."
  (format-test--skip-unless-executable "black")
  (should (executable-find "black")))

(provide 'test-prog-python--format-wiring)
;;; test-prog-python--format-wiring.el ends here
