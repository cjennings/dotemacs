;;; test-prog-shell--format-wiring.el --- Verify the Shell formatter is wired -*- lexical-binding: t; -*-

;;; Commentary:
;; The shfmt use-package gates on `:if (executable-find shfmt-path)' so
;; the keymap entries only install when shfmt is on PATH. The bind/
;; fboundp tests therefore call the executable-skip helper first.
;;
;; Three checks for the shell formatter, each repeated for the two
;; relevant mode-maps (`sh-mode-map' and `bash-ts-mode-map'):
;;   1. `shfmt-buffer' is fboundp.
;;   2. C-; f in each map resolves to `shfmt-buffer'.
;;   3. The `shfmt' executable is on PATH.

;;; Code:

(require 'ert)
(require 'sh-script)  ; provides sh-mode-map

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
;; bash-ts-mode is autoloaded from sh-script in modern Emacs; force
;; the map into existence by referencing the mode definition.  When
;; tree-sitter isn't built in, bash-ts-mode is still defined as a
;; symbol via define-derived-mode, and its keymap exists.
(ignore-errors (require 'prog-shell))
(when (executable-find "shfmt")
  (require 'shfmt))

(ert-deftest test-prog-shell-format-command-fboundp ()
  "Normal: `shfmt-buffer' is fboundp after shfmt loads.
Skipped when shfmt isn't installed because the use-package gates on
`:if (executable-find shfmt-path)'."
  (format-test--skip-unless-executable "shfmt")
  (should (fboundp 'shfmt-buffer)))

(ert-deftest test-prog-shell-format-binding-in-sh-mode ()
  "Normal: C-; f in `sh-mode-map' is bound to `shfmt-buffer'."
  (format-test--skip-unless-executable "shfmt")
  (should (eq 'shfmt-buffer
              (lookup-key sh-mode-map (kbd "C-; f")))))

(ert-deftest test-prog-shell-format-binding-in-bash-ts-mode ()
  "Normal: C-; f in `bash-ts-mode-map' is bound to `shfmt-buffer'."
  (format-test--skip-unless-executable "shfmt")
  (should (boundp 'bash-ts-mode-map))
  (should (eq 'shfmt-buffer
              (lookup-key bash-ts-mode-map (kbd "C-; f")))))

(ert-deftest test-prog-shell-format-executable-on-path ()
  "Boundary: the `shfmt' executable is on PATH (skipped if not installed)."
  (format-test--skip-unless-executable "shfmt")
  (should (executable-find "shfmt")))

(provide 'test-prog-shell--format-wiring)
;;; test-prog-shell--format-wiring.el ends here
