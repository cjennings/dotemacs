;;; test-prog-webdev-setup.el --- Tests for cj/webdev-setup mode hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the prettier format wrapper and command builder.
;; `cj/webdev-setup' (the shared per-buffer preferences for TS/JS/TSX
;; modes) was uncovered.  Same stub-and-assert shape as the prog-python
;; setup test.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-webdev)

(defmacro test-pw-setup--with-stubs (&rest body)
  "Stub mode-toggles + LSP primitives, then evaluate BODY."
  `(cl-letf (((symbol-function 'company-mode) #'ignore)
             ((symbol-function 'flyspell-prog-mode) #'ignore)
             ((symbol-function 'superword-mode) #'ignore)
             ((symbol-function 'electric-pair-local-mode) #'ignore))
     ,@body))

(ert-deftest test-prog-webdev-setup-sets-buffer-local-preferences ()
  "Normal: fill-column, tab-width, standard-indent, indent-tabs-mode all
land at the webdev defaults (100 / 2 / 2 / nil)."
  (with-temp-buffer
    (test-pw-setup--with-stubs
     (cj/webdev-setup)
     (should (= fill-column 100))
     (should (= tab-width 2))
     (should (= standard-indent 2))
     (should-not indent-tabs-mode))))

(ert-deftest test-prog-webdev-setup-enables-mode-helpers ()
  "Normal: company-mode, flyspell-prog-mode, superword-mode, and
electric-pair-local-mode all get called."
  (with-temp-buffer
    (let ((calls nil))
      (cl-letf (((symbol-function 'company-mode)
                 (lambda (&rest _) (push 'company calls)))
                ((symbol-function 'flyspell-prog-mode)
                 (lambda (&rest _) (push 'flyspell calls)))
                ((symbol-function 'superword-mode)
                 (lambda (&rest _) (push 'superword calls)))
                ((symbol-function 'electric-pair-local-mode)
                 (lambda (arg) (push (cons 'pair arg) calls))))
        (cj/webdev-setup))
      (should (memq 'company calls))
      (should (memq 'flyspell calls))
      (should (memq 'superword calls))
      (should (assq 'pair calls))
      (should (cdr (assq 'pair calls))))))

(ert-deftest test-prog-webdev-setup-starts-lsp-when-tsserver-on-path ()
  "Normal: with typescript-language-server on PATH, `lsp-deferred' fires."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'flyspell-prog-mode) #'ignore)
                ((symbol-function 'superword-mode) #'ignore)
                ((symbol-function 'electric-pair-local-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find)
                 (lambda (path &rest _) (when (equal path ts-language-server-path)
                                  "/usr/bin/typescript-language-server"))))
        (cj/webdev-setup))
      (should started))))

(ert-deftest test-prog-webdev-setup-skips-lsp-without-tsserver ()
  "Boundary: without typescript-language-server, `lsp-deferred' is NOT called."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'flyspell-prog-mode) #'ignore)
                ((symbol-function 'superword-mode) #'ignore)
                ((symbol-function 'electric-pair-local-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
        (cj/webdev-setup))
      (should-not started))))

(ert-deftest test-prog-webdev-keybindings-binds-format-key ()
  "Normal: `cj/webdev-keybindings' wires C-; f to the prettier wrapper."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cj/webdev-keybindings)
    (should (eq #'cj/webdev-format-buffer (local-key-binding (kbd "C-; f"))))))

(provide 'test-prog-webdev-setup)
;;; test-prog-webdev-setup.el ends here
