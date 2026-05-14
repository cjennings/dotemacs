;;; test-prog-python-setup.el --- Tests for cj/python-setup mode hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the pure command builders and the interactive
;; mypy/debug wrappers.  `cj/python-setup' itself -- the per-buffer
;; preferences applied when `python-ts-mode' starts -- was uncovered.
;; The external modes (`company-mode', `flyspell-prog-mode',
;; `superword-mode', `electric-pair-local-mode', `lsp-deferred') are
;; stubbed so the test exercises the setq-local assignments and the
;; LSP gating without needing a real Python install or LSP backend.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-python)

(defmacro test-pp-setup--with-stubs (&rest body)
  "Stub mode-toggles and LSP primitives, then evaluate BODY."
  `(cl-letf (((symbol-function 'company-mode) #'ignore)
             ((symbol-function 'flyspell-prog-mode) #'ignore)
             ((symbol-function 'superword-mode) #'ignore)
             ((symbol-function 'electric-pair-local-mode) #'ignore))
     ,@body))

(ert-deftest test-prog-python-setup-sets-buffer-local-preferences ()
  "Normal: fill-column, tab-width, standard-indent, indent-tabs-mode all
land at the Python defaults (80 / 4 / 4 / nil)."
  (with-temp-buffer
    (test-pp-setup--with-stubs
     (cj/python-setup)
     (should (= fill-column 80))
     (should (= tab-width 4))
     (should (= standard-indent 4))
     (should-not indent-tabs-mode))))

(ert-deftest test-prog-python-setup-enables-mode-helpers ()
  "Normal: company-mode, flyspell-prog-mode, superword-mode, and
electric-pair-local-mode all get called once."
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
        (cj/python-setup))
      (should (memq 'company calls))
      (should (memq 'flyspell calls))
      (should (memq 'superword calls))
      ;; electric-pair-local-mode gets enabled (truthy arg)
      (should (assq 'pair calls))
      (should (cdr (assq 'pair calls))))))

(ert-deftest test-prog-python-setup-starts-lsp-when-pyright-on-path ()
  "Normal: with pyright on PATH, `lsp-deferred' is called."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'flyspell-prog-mode) #'ignore)
                ((symbol-function 'superword-mode) #'ignore)
                ((symbol-function 'electric-pair-local-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find)
                 (lambda (path) (when (equal path pyright-path)
                                  "/usr/bin/pyright"))))
        (cj/python-setup))
      (should started))))

(ert-deftest test-prog-python-setup-skips-lsp-without-pyright ()
  "Boundary: without pyright on PATH, `lsp-deferred' is NOT called."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'flyspell-prog-mode) #'ignore)
                ((symbol-function 'superword-mode) #'ignore)
                ((symbol-function 'electric-pair-local-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find) (lambda (_) nil)))
        (cj/python-setup))
      (should-not started))))

(provide 'test-prog-python-setup)
;;; test-prog-python-setup.el ends here
