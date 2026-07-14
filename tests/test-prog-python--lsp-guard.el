;;; test-prog-python--lsp-guard.el --- Python LSP guard tests -*- lexical-binding: t; -*-

;;; Commentary:
;; The config audit found lsp-pyright's unguarded :hook lambda calling
;; (require 'lsp-pyright) + (lsp-deferred) on every python-ts buffer, so
;; pyright-less machines got the LSP attach prompt that cj/python-setup's
;; guard exists to prevent.  The guarded branch now owns the require and
;; the attach, and both classic and treesit modes run the same setup.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-python)

(defmacro test-python-guard--setup (pyright-found &rest body)
  "Run `cj/python-setup' with pyright presence set to PYRIGHT-FOUND.
Records requires into `required' and lsp attaches into `attached';
BODY sees both.  Package minor modes are stubbed at their boundary."
  (declare (indent 1))
  `(let ((required '()) (attached nil))
     (cl-letf (((symbol-function 'company-mode) #'ignore)
               ((symbol-function 'flyspell-prog-mode) #'ignore)
               ((symbol-function 'superword-mode) #'ignore)
               ((symbol-function 'executable-find)
                (lambda (&rest _) ,pyright-found))
               ((symbol-function 'require)
                (lambda (feature &rest _) (push feature required) feature))
               ((symbol-function 'lsp-deferred)
                (lambda () (setq attached t))))
       (with-temp-buffer
         (cj/python-setup))
       ,@body)))

(ert-deftest test-prog-python-setup-pyright-absent-no-lsp ()
  "Error: without pyright, setup neither loads lsp-pyright nor attaches."
  (test-python-guard--setup nil
    (should-not (memq 'lsp-pyright required))
    (should-not attached)))

(ert-deftest test-prog-python-setup-pyright-present-attaches ()
  "Normal: with pyright on PATH, setup loads lsp-pyright then attaches."
  (test-python-guard--setup "/usr/bin/pyright"
    (should (memq 'lsp-pyright required))
    (should attached)))

(ert-deftest test-prog-python-hooks-cover-both-mode-variants ()
  "Normal: classic and treesit python modes both run the same setup."
  (dolist (hook '(python-mode-hook python-ts-mode-hook))
    (should (memq #'cj/python-setup (symbol-value hook)))
    (should (memq #'cj/python-mode-keybindings (symbol-value hook)))))

(ert-deftest test-prog-python-no-anonymous-hook-lambdas ()
  "Boundary: no anonymous lambda remains on the python hooks.
The audit's unguarded lsp-pyright lambda was anonymous; symbols only
means every hook entry is a named, greppable function."
  (dolist (hook '(python-mode-hook python-ts-mode-hook))
    (dolist (fn (symbol-value hook))
      (should (symbolp fn)))))

(provide 'test-prog-python--lsp-guard)
;;; test-prog-python--lsp-guard.el ends here
