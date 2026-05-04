;;; test-prog-c-mode-settings.el --- Tests for cj/c-mode-settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the C mode hook applies the documented buffer-local values and
;; only calls `lsp-deferred' when both the function and clangd are available.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-c)

(ert-deftest test-prog-c-mode-settings-normal-applies-buffer-locals ()
  "Normal: cj/c-mode-settings applies the documented buffer-local values."
  (with-temp-buffer
    (cl-letf (((symbol-function 'auto-fill-mode) (lambda (&rest _) nil))
              ((symbol-function 'electric-pair-mode) (lambda (&rest _) nil))
              ((symbol-function 'lsp-deferred) (lambda (&rest _) nil))
              ((symbol-function 'executable-find) (lambda (_) nil)))
      (cj/c-mode-settings))
    (should (eq indent-tabs-mode nil))
    (should (= c-basic-offset 4))
    (should (= tab-width 4))
    (should (= fill-column 80))
    (should comment-auto-fill-only-comments)))

(ert-deftest test-prog-c-mode-settings-boundary-lsp-available-deferred ()
  "Boundary: when lsp-deferred is fbound and clangd is on PATH, lsp-deferred is called."
  (let ((lsp-calls 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'auto-fill-mode) (lambda (&rest _) nil))
                ((symbol-function 'electric-pair-mode) (lambda (&rest _) nil))
                ((symbol-function 'lsp-deferred) (lambda () (cl-incf lsp-calls)))
                ((symbol-function 'executable-find) (lambda (_) "/usr/bin/clangd")))
        (cj/c-mode-settings)))
    (should (= lsp-calls 1))))

(ert-deftest test-prog-c-mode-settings-error-clangd-missing-skips-lsp ()
  "Error: when clangd is not on PATH, lsp-deferred is NOT called."
  (let ((lsp-calls 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'auto-fill-mode) (lambda (&rest _) nil))
                ((symbol-function 'electric-pair-mode) (lambda (&rest _) nil))
                ((symbol-function 'lsp-deferred) (lambda () (cl-incf lsp-calls)))
                ((symbol-function 'executable-find) (lambda (_) nil)))
        (cj/c-mode-settings)))
    (should (zerop lsp-calls))))

(provide 'test-prog-c-mode-settings)
;;; test-prog-c-mode-settings.el ends here
