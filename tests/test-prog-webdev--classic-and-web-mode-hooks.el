;;; test-prog-webdev--classic-and-web-mode-hooks.el --- Classic js + web-mode setup hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; The config audit found the webdev setup hooks attached to the
;; tree-sitter modes only, so a grammar-unavailable fallback to classic
;; js-mode silently lost indent/keys/LSP, and web-mode got the format
;; key but none of the promised setup (no company/flyspell/LSP in HTML
;; buffers).  These tests pin the classic js-mode hooks and the
;; web-mode setup hook, plus the html-language-server guard that keeps
;; LSP silent on machines without the server.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-webdev)

(ert-deftest test-prog-webdev-hooks-cover-classic-js-mode ()
  "Normal: classic js-mode runs the same setup + keybindings as js-ts-mode."
  (should (memq #'cj/webdev-setup js-mode-hook))
  (should (memq #'cj/webdev-keybindings js-mode-hook)))

(ert-deftest test-prog-webdev-web-mode-hook-runs-setup ()
  "Normal: web-mode runs `cj/web-mode-setup' alongside the keybindings."
  (should (memq #'cj/web-mode-setup web-mode-hook))
  (should (memq #'cj/webdev-keybindings web-mode-hook)))

(ert-deftest test-prog-webdev-web-mode-setup-sets-buffer-local-preferences ()
  "Normal: web-mode setup lands the shared webdev buffer preferences."
  (with-temp-buffer
    (cl-letf (((symbol-function 'company-mode) #'ignore)
              ((symbol-function 'flyspell-prog-mode) #'ignore)
              ((symbol-function 'superword-mode) #'ignore)
              ((symbol-function 'electric-pair-local-mode) #'ignore)
              ((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
      (cj/web-mode-setup))
    (should (= fill-column 100))
    (should (= tab-width 2))
    (should-not indent-tabs-mode)))

(ert-deftest test-prog-webdev-web-mode-setup-starts-lsp-when-html-server-on-path ()
  "Normal: with the html language server on PATH, `lsp-deferred' fires."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'flyspell-prog-mode) #'ignore)
                ((symbol-function 'superword-mode) #'ignore)
                ((symbol-function 'electric-pair-local-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find)
                 (lambda (path &rest _)
                   (when (equal path html-language-server-path)
                     "/usr/bin/vscode-html-language-server"))))
        (cj/web-mode-setup))
      (should started))))

(ert-deftest test-prog-webdev-web-mode-setup-skips-lsp-without-html-server ()
  "Boundary: without the html language server, `lsp-deferred' is NOT called."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'flyspell-prog-mode) #'ignore)
                ((symbol-function 'superword-mode) #'ignore)
                ((symbol-function 'electric-pair-local-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
        (cj/web-mode-setup))
      (should-not started))))

(provide 'test-prog-webdev--classic-and-web-mode-hooks)
;;; test-prog-webdev--classic-and-web-mode-hooks.el ends here
