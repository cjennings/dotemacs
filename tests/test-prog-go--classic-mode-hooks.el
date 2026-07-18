;;; test-prog-go--classic-mode-hooks.el --- Classic go-mode hooks + gopls warn -*- lexical-binding: t; -*-

;;; Commentary:
;; The config audit found the Go setup hooks attached to go-ts-mode
;; only, so a fallback to classic go-mode silently lost indent, keys,
;; and LSP.  It also flagged gopls for the load-time missing-tool
;; warning pyright/prettier already have; that warn is only honest if
;; ~/go/bin is on `exec-path' at load time (it used to join only in
;; go-mode's deferred :config, so gopls installed there read as
;; missing).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-go)

(ert-deftest test-prog-go-hooks-cover-classic-go-mode ()
  "Normal: classic go-mode runs the same setup + keybindings as go-ts-mode."
  (dolist (hook '(go-mode-hook go-ts-mode-hook))
    (should (memq #'cj/go-setup (symbol-value hook)))
    (should (memq #'cj/go-mode-keybindings (symbol-value hook)))))

(ert-deftest test-prog-go-bin-path-on-exec-path-at-load ()
  "Normal: ~/go/bin joins `exec-path' at module load, not first go buffer.
The load-time gopls warn resolves through `exec-path'; registering the
Go bin directory only in go-mode's deferred :config would make gopls
installed there warn as missing on every start."
  (should (member go-bin-path exec-path)))

(ert-deftest test-prog-go-warns-when-gopls-missing ()
  "Error: loading the module without gopls on PATH warns about gopls."
  (let ((warned '()))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (push msg warned))))
      (load (expand-file-name "modules/prog-go.el" user-emacs-directory) nil t))
    (should (cl-some (lambda (m) (string-match-p "gopls" m)) warned))))

(provide 'test-prog-go--classic-mode-hooks)
;;; test-prog-go--classic-mode-hooks.el ends here
