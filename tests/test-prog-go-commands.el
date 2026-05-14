;;; test-prog-go-commands.el --- Tests for prog-go setup + commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the four functions in prog-go.el:
;;
;;   cj/go-setup
;;   cj/go-staticcheck
;;   cj/go-debug
;;   cj/go-mode-keybindings
;;
;; External modes and process primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)
(format-test--ensure-packages-init)
(require 'prog-go)

;;; cj/go-setup

(ert-deftest test-prog-go-setup-sets-buffer-local-preferences ()
  "Normal: tab-width 4, standard-indent 4, indent-tabs-mode t (Go convention)."
  (with-temp-buffer
    (cl-letf (((symbol-function 'company-mode) #'ignore)
              ((symbol-function 'electric-pair-mode) #'ignore))
      (cj/go-setup)
      (should (= tab-width 4))
      (should (= standard-indent 4))
      (should indent-tabs-mode))))

(ert-deftest test-prog-go-setup-enables-mode-helpers ()
  "Normal: company-mode and electric-pair-mode are both called."
  (with-temp-buffer
    (let ((called nil))
      (cl-letf (((symbol-function 'company-mode)
                 (lambda (&rest _) (push 'company called)))
                ((symbol-function 'electric-pair-mode)
                 (lambda (arg) (push (cons 'pair arg) called))))
        (cj/go-setup))
      (should (memq 'company called))
      (should (assq 'pair called)))))

(ert-deftest test-prog-go-setup-starts-lsp-when-gopls-on-path ()
  "Normal: with gopls on PATH, `lsp-deferred' is called."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'electric-pair-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find)
                 (lambda (path) (when (equal path gopls-path) "/usr/bin/gopls"))))
        (cj/go-setup))
      (should started))))

(ert-deftest test-prog-go-setup-skips-lsp-without-gopls ()
  "Boundary: without gopls on PATH, `lsp-deferred' is NOT called."
  (with-temp-buffer
    (let ((started nil))
      (cl-letf (((symbol-function 'company-mode) #'ignore)
                ((symbol-function 'electric-pair-mode) #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'executable-find) (lambda (_) nil)))
        (cj/go-setup))
      (should-not started))))

;;; cj/go-staticcheck

(ert-deftest test-prog-go-staticcheck-runs-compile-when-binary-present ()
  "Normal: with staticcheck on disk, `compile' is called with `./...'."
  (let (compiled)
    (cl-letf (((symbol-function 'file-executable-p) (lambda (_) t))
              ((symbol-function 'compile)
               (lambda (cmd &rest _) (setq compiled cmd))))
      (with-temp-buffer
        (cj/go-staticcheck)))
    (should (stringp compiled))
    (should (string-match-p "./\\.\\.\\." compiled))))

(ert-deftest test-prog-go-staticcheck-messages-when-missing ()
  "Error: missing staticcheck -> message + no compile."
  (let ((compiled nil)
        (msg nil))
    (cl-letf (((symbol-function 'file-executable-p) (lambda (_) nil))
              ((symbol-function 'compile)
               (lambda (&rest _) (setq compiled t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (with-temp-buffer
        (cj/go-staticcheck)))
    (should-not compiled)
    (should (string-match-p "staticcheck not found" msg))))

;;; cj/go-debug

(ert-deftest test-prog-go-debug-runs-gud-gdb-when-delve-on-path ()
  "Normal: with delve on PATH, `gud-gdb' is called with `dlv debug'."
  (let (started)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (path) (when (equal path dlv-path) "/usr/bin/dlv")))
              ((symbol-function 'file-executable-p) (lambda (_) nil))
              ((symbol-function 'gud-gdb)
               (lambda (cmd &rest _) (setq started cmd))))
      (with-temp-buffer
        (cj/go-debug)))
    (should (stringp started))
    (should (string-match-p "dlv debug" started))))

(ert-deftest test-prog-go-debug-messages-when-delve-missing ()
  "Error: delve missing -> message + no gud-gdb call."
  (let ((started nil)
        (msg nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
              ((symbol-function 'file-executable-p) (lambda (_) nil))
              ((symbol-function 'gud-gdb)
               (lambda (&rest _) (setq started t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (with-temp-buffer
        (cj/go-debug)))
    (should-not started)
    (should (string-match-p "Delve not found" msg))))

;;; cj/go-mode-keybindings

(ert-deftest test-prog-go-mode-keybindings-binds-the-keys ()
  "Normal: C-; f -> gofmt, S-<f5> -> staticcheck, S-<f6> -> debug."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cj/go-mode-keybindings)
    (should (eq #'gofmt (local-key-binding (kbd "C-; f"))))
    (should (eq #'cj/go-staticcheck (local-key-binding (kbd "S-<f5>"))))
    (should (eq #'cj/go-debug (local-key-binding (kbd "S-<f6>"))))))

(provide 'test-prog-go-commands)
;;; test-prog-go-commands.el ends here
