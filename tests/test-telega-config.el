;;; test-telega-config.el --- Tests for Telegram (telega) module wiring -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight asserts that the module loads, exposes the launcher,
;; sets the docker preference, and registers the C-; G keybinding.
;; The bulk of telega.el's behaviour is owned upstream; the tests
;; here only cover what this config wires.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'telega-config)

(ert-deftest test-telega-config-loads-cleanly ()
  "Normal: module loads without errors and `provide's its feature."
  (should (featurep 'telega-config)))

(ert-deftest test-telega-config-launcher-binding-is-telega ()
  "Normal: =C-; G= invokes the launcher wrapper, which routes to
`telega' when installed or signals a helpful user-error otherwise."
  (should (eq (keymap-lookup cj/custom-keymap "G") #'cj/telega)))

(ert-deftest test-telega-config-launcher-without-package-signals-user-error ()
  "Error: with telega absent, the launcher signals a `user-error' that
mentions the recovery path instead of falling through to the autoload
stub's cryptic load-file failure."
  (cl-letf (((symbol-function 'featurep)
             (lambda (sym &optional _sub)
               (and (not (eq sym 'telega)) t)))
            ((symbol-function 'locate-library)
             (lambda (lib &rest _) (unless (equal lib "telega") t))))
    (let* ((err (should-error (cj/telega) :type 'user-error))
           (msg (error-message-string err)))
      (should (string-match-p "telega not installed" msg))
      (should (string-match-p "setup-telega\\.sh\\|package-install" msg)))))

(ert-deftest test-telega-config-launcher-with-package-calls-telega ()
  "Normal: with telega loadable, the launcher delegates to `telega'."
  (let (called)
    (cl-letf (((symbol-function 'featurep)
               (lambda (sym &optional _sub) (eq sym 'telega)))
              ((symbol-function 'telega)
               (lambda (&rest _) (setq called t))))
      (cj/telega))
    (should called)))

(provide 'test-telega-config)
;;; test-telega-config.el ends here
