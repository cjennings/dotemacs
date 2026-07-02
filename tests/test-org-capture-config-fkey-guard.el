;;; test-org-capture-config-fkey-guard.el --- capture F-key guard -*- lexical-binding: t; -*-

;;; Commentary:
;; While a capture is in progress, the global popup keys (F1 dashboard
;; sweep, F10 music, F11 dirvish-side, F12 terminal, M-SPC agent swap)
;; must not fire and pop UI over the capture.  org-capture-mode is a
;; minor mode active exactly for the capture's duration and its keymap
;; shadows the global map, so the guard binds those keys there to a
;; blocker that signals a `user-error' naming the way out.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load org-capture first so the module's with-eval-after-load fires
;; synchronously.
(require 'org-capture)
(require 'org-capture-config)

(ert-deftest test-org-capture-fkey-guard-keys-bound ()
  "Normal: every leaking popup key is bound to the blocker in capture mode."
  (dolist (key '("<f1>" "<f10>" "<f11>" "<f12>" "M-SPC"))
    (should (eq (keymap-lookup org-capture-mode-map key)
                #'cj/--org-capture-blocked-key))))

(ert-deftest test-org-capture-fkey-guard-blocker-signals-user-error ()
  "Error: the blocker signals a user-error rather than doing nothing."
  (should-error (cj/--org-capture-blocked-key) :type 'user-error))

(ert-deftest test-org-capture-fkey-guard-shadows-global ()
  "Normal: with org-capture-mode active, the minor-mode binding wins."
  (with-temp-buffer
    (org-capture-mode 1)
    (unwind-protect
        (should (eq (key-binding (kbd "<f12>"))
                    #'cj/--org-capture-blocked-key))
      (org-capture-mode -1))))

(provide 'test-org-capture-config-fkey-guard)
;;; test-org-capture-config-fkey-guard.el ends here
