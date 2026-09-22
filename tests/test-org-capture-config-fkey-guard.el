;;; test-org-capture-config-fkey-guard.el --- capture F-key guard -*- lexical-binding: t; -*-

;;; Commentary:
;; While a capture is in progress, the global popup keys (F1 dashboard
;; sweep, F9 dirvish-side, F10 music, F12 terminal, M-SPC agent swap)
;; must not fire and pop UI over the capture.  F11 is deliberately not on
;; the list: it toggles frame fullscreen, which pops nothing over the
;; capture, so blocking it would only get in the way.  org-capture-mode is a
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
  (dolist (key '("<f1>" "<f9>" "<f10>" "<f12>" "M-SPC"))
    (should (eq (keymap-lookup org-capture-mode-map key)
                #'cj/--org-capture-blocked-key))))

(ert-deftest test-org-capture-fkey-guard-leaves-fullscreen-alone ()
  "Boundary: F11 (frame fullscreen) is not blocked.  It pops no UI, and
before dirvish-side moved to F9 it was on this list only because F11 was the
sidebar key; the entry must follow the command, not the key."
  (should-not (eq (keymap-lookup org-capture-mode-map "<f11>")
                  #'cj/--org-capture-blocked-key)))

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
