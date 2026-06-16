;;; test-system-utils-scratch-background.el --- Tests for the scratch tint -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--scratch-lightened-background lightens the default background by a
;; tunable percent for the *scratch* buffer's buffer-local face remap.  The
;; colour arithmetic (color-lighten-name -> color-name-to-rgb) is
;; display-dependent and returns zeros under --batch, so the actual lightening
;; is verified live in the daemon; here we cover the display-independent
;; contract: a usable colour string yields a string, junk yields nil.

;;; Code:

(require 'ert)
(require 'color)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-utils)

(ert-deftest test-system-utils-scratch-lightened-background-returns-string ()
  "Normal: a valid hex colour yields a colour string (not nil)."
  (let ((cj/scratch-background-lighten 5))
    (should (stringp (cj/--scratch-lightened-background "#100f0f")))))

(ert-deftest test-system-utils-scratch-lightened-background-bad-input ()
  "Error: non-colour input yields nil rather than signalling."
  (should (null (cj/--scratch-lightened-background nil)))
  (should (null (cj/--scratch-lightened-background 'unspecified)))
  (should (null (cj/--scratch-lightened-background "not-a-color"))))

(provide 'test-system-utils-scratch-background)
;;; test-system-utils-scratch-background.el ends here
