;;; test-font-config.el --- Smoke tests for font-config -*- lexical-binding: t; -*-

;;; Commentary:

;; font-config.el is mostly top-level font/package setup.  These smoke tests
;; cover the logic that should stay correct regardless of which fonts are
;; installed: the install check, and the daemon-frame font applier (env-gui-p
;; guard plus idempotency).  The module :demand's fontaine and references
;; nerd-icons, so the tests skip when those packages are absent rather than
;; failing on a bare checkout.  GUI and font lookups are stubbed so the run
;; stays headless.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; font-config :demand's external packages, so make the installed packages
;; reachable; the test runners do not package-initialize themselves.
(package-initialize)

(defconst test-font-config--available
  (and (locate-library "fontaine")
       (locate-library "nerd-icons"))
  "Non-nil when the packages font-config needs are loadable.")

;;; cj/font-installed-p

(ert-deftest test-font-config-font-installed-p-true-when-found ()
  "Normal: the install check returns t when the font is found."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'find-font) (lambda (&rest _) t)))
    (should (eq t (cj/font-installed-p "BerkeleyMono Nerd Font")))))

(ert-deftest test-font-config-font-installed-p-nil-when-absent ()
  "Error: the install check returns nil when the font is missing."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'find-font) (lambda (&rest _) nil)))
    (should (null (cj/font-installed-p "No Such Font 12345")))))

;;; cj/apply-font-settings-to-frame

(ert-deftest test-font-config-apply-font-settings-noop-without-gui ()
  "Boundary: on a non-GUI frame the applier does nothing and does not error."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((cj/fontaine-configured-frames nil)
        (applied nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) nil))
              ((symbol-function 'fontaine-set-preset)
               (lambda (&rest _) (setq applied t))))
      (cj/apply-font-settings-to-frame (selected-frame))
      (should-not applied)
      (should-not cj/fontaine-configured-frames))))

(ert-deftest test-font-config-apply-font-settings-applies-once-per-frame ()
  "Normal: on a GUI frame the applier sets the preset once and is idempotent."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((cj/fontaine-configured-frames nil)
        (calls 0))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) t))
              ((symbol-function 'fontaine-set-preset)
               (lambda (&rest _) (setq calls (1+ calls)))))
      (cj/apply-font-settings-to-frame (selected-frame))
      (cj/apply-font-settings-to-frame (selected-frame))
      (should (= calls 1))
      (should (memq (selected-frame) cj/fontaine-configured-frames)))))

;;; cj/setup-emoji-fontset

(ert-deftest test-font-config-setup-emoji-fontset-noop-without-gui ()
  "Boundary: without a GUI the emoji setup does nothing and does not error."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((called nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) nil))
              ((symbol-function 'set-fontset-font)
               (lambda (&rest _) (setq called t))))
      (cj/setup-emoji-fontset)
      (should-not called))))

(ert-deftest test-font-config-setup-emoji-fontset-runs-on-gui ()
  "Normal: on a GUI frame the emoji setup runs without error."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) t))
            ((symbol-function 'font-family-list)
             (lambda (&rest _) '("Noto Color Emoji")))
            ((symbol-function 'set-fontset-font) (lambda (&rest _) t)))
    (should (progn (cj/setup-emoji-fontset) t))))

;;; cj/set-emojify-display-style

(defvar emojify-display-style)

(ert-deftest test-font-config-emojify-display-style-image-on-gui ()
  "Normal: on a GUI frame the emoji display style is `image'."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((emojify-display-style nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) t)))
      (cj/set-emojify-display-style)
      (should (eq emojify-display-style 'image)))))

(ert-deftest test-font-config-emojify-display-style-unicode-without-gui ()
  "Boundary: without a GUI the emoji display style is `unicode'."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (let ((emojify-display-style nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda (&rest _) nil)))
      (cj/set-emojify-display-style)
      (should (eq emojify-display-style 'unicode)))))

;;; cj/display-available-fonts

(ert-deftest test-font-config-display-available-fonts-second-call-no-error ()
  "Error: a second invocation does not signal on the read-only buffer."
  (skip-unless test-font-config--available)
  (require 'font-config)
  (cl-letf (((symbol-function 'font-family-list)
             (lambda (&rest _) '("Fixture Font A" "Fixture Font B"))))
    (unwind-protect
        (progn
          (cj/display-available-fonts)
          ;; The first call ends in `special-mode' (read-only); the second must
          ;; not signal when it erases and rewrites the buffer.
          (cj/display-available-fonts)
          (with-current-buffer "*Available Fonts*"
            (should (> (buffer-size) 0))
            (should buffer-read-only)))
      (when (get-buffer "*Available Fonts*")
        (kill-buffer "*Available Fonts*")))))

(provide 'test-font-config)
;;; test-font-config.el ends here
