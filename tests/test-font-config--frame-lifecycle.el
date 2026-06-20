;;; test-font-config--frame-lifecycle.el --- Tests for the lifted font frame helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/apply-font-settings-to-frame, cj/cleanup-frame-list, and
;; cj/maybe-install-all-the-icons-fonts were defined inside use-package
;; :config / with-eval-after-load (unreachable under `make test').  Lifting
;; them to top level makes their branching unit-testable; env-gui-p and the
;; package side-effect calls are mocked at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'font-config)

(defvar cj/fontaine-configured-frames)

(ert-deftest test-font-cleanup-frame-list-removes-frame ()
  "Normal: cleanup drops the given frame from the configured list."
  (let ((cj/fontaine-configured-frames '(fr1 fr2 fr3)))
    (cj/cleanup-frame-list 'fr2)
    (should (equal cj/fontaine-configured-frames '(fr1 fr3)))))

(ert-deftest test-font-apply-gui-unconfigured-sets-preset ()
  "Normal: a GUI frame not yet configured gets the preset and is tracked."
  (let ((cj/fontaine-configured-frames nil)
        (called nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () t))
              ((symbol-function 'fontaine-set-preset) (lambda (_p) (setq called t))))
      (cj/apply-font-settings-to-frame (selected-frame)))
    (should called)
    (should (member (selected-frame) cj/fontaine-configured-frames))))

(ert-deftest test-font-apply-already-configured-is-noop ()
  "Boundary: an already-configured frame is not re-preset."
  (let ((cj/fontaine-configured-frames (list (selected-frame)))
        (called nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () t))
              ((symbol-function 'fontaine-set-preset) (lambda (_p) (setq called t))))
      (cj/apply-font-settings-to-frame (selected-frame)))
    (should-not called)))

(ert-deftest test-font-apply-non-gui-is-noop ()
  "Boundary: without a GUI nothing is applied or tracked."
  (let ((cj/fontaine-configured-frames nil)
        (called nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () nil))
              ((symbol-function 'fontaine-set-preset) (lambda (_p) (setq called t))))
      (cj/apply-font-settings-to-frame (selected-frame)))
    (should-not called)
    (should-not (member (selected-frame) cj/fontaine-configured-frames))))

(ert-deftest test-font-maybe-install-icons-gui-missing-installs ()
  "Normal: GUI present and font missing triggers the install."
  (let ((installed nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () t))
              ((symbol-function 'cj/font-installed-p) (lambda (_n) nil))
              ((symbol-function 'all-the-icons-install-fonts) (lambda (&rest _) (setq installed t)))
              ((symbol-function 'remove-hook) #'ignore))
      (cj/maybe-install-all-the-icons-fonts))
    (should installed)))

(ert-deftest test-font-maybe-install-icons-already-present-skips ()
  "Boundary: an installed font means no install attempt."
  (let ((installed nil))
    (cl-letf (((symbol-function 'env-gui-p) (lambda () t))
              ((symbol-function 'cj/font-installed-p) (lambda (_n) t))
              ((symbol-function 'all-the-icons-install-fonts) (lambda (&rest _) (setq installed t))))
      (cj/maybe-install-all-the-icons-fonts))
    (should-not installed)))

(provide 'test-font-config--frame-lifecycle)
;;; test-font-config--frame-lifecycle.el ends here
