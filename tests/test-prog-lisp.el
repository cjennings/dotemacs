;;; test-prog-lisp.el --- Smoke tests for prog-lisp owned config -*- lexical-binding: t; -*-

;;; Commentary:

;; prog-lisp.el is mostly use-package config; the behavior it owns directly is
;; the two mode-setup functions and their hook registration.  These tests load
;; the module with use-package stubbed to a no-op, so no packages load, ensure,
;; or download in batch, then assert the setup hooks are registered and apply
;; the buffer-local conventions this config sets.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defconst test-prog-lisp--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root, derived from this file's location under tests/.")

(defun test-prog-lisp--load ()
  "Load prog-lisp.el with use-package stubbed so no packages load or install."
  (cl-letf (((symbol-function 'use-package)
             (cons 'macro (lambda (&rest _) nil))))
    (load (expand-file-name "modules/prog-lisp.el" test-prog-lisp--repo-root)
          nil t)))

;;; cj/elisp-setup

(ert-deftest test-prog-lisp-elisp-setup-registered ()
  "Normal: the elisp setup function is added to emacs-lisp-mode-hook."
  (let ((emacs-lisp-mode-hook nil))
    (test-prog-lisp--load)
    (should (memq 'cj/elisp-setup emacs-lisp-mode-hook))))

(ert-deftest test-prog-lisp-elisp-setup-sets-buffer-locals ()
  "Normal: elisp setup uses 4-space indent, no tabs, fill-column 120."
  (test-prog-lisp--load)
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-fill-column-indicator-mode) #'ignore))
      (cj/elisp-setup)
      (should (= tab-width 4))
      (should (null indent-tabs-mode))
      (should (= fill-column 120)))))

;;; cj/common-lisp-setup

(ert-deftest test-prog-lisp-common-lisp-setup-registered ()
  "Normal: the Common Lisp setup function is added to lisp-mode-hook."
  (let ((lisp-mode-hook nil))
    (test-prog-lisp--load)
    (should (memq 'cj/common-lisp-setup lisp-mode-hook))))

(ert-deftest test-prog-lisp-common-lisp-setup-sets-buffer-locals ()
  "Normal: Common Lisp setup uses 2-space indent, no tabs, fill-column 100."
  (test-prog-lisp--load)
  (with-temp-buffer
    (cj/common-lisp-setup)
    (should (= tab-width 2))
    (should (null indent-tabs-mode))
    (should (= fill-column 100))))

(provide 'test-prog-lisp)
;;; test-prog-lisp.el ends here
