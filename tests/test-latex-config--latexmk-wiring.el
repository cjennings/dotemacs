;;; test-latex-config--latexmk-wiring.el --- latexmk activation guards -*- lexical-binding: t; -*-

;;; Commentary:
;; Guards the two breaks that kept the latexmk workflow from activating:
;;   1. The :hook entry that sets `TeX-command-default' must target the real
;;      `TeX-mode-hook'.  use-package appends "-hook" to any hook symbol not
;;      ending in "-mode", so the mode name `TeX-mode' is required; the literal
;;      `TeX-mode-hook' expands to the nonexistent `TeX-mode-hook-hook'.
;;   2. `auctex-latexmk' must load so `auctex-latexmk-setup' runs.  `:defer t'
;;      with no trigger never fires; `:after tex' loads it when AUCTeX loads.
;;
;; The forms are read from the source and macroexpanded, so the test fails the
;; way the live config failed -- against the actual declaration.

;;; Code:

(require 'ert)
(require 'seq)
(require 'use-package)

(defun test-latex-config--forms ()
  "Return the top-level forms in latex-config.el."
  (let ((file (expand-file-name "modules/latex-config.el" user-emacs-directory))
        (forms '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (nreverse forms)))

(defun test-latex-config--use-package-form (package)
  "Return the (use-package PACKAGE ...) top-level form from latex-config.el."
  (seq-find (lambda (form)
              (and (consp form)
                   (eq (car form) 'use-package)
                   (eq (cadr form) package)))
            (test-latex-config--forms)))

(ert-deftest test-latex-config-tex-hook-targets-real-hook ()
  "Regression: the latexmk-default :hook expands to `TeX-mode-hook', not the
unbound `TeX-mode-hook-hook' use-package builds from a non-mode hook symbol."
  (let* ((form (test-latex-config--use-package-form 'tex))
         (expansion (format "%S" (macroexpand-all form))))
    (should form)
    ;; The hook symbol is followed by whitespace before its lambda, so anchor
    ;; on that to distinguish `TeX-mode-hook' from the broken `...-hook-hook'.
    (should (string-match-p "TeX-mode-hook[ )]" expansion))
    (should-not (string-match-p "TeX-mode-hook-hook" expansion))))

(ert-deftest test-latex-config-auctex-latexmk-loads-after-tex ()
  "Regression: auctex-latexmk uses `:after tex' so `auctex-latexmk-setup' runs;
a bare `:defer t' with no trigger would never load it."
  (let ((form (test-latex-config--use-package-form 'auctex-latexmk)))
    (should form)
    (should (member :after form))
    (should (eq (cadr (member :after form)) 'tex))
    (should-not (member :defer form))))

(provide 'test-latex-config--latexmk-wiring)
;;; test-latex-config--latexmk-wiring.el ends here
