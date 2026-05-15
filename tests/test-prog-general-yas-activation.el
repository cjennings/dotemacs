;;; test-prog-general-yas-activation.el --- Tests for universal yasnippet activation -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the universal-yasnippet wiring in prog-general.el:
;;
;; Wiring:
;; - yas-global-mode is enabled at module-load time (not only in prog-mode).
;; - yas-snippet-dirs points at the repo's snippets/ directory.
;; - yas-minor-mode-hook activates fundamental-mode as an extra mode in
;;   every buffer so the universal snippet table is always consulted.
;;
;; Expansion (the user-visible verification):
;; - The <cj snippet from snippets/fundamental-mode/ expands to the literal
;;   marker block "#+begin_src cj: comment\n\n#+end_src" in any major mode,
;;   verified across fundamental, text, org, emacs-lisp, and python-ts.

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'user-constants)
(require 'prog-general)

(require 'yasnippet)

;;; Wiring tests

(ert-deftest test-prog-general-yas-global-mode-enabled ()
  "Normal: yas-global-mode is on after prog-general loads."
  (should (bound-and-true-p yas-global-mode)))

(ert-deftest test-prog-general-yas-snippet-dirs-set ()
  "Normal: yas-snippet-dirs contains the repo's snippets/ directory."
  (should (cl-some (lambda (d)
                     (string= (file-name-as-directory (expand-file-name d))
                              (file-name-as-directory (expand-file-name snippets-dir))))
                   yas-snippet-dirs)))

(defmacro test-prog-general--with-named-buffer (mode-form &rest body)
  "Run BODY in a fresh, non-temp buffer set to the major mode of MODE-FORM.
Uses a generated name without a leading space so `yas-global-mode' doesn't
skip the buffer the way it skips `with-temp-buffer' buffers (which start
with a space and are excluded by globalized-minor-mode defaults).  The
buffer is killed in `unwind-protect' regardless of BODY outcome."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "yas-activation-test")))
     (unwind-protect
         (with-current-buffer buf
           ,mode-form
           ,@body)
       (kill-buffer buf))))

(ert-deftest test-prog-general-yas-extra-mode-fundamental-in-text-buffer ()
  "Normal: a fresh text-mode buffer with yas-minor-mode on has
fundamental-mode in yas-extra-modes (proves the hook fired)."
  (test-prog-general--with-named-buffer
      (dlet ((text-mode-hook nil)) (text-mode))
    (should (bound-and-true-p yas-minor-mode))
    (should (memq 'fundamental-mode yas-extra-modes))))

(ert-deftest test-prog-general-yas-minor-mode-in-org-buffer ()
  "Boundary: yas-minor-mode is active in an org-mode buffer
(not just in prog-mode-derived buffers)."
  (test-prog-general--with-named-buffer
      (dlet ((org-mode-hook nil)) (org-mode))
    (should (bound-and-true-p yas-minor-mode))))

;;; Expansion tests — the user-visible verification

(defconst test-prog-general--cj-expected
  "#+begin_src cj: comment\n\n#+end_src"
  "Exact expansion of the <cj snippet from snippets/fundamental-mode/.")

(defun test-prog-general--expand-cj-in-mode (mode-fn)
  "Insert <cj into a buffer of MODE-FN, expand via yas, return the buffer string.
Silences mode hooks to keep test load light.  `dlet' is used in place of
`let' because some of these hook vars are declared lexical elsewhere
under `lexical-binding: t', and a plain `let' would error with
\"Defining as dynamic an already lexical var\"."
  (with-temp-buffer
    (dlet ((emacs-lisp-mode-hook nil)
           (org-mode-hook nil)
           (text-mode-hook nil)
           (python-mode-hook nil)
           (python-ts-mode-hook nil)
           (prog-mode-hook nil))
      (funcall mode-fn))
    (unless (bound-and-true-p yas-minor-mode)
      (yas-minor-mode 1))
    (insert "<cj")
    (yas-expand)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-prog-general-cj-expands-in-fundamental-mode ()
  "Normal: <cj + expand in fundamental-mode produces the marker block."
  (should (string= (test-prog-general--expand-cj-in-mode #'fundamental-mode)
                   test-prog-general--cj-expected)))

(ert-deftest test-prog-general-cj-expands-in-text-mode ()
  "Normal: <cj + expand in text-mode produces the marker block."
  (should (string= (test-prog-general--expand-cj-in-mode #'text-mode)
                   test-prog-general--cj-expected)))

(ert-deftest test-prog-general-cj-expands-in-org-mode ()
  "Boundary: <cj + expand in org-mode produces the marker block.
Regression test for replacing the org-tempo entry with the yasnippet."
  (should (string= (test-prog-general--expand-cj-in-mode #'org-mode)
                   test-prog-general--cj-expected)))

(ert-deftest test-prog-general-cj-expands-in-emacs-lisp-mode ()
  "Normal: <cj + expand in emacs-lisp-mode (a prog-mode-derived mode)
produces the marker block."
  (should (string= (test-prog-general--expand-cj-in-mode #'emacs-lisp-mode)
                   test-prog-general--cj-expected)))

(ert-deftest test-prog-general-cj-expands-in-python-ts-mode ()
  "Boundary: <cj + expand in python-ts-mode (a tree-sitter prog-mode-derived
mode) produces the marker block.  Verifies the snippet reaches modern
tree-sitter modes through fundamental-mode inheritance."
  (skip-unless (fboundp 'python-ts-mode))
  (should (string= (test-prog-general--expand-cj-in-mode #'python-ts-mode)
                   test-prog-general--cj-expected)))

(provide 'test-prog-general-yas-activation)
;;; test-prog-general-yas-activation.el ends here
