;;; test-latex-config.el --- Tests for latex-config PDF viewer selection -*- lexical-binding: t; -*-

;;; Commentary:
;; latex-config picks the first available external PDF viewer and pushes it
;; onto `TeX-view-program-selection' for output-pdf.  These tests exercise
;; `cj/--latex-select-pdf-viewer' with `executable-find' mocked, so they don't
;; depend on which viewers happen to be installed on the test machine.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Declare special so the let-bindings below are dynamic, matching the global
;; valued `defvar' AUCTeX's tex.el provides at runtime.  Without it the module's
;; file-local `(defvar TeX-view-program-selection)' does not reach a test's let.
(defvar TeX-view-program-selection nil)

(require 'latex-config)

(defun test-latex--with-available (available thunk)
  "Run THUNK with `executable-find' returning non-nil only for AVAILABLE names."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _)
               (and (member cmd available) (concat "/usr/bin/" cmd)))))
    (funcall thunk)))

(ert-deftest test-latex-config-select-pdf-viewer-picks-available ()
  "Normal: selects the available external viewer for output-pdf."
  (let ((TeX-view-program-selection nil))
    (test-latex--with-available
     '("zathura")
     (lambda () (cj/--latex-select-pdf-viewer)))
    (should (equal (assq 'output-pdf TeX-view-program-selection)
                   '(output-pdf "Zathura")))))

(ert-deftest test-latex-config-select-pdf-viewer-honors-preference-order ()
  "Normal: picks the first candidate in preference order when several exist."
  (let ((TeX-view-program-selection nil))
    ;; both available; evince precedes okular in the candidate list
    (test-latex--with-available
     '("okular" "evince")
     (lambda () (cj/--latex-select-pdf-viewer)))
    (should (equal (assq 'output-pdf TeX-view-program-selection)
                   '(output-pdf "Evince")))))

(ert-deftest test-latex-config-select-pdf-viewer-falls-back-to-pdf-tools ()
  "Boundary: no external viewer on PATH falls back to PDF Tools."
  (let ((TeX-view-program-selection nil))
    (test-latex--with-available
     '()
     (lambda () (cj/--latex-select-pdf-viewer)))
    (should (equal (assq 'output-pdf TeX-view-program-selection)
                   '(output-pdf "PDF Tools")))))

(ert-deftest test-latex-config-select-pdf-viewer-idempotent ()
  "Boundary: re-running leaves exactly one output-pdf entry."
  (let ((TeX-view-program-selection nil))
    (test-latex--with-available
     '("zathura")
     (lambda ()
       (cj/--latex-select-pdf-viewer)
       (cj/--latex-select-pdf-viewer)
       (cj/--latex-select-pdf-viewer)))
    (should (= 1 (cl-count 'output-pdf TeX-view-program-selection :key #'car)))
    (should (equal (assq 'output-pdf TeX-view-program-selection)
                   '(output-pdf "Zathura")))))

(ert-deftest test-latex-config-select-pdf-viewer-overrides-existing-default ()
  "Boundary: an existing output-pdf default is replaced, not stacked."
  (let ((TeX-view-program-selection '((output-pdf "Evince") (output-dvi "xdvi"))))
    (test-latex--with-available
     '("zathura")
     (lambda () (cj/--latex-select-pdf-viewer)))
    (should (= 1 (cl-count 'output-pdf TeX-view-program-selection :key #'car)))
    (should (equal (assq 'output-pdf TeX-view-program-selection)
                   '(output-pdf "Zathura")))
    ;; an unrelated output type survives the replacement
    (should (equal (assq 'output-dvi TeX-view-program-selection)
                   '(output-dvi "xdvi")))))

(provide 'test-latex-config)
;;; test-latex-config.el ends here
