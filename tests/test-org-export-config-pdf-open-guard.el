;;; test-org-export-config-pdf-open-guard.el --- zathura open guard test -*- lexical-binding: t; -*-

;;; Commentary:
;; `my/org-pandoc-export-to-pdf-and-open' opens the exported PDF with zathura
;; via `start-process'.  Without zathura on PATH that call fails with an opaque
;; error.  These tests pin the command-time guard: a missing zathura signals a
;; `user-error' naming the tool, and a present zathura lets the open proceed.
;; Requiring the module then ox fires the deferred ox-pandoc :config in batch,
;; which is where the function is defined.

;;; Code:

;; Initialize package system for batch mode so elpa packages (ox-pandoc) load.
(when noninteractive
  (package-initialize))

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-export-config)
(require 'ox)
;; The function under test lives in ox-pandoc's :config block.  In batch the
;; deferred :after-ox config does not fire on a bare module load, so require
;; ox-pandoc directly to ensure the function is defined.
(require 'ox-pandoc)

(ert-deftest test-org-export-config-pdf-open-missing-zathura-errors ()
  "Error: missing zathura signals a user-error before opening the PDF."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should-error (my/org-pandoc-export-to-pdf-and-open) :type 'user-error)))

(ert-deftest test-org-export-config-pdf-open-present-zathura-proceeds ()
  "Normal: with zathura present, the open step runs without error."
  (let ((started nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &rest _) (if (equal cmd "zathura") "/usr/bin/zathura" nil)))
              ((symbol-function 'org-pandoc-export-to-latex-pdf)
               (lambda (&rest _) "/tmp/out.pdf"))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq started t) 'fake-process)))
      (my/org-pandoc-export-to-pdf-and-open)
      (should started))))

(provide 'test-org-export-config-pdf-open-guard)
;;; test-org-export-config-pdf-open-guard.el ends here
