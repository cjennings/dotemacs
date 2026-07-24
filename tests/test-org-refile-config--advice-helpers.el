;;; test-org-refile-config--advice-helpers.el --- Tests for the refile advice helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the two named advice helpers extracted from anonymous lambdas
;; in the org-refile use-package :config block:
;;
;;   cj/org-refile--save-all-buffers          (:after org-refile)
;;   cj/org-refile--ensure-targets-in-org-mode (:before org-refile-get-targets)
;;
;; They were anonymous `(lambda (&rest _) ...)' advices, which cannot be
;; `advice-remove'd by reference and cannot be tested.  Naming them makes both
;; possible.  The install-by-reference and removability are verified live in the
;; daemon (the :config block doesn't run under batch make test); these tests
;; pin the extracted logic.
;;
;; Test organization:
;; - Normal Cases: ensure-targets visits each string-named target
;; - Boundary Cases: empty targets, non-string cars, mixed list
;; - Error Cases: a nil target list is a no-op
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-refile-config)

;; The module's bare `(defvar org-refile-targets)' marks the symbol special only
;; within its own file, so it isn't special here.  Declare it (with a value) so
;; the `let' bindings below bind it dynamically, the way the sibling
;; test-org-refile-config-commands.el does.
(defvar org-refile-targets nil)

;;; cj/org-refile--ensure-targets-in-org-mode

(ert-deftest test-org-refile-ensure-targets-visits-each-string-target ()
  "Normal: every string-named target file is passed to the ensure helper."
  (let ((org-refile-targets '(("/a.org" :maxlevel . 3)
                              ("/b.org" :maxlevel . 3)))
        (seen '()))
    (cl-letf (((symbol-function 'cj/org-refile-ensure-org-mode)
               (lambda (f) (push f seen))))
      (cj/org-refile--ensure-targets-in-org-mode))
    (should (equal '("/a.org" "/b.org") (nreverse seen)))))

;;; Boundary

(ert-deftest test-org-refile-ensure-targets-skips-non-string-cars ()
  "Boundary: a target whose car is not a string (a function/symbol spec) is
skipped rather than passed to the ensure helper."
  (let ((org-refile-targets `((,(lambda () '("/x.org")) :maxlevel . 2)
                              ("/real.org" :maxlevel . 2)
                              (org-agenda-files :maxlevel . 2)))
        (seen '()))
    (cl-letf (((symbol-function 'cj/org-refile-ensure-org-mode)
               (lambda (f) (push f seen))))
      (cj/org-refile--ensure-targets-in-org-mode))
    (should (equal '("/real.org") seen))))

(ert-deftest test-org-refile-ensure-targets-empty-list-is-noop ()
  "Boundary: no targets means the ensure helper is never called."
  (let ((org-refile-targets '())
        (called nil))
    (cl-letf (((symbol-function 'cj/org-refile-ensure-org-mode)
               (lambda (_f) (setq called t))))
      (cj/org-refile--ensure-targets-in-org-mode))
    (should-not called)))

;;; Error

(ert-deftest test-org-refile-ensure-targets-nil-targets-does-not-signal ()
  "Error: a nil `org-refile-targets' completes without signaling."
  (let ((org-refile-targets nil))
    (cl-letf (((symbol-function 'cj/org-refile-ensure-org-mode) #'ignore))
      (should (progn (cj/org-refile--ensure-targets-in-org-mode) t)))))

;;; cj/org-refile--save-all-buffers

(ert-deftest test-org-refile-save-all-buffers-delegates ()
  "Normal: the save helper calls `org-save-all-org-buffers'."
  (let ((called nil))
    (cl-letf (((symbol-function 'org-save-all-org-buffers)
               (lambda (&rest _) (setq called t))))
      (cj/org-refile--save-all-buffers))
    (should called)))

(provide 'test-org-refile-config--advice-helpers)
;;; test-org-refile-config--advice-helpers.el ends here
