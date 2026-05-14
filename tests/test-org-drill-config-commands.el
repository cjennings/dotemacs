;;; test-org-drill-config-commands.el --- Tests for org-drill edit / capture / refile -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-org-drill-config.el' covers the file/dir picker and
;; `cj/drill-start' / `cj/drill-this-file'.  This file covers the
;; three remaining wrapper commands:
;;
;;   cj/drill-edit
;;   cj/drill-capture
;;   cj/drill-refile

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)
(package-initialize)
(require 'org)
(require 'org-capture)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub custom keymap for org-drill-config tests.")
(defvar drill-dir (file-name-as-directory
                   (expand-file-name "cj-drill-default" temporary-file-directory))
  "Stub `drill-dir' for org-drill-config tests.")
;; `org-refile-targets' is defined by org-refile; declare at top level so
;; the module's `setq' on it reaches a dynamic binding under lexical scope.
(defvar org-refile-targets nil
  "Dynamic stand-in for `org-refile-targets'.")

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-drill-config)

;;; cj/drill-edit

(ert-deftest test-org-drill-edit-opens-the-picked-file ()
  "Normal: drill-edit picks a file and visits it (no `org-drill' call)."
  (let (opened (drilled 0))
    (cl-letf (((symbol-function 'cj/--drill-pick-file)
               (lambda (_dir) "/decks/german.org"))
              ((symbol-function 'find-file) (lambda (f) (setq opened f)))
              ((symbol-function 'org-drill)
               (lambda (&rest _) (cl-incf drilled))))
      (cj/drill-edit))
    (should (equal "/decks/german.org" opened))
    (should (= 0 drilled))))

(ert-deftest test-org-drill-edit-prefix-uses-prompted-dir ()
  "Normal: a prefix arg routes the file pick through the prompted directory."
  (let ((tmp (file-name-as-directory (make-temp-file "cj-drill-edit-" t)))
        opened)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "latin.org" tmp))
          (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) tmp))
                    ((symbol-function 'completing-read) (lambda (&rest _) "latin.org"))
                    ((symbol-function 'find-file) (lambda (f) (setq opened f))))
            (cj/drill-edit t))
          (should (equal (expand-file-name "latin.org" tmp) opened)))
      (delete-directory tmp t))))

;;; cj/drill-capture

(ert-deftest test-org-drill-capture-routes-through-d-template ()
  "Normal: drill-capture calls `org-capture' with the \"d\" template key."
  (let (capture-key)
    (cl-letf (((symbol-function 'org-capture)
               (lambda (_arg key) (setq capture-key key))))
      (cj/drill-capture))
    (should (equal "d" capture-key))))

;;; cj/drill-refile

(ert-deftest test-org-drill-refile-sets-targets-and-delegates ()
  "Normal: drill-refile narrows `org-refile-targets' to current buffer +
`drill-dir', then dispatches to `org-refile' via `call-interactively'."
  (let (seen-targets called-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn)
                 (setq called-fn fn
                       seen-targets org-refile-targets))))
      (cj/drill-refile))
    (should (eq called-fn 'org-refile))
    (should seen-targets)
    ;; Two entries: (nil :maxlevel . 1) and (drill-dir :maxlevel . 1).
    (should (= 2 (length seen-targets)))
    (should (assoc nil seen-targets))
    (should (assoc 'drill-dir seen-targets))))

(provide 'test-org-drill-config-commands)
;;; test-org-drill-config-commands.el ends here
