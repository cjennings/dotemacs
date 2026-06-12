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

(ert-deftest test-org-drill-refile-targets-from-validated-helper ()
  "Normal: drill-refile builds its drill targets from the shared
`cj/--drill-files-or-error' helper, expanded against `drill-dir' — not from
a raw `directory-files' call (so it inherits the helper's dot-file exclusion
and validation)."
  (let ((drill-dir "/tmp/cj-drill/")
        seen-targets called-fn)
    (cl-letf (((symbol-function 'cj/--drill-files-or-error)
               (lambda (_dir) '("a.org" "b.org")))
              ;; If the old raw path were still in use it would call
              ;; `directory-files'; a sentinel here keeps it from masquerading.
              ((symbol-function 'directory-files)
               (lambda (&rest _) '("/WRONG/raw.org")))
              ((symbol-function 'call-interactively)
               (lambda (fn)
                 (setq called-fn fn
                       seen-targets org-refile-targets))))
      (cj/drill-refile))
    (should (eq called-fn 'org-refile))
    (should (= 2 (length seen-targets)))
    (should (assoc nil seen-targets))
    (should (equal (car (nth 1 seen-targets))
                   '("/tmp/cj-drill/a.org" "/tmp/cj-drill/b.org")))))

(ert-deftest test-org-drill-refile-does-not-clobber-global-targets ()
  "Error: drill-refile let-binds `org-refile-targets'; the session-wide value
survives the call instead of being permanently replaced."
  (let ((drill-dir "/tmp/cj-drill/")
        (org-refile-targets '((sentinel :maxlevel . 9))))
    (cl-letf (((symbol-function 'cj/--drill-files-or-error) (lambda (_dir) '("a.org")))
              ((symbol-function 'call-interactively) (lambda (_fn) nil)))
      (cj/drill-refile))
    (should (equal org-refile-targets '((sentinel :maxlevel . 9))))))

(ert-deftest test-org-drill-refile-errors-on-missing-drill-dir ()
  "Error: a missing or unreadable drill dir signals a clear `user-error' via
the shared validated helper, instead of a low-level error, and never reaches
`org-refile'."
  (let ((drill-dir (expand-file-name "cj-drill-nonexistent-XYZ/"
                                     temporary-file-directory))
        (called nil))
    (cl-letf (((symbol-function 'call-interactively) (lambda (_fn) (setq called t))))
      (should-error (cj/drill-refile) :type 'user-error))
    (should-not called)))

(provide 'test-org-drill-config-commands)
;;; test-org-drill-config-commands.el ends here
