;;; test-org-refile-config-commands.el --- Tests for org-refile-config command wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the ensure-org-mode helper and the build-targets
;; pipeline.  This file covers the wrapper commands:
;;
;;   cj/org-refile-refresh-targets
;;   cj/org-refile
;;   cj/org-refile-in-file

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-refile-config)

;; `org-refile-targets' is normally provided by org-refile; the module's
;; `cj/org-refile-in-file' let-binds it.  Declare it at top level so the
;; let-binding is dynamic under lexical scope.
(defvar org-refile-targets nil
  "Dynamic stand-in for `org-refile-targets'.")

;;; cj/org-refile-refresh-targets

(ert-deftest test-org-refile-refresh-targets-forces-rebuild ()
  "Normal: refresh-targets calls build-targets with the force-rebuild flag."
  (let ((forced nil))
    (cl-letf (((symbol-function 'cj/build-org-refile-targets)
               (lambda (&optional flag) (setq forced flag))))
      (cj/org-refile-refresh-targets))
    (should (eq forced 'force-rebuild))))

;;; cj/org-refile

(ert-deftest test-org-refile-builds-targets-then-delegates ()
  "Normal: cj/org-refile builds the cache then calls `org-refile' with the args."
  (let ((built nil)
        (call-args nil))
    (cl-letf (((symbol-function 'cj/build-org-refile-targets)
               (lambda (&optional _) (setq built t)))
              ((symbol-function 'org-refile)
               (lambda (&rest args) (setq call-args args))))
      (cj/org-refile 'arg 'buffer 'rfloc 'msg))
    (should built)
    (should (equal call-args '(arg buffer rfloc msg)))))

;;; cj/org-refile-in-file

(ert-deftest test-org-refile-in-file-uses-current-file-as-target ()
  "Normal: refile-in-file scopes `org-refile-targets' to the current file."
  (let ((seen-targets nil))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/notes.org")
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (_fn &rest _)
                   (setq seen-targets org-refile-targets)))
                ((symbol-function 'save-buffer) #'ignore))
        (cj/org-refile-in-file))
      (setq buffer-file-name nil))
    ;; The let-bound targets are: (((file) :maxlevel . 6))
    (should seen-targets)
    (let* ((entry (car seen-targets))
           (files (car entry)))
      (should (member "/tmp/notes.org" files))
      (should (equal (cdr entry) '(:maxlevel . 6))))))

(ert-deftest test-org-refile-in-file-saves-after-refile ()
  "Normal: refile-in-file calls `save-buffer' after the refile."
  (let ((saved nil))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/notes.org")
      (cl-letf (((symbol-function 'call-interactively) #'ignore)
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (setq saved t))))
        (cj/org-refile-in-file))
      (setq buffer-file-name nil))
    (should saved)))

(provide 'test-org-refile-config-commands)
;;; test-org-refile-config-commands.el ends here
