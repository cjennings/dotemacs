;;; test-org-contacts-config-anniversaries.el --- Tests for org-contacts launch wiring -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers two launch-time concerns that produced the
;; "[org-contacts] ERROR: Your custom variable `org-contacts-files' is
;; nil." message at startup:
;;
;;   1. `org-contacts-files' must be set to the configured contacts file
;;      as soon as the module is required -- not deferred behind the
;;      package load -- so the agenda-finalize anniversaries hook never
;;      sees it nil.
;;   2. `cj/org-contacts-anniversaries-safe' must not call
;;      `org-contacts-anniversaries' when `org-contacts-files' is nil
;;      (the function messages an error rather than signalling, so the
;;      wrapper's `ignore-errors' can't suppress it).
;;
;; `org-agenda' is required first so the `with-eval-after-load' that
;; defines the wrapper fires.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'user-constants)
(require 'org-agenda)
(require 'org-contacts-config)

(ert-deftest test-org-contacts-config-files-configured-on-load ()
  "Normal: requiring the module sets `org-contacts-files' to the contacts file."
  (should (equal org-contacts-files (list contacts-file))))

(ert-deftest test-org-contacts-anniversaries-safe-skips-when-files-nil ()
  "Error: wrapper does not call `org-contacts-anniversaries' when files are nil."
  (let ((called nil)
        (org-contacts-files nil))
    (cl-letf (((symbol-function 'org-contacts-anniversaries)
               (lambda (&rest _) (setq called t))))
      (cj/org-contacts-anniversaries-safe))
    (should-not called)))

(ert-deftest test-org-contacts-anniversaries-safe-runs-when-files-set ()
  "Normal: wrapper calls `org-contacts-anniversaries' when files are configured."
  (let ((called nil)
        (org-contacts-files (list contacts-file)))
    (cl-letf (((symbol-function 'org-contacts-anniversaries)
               (lambda (&rest _) (setq called t))))
      (cj/org-contacts-anniversaries-safe))
    (should called)))

(provide 'test-org-contacts-config-anniversaries)
;;; test-org-contacts-config-anniversaries.el ends here
