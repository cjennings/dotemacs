;;; test-dashboard-config-recentf-exclude.el --- recentf-exclude is not clobbered -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--dashboard-exclude-emms-from-recentf' adds the EMMS history pattern
;; to `recentf-exclude'.  It must ADD to the list, not replace it, or it
;; wipes the exclusions system-defaults.el set earlier in init order
;; (emacs_bookmarks, elpa, recentf, ElfeedDB, airootfs).

;;; Code:

(require 'ert)
(require 'recentf)   ; makes `recentf-exclude' special so the let below is dynamic

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-general)
(require 'dashboard-config)

(ert-deftest test-dashboard-config-exclude-emms-preserves-existing-entries ()
  "Error: excluding the EMMS history preserves prior recentf-exclude entries."
  (let ((recentf-exclude (list "emacs_bookmarks" "airootfs")))
    (cj/--dashboard-exclude-emms-from-recentf)
    (should (member "/emms/history" recentf-exclude))
    (should (member "emacs_bookmarks" recentf-exclude))
    (should (member "airootfs" recentf-exclude))))

(ert-deftest test-dashboard-config-exclude-emms-adds-the-pattern ()
  "Normal: the EMMS history pattern is present after the call."
  (let ((recentf-exclude nil))
    (cj/--dashboard-exclude-emms-from-recentf)
    (should (member "/emms/history" recentf-exclude))))

(provide 'test-dashboard-config-recentf-exclude)
;;; test-dashboard-config-recentf-exclude.el ends here
