;;; org-spec-links.el --- Resolve org-id links into project spec docs -*- lexical-binding: t -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: eager (cheap: defuns + one after-load registration).
;; Eager reason: none beyond the after-load hook; the scan itself runs
;;   only when org-id loads.
;; Top-level side effects: one with-eval-after-load (org-id).
;; Runtime requires: none (org-id is configured after it loads).
;; Direct test load: yes.
;;
;; The docs-lifecycle convention (rulesets, 2026-07-01) gives every
;; formal spec under a project's docs/specs/ an :ID: UUID, and
;; cross-project links use [[id:...]].  org-id-locations only indexes
;; agenda files and files Emacs has visited, so a fresh spec's ID won't
;; resolve on click.  This module enumerates every project's
;; docs/specs/*.org into `org-id-extra-files' (a literal file list --
;; org-id doesn't glob) so org-id's own scans can find them, and
;; provides `cj/org-id-refresh-spec-locations' to re-scan after new
;; specs land and teach `org-id-locations' immediately.
;;
;; Projects are directories carrying .ai/protocols.org under ~/code/,
;; ~/projects/, and ~/.emacs.d -- the same discovery the `ai' launcher
;; and inbox-send use.

;;; Code:

(defun cj/--org-spec-project-base-dirs ()
  "Return the base directories scanned for projects."
  (list (expand-file-name "code/" "~")
        (expand-file-name "projects/" "~")))

(defun cj/--org-spec-project-roots (&optional base-dirs)
  "Return project roots: dirs carrying .ai/protocols.org.
Scans one level under each of BASE-DIRS (default
`cj/--org-spec-project-base-dirs'), plus `user-emacs-directory'."
  (let ((candidates
         (append
          (mapcan (lambda (base)
                    (when (file-directory-p base)
                      (directory-files base t "\\`[^.]" t)))
                  (or base-dirs (cj/--org-spec-project-base-dirs)))
          (unless base-dirs (list user-emacs-directory)))))
    (seq-filter (lambda (dir)
                  (and (file-directory-p dir)
                       (file-exists-p (expand-file-name ".ai/protocols.org" dir))))
                candidates)))

(defun cj/--org-spec-files (&optional base-dirs)
  "Return every project's docs/specs/*.org as absolute file names.
BASE-DIRS overrides the default project discovery (for tests)."
  (mapcan (lambda (root)
            (let ((specs (expand-file-name "docs/specs/" root)))
              (when (file-directory-p specs)
                (directory-files specs t "\\.org\\'" t))))
          (cj/--org-spec-project-roots base-dirs)))

(defun cj/org-id-refresh-spec-locations ()
  "Point `org-id-extra-files' at every project's spec docs and rescan.
Run after new specs land (a spec-sort pass, a new project) so their
:ID:s resolve.  The rescan also walks agenda files, so it can take a
few seconds; the `org-id-extra-files' assignment alone is instant."
  (interactive)
  (let ((files (cj/--org-spec-files)))
    (setq org-id-extra-files files)
    (when (and files (fboundp 'org-id-update-id-locations))
      (org-id-update-id-locations files))
    (message "org-id: %d spec file(s) registered" (length files))))

;; Teach org-id the spec files as soon as it loads: the extra-files list
;; is enough for org-id's own fallback scans; the full refresh command
;; is for immediacy after new specs land.
(with-eval-after-load 'org-id
  (setq org-id-extra-files (cj/--org-spec-files)))

(provide 'org-spec-links)
;;; org-spec-links.el ends here
