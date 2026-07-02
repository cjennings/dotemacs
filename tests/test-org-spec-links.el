;;; test-org-spec-links.el --- docs/specs org-id resolution -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the docs/specs spec-file scanner behind org-id link
;; resolution (org-spec-links.el).  Projects are directories carrying
;; .ai/protocols.org; each contributes its docs/specs/*.org files.  The
;; scanner takes explicit base dirs so the tests run against a sandbox.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'org-spec-links)

;; org-id may never load in batch; declare its var special so the
;; let-binding below is dynamic, not a silent lexical shadow.
(defvar org-id-extra-files)

(defun test-org-spec-links--make-sandbox ()
  "Build a sandbox: two projects, one with specs, one without; one non-project."
  (let ((base (make-temp-file "spec-links-" t)))
    ;; p1: a project with two spec files
    (make-directory (expand-file-name "p1/.ai" base) t)
    (write-region "" nil (expand-file-name "p1/.ai/protocols.org" base))
    (make-directory (expand-file-name "p1/docs/specs" base) t)
    (write-region "" nil (expand-file-name "p1/docs/specs/a-spec.org" base))
    (write-region "" nil (expand-file-name "p1/docs/specs/b-spec.org" base))
    ;; p2: a project with no docs/specs
    (make-directory (expand-file-name "p2/.ai" base) t)
    (write-region "" nil (expand-file-name "p2/.ai/protocols.org" base))
    ;; p3: not a project (no protocols.org) but has docs/specs
    (make-directory (expand-file-name "p3/docs/specs" base) t)
    (write-region "" nil (expand-file-name "p3/docs/specs/c-spec.org" base))
    base))

(ert-deftest test-org-spec-links-scanner-finds-project-specs ()
  "Normal: spec files from protocols-carrying projects only, absolute paths."
  (let ((base (test-org-spec-links--make-sandbox)))
    (unwind-protect
        (let ((files (cj/--org-spec-files (list base))))
          (should (= (length files) 2))
          (should (cl-every #'file-name-absolute-p files))
          (should (seq-find (lambda (f) (string-suffix-p "p1/docs/specs/a-spec.org" f)) files))
          (should (seq-find (lambda (f) (string-suffix-p "p1/docs/specs/b-spec.org" f)) files))
          (should-not (seq-find (lambda (f) (string-match-p "p3" f)) files)))
      (delete-directory base t))))

(ert-deftest test-org-spec-links-scanner-empty-base ()
  "Boundary: a base dir with no projects yields nil."
  (let ((base (make-temp-file "spec-links-empty-" t)))
    (unwind-protect
        (should-not (cj/--org-spec-files (list base)))
      (delete-directory base t))))

(ert-deftest test-org-spec-links-scanner-missing-base ()
  "Error: a non-existent base dir is skipped without signaling."
  (should-not (cj/--org-spec-files '("/nonexistent/base/dir"))))

(ert-deftest test-org-spec-links-refresh-sets-extra-files ()
  "Normal: the refresh command points org-id-extra-files at the scan result."
  (let* ((base (test-org-spec-links--make-sandbox))
         (org-id-extra-files nil)
         ;; the default scan appends user-emacs-directory; point it into
         ;; the sandbox so the real config's specs don't leak in
         (user-emacs-directory (expand-file-name "p2/" base)))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--org-spec-project-base-dirs)
                   (lambda () (list base)))
                  ;; org-id may be absent in batch; the update step is mocked.
                  ((symbol-function 'org-id-update-id-locations)
                   (lambda (&rest _) nil)))
          (cj/org-id-refresh-spec-locations)
          (should (= (length org-id-extra-files) 2)))
      (delete-directory base t))))

(provide 'test-org-spec-links)
;;; test-org-spec-links.el ends here
