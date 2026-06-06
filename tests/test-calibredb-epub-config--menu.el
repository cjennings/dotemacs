;;; test-calibredb-epub-config--menu.el --- calibredb curated-menu tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the docked book-description command bound into the curated calibredb
;; menu.  The transient itself, its `?'/`H' keybindings, and the
;; display-buffer-alist dock live in calibredb's deferred `use-package' config
;; (they need the elpa transient, which batch does not load) and are verified
;; live in the daemon; here we cover the describe command, which has no transient
;; dependency.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)

;; calibredb vars (defvar'd here so the tests' `let' bindings are dynamic; the
;; module's bare defvars are file-local to its own compilation unit).
(defvar calibredb-sort-by)
(defvar calibredb-search-filter)
(defvar calibredb-format-filter-p)

(ert-deftest test-calibredb-describe-at-point-shows-entry-without-switch ()
  "Normal: describe calls `calibredb-show-entry' on the entry at point with no
switch argument, so the entry lands in the docked window with focus (q quits)."
  (let (call)
    (cl-letf (((symbol-function 'calibredb-find-candidate-at-point)
               (lambda () '(the-entry extra)))
              ((symbol-function 'calibredb-show-entry)
               (lambda (&rest args) (setq call args))))
      (cj/calibredb-describe-at-point)
      ;; one argument only -- the entry -- and switch is therefore nil
      (should (equal call '(the-entry))))))

(ert-deftest test-calibredb-sort-preserving-filter-keeps-filter ()
  "Normal: the filter-preserving sort sets the field and refreshes via
`calibredb-search-refresh-or-resume' without touching the active filter."
  (let ((calibredb-sort-by 'id)
        (calibredb-search-filter "epub")
        (calibredb-format-filter-p t)
        (refreshed nil))
    (cl-letf (((symbol-function 'calibredb-search-refresh-or-resume)
               (lambda (&rest _) (setq refreshed t))))
      (cj/--calibredb-sort-preserving-filter 'author)
      (should (eq calibredb-sort-by 'author))   ; field updated
      (should refreshed)                          ; refreshed
      (should (equal calibredb-search-filter "epub")) ; filter kept
      (should calibredb-format-filter-p))))           ; filter flag kept

(provide 'test-calibredb-epub-config--menu)
;;; test-calibredb-epub-config--menu.el ends here
