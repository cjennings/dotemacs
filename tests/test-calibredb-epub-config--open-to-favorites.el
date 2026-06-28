;;; test-calibredb-epub-config--open-to-favorites.el --- in-progress open filter -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--calibredb-open-to-favorites' advises `calibredb' :after so every launch
;; lands filtered to `calibredb-favorite-keyword' (Craig's "in-progress" books).
;; It scopes the filter to the TAG field (sets `calibredb-tag-filter-p', clears
;; the other filter-p flags) before delegating to `calibredb-search-keyword-filter',
;; so the keyword can't over-match in a book's title or description.  It no-ops
;; when no usable keyword is set.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)

;; calibredb defcustom + internal flags; declared special so `let' binds them
;; dynamically (all unbound under `make test', which never runs the use-package
;; :config or loads calibredb).
(defvar calibredb-favorite-keyword)
(defvar calibredb-tag-filter-p)
(defvar calibredb-favorite-filter-p)
(defvar calibredb-author-filter-p)
(defvar calibredb-date-filter-p)
(defvar calibredb-format-filter-p)

(ert-deftest test-calibredb-open-to-favorites-applies-keyword-scoped-to-tags ()
  "Normal: with a favorite keyword set, the filter runs with that keyword and is
scoped to the tag field (so it can't over-match a description); a stale
non-tag filter flag is cleared."
  (let ((applied :unset)
        (calibredb-favorite-keyword "in-progress")
        (calibredb-tag-filter-p nil)
        (calibredb-favorite-filter-p t)   ; stale, must be cleared
        (calibredb-author-filter-p nil)
        (calibredb-date-filter-p nil)
        (calibredb-format-filter-p nil))
    (cl-letf (((symbol-function 'calibredb-search-keyword-filter)
               (lambda (kw) (setq applied kw))))
      (cj/--calibredb-open-to-favorites))
    (should (equal applied "in-progress"))     ; keyword applied
    (should (eq calibredb-tag-filter-p t))     ; scoped to the tag field
    (should-not calibredb-favorite-filter-p))) ; stale flag cleared

(ert-deftest test-calibredb-open-to-favorites-noop-without-usable-keyword ()
  "Boundary/Error: nil, empty, or non-string keyword applies no filter."
  (dolist (kw (list nil "" 42))
    (let ((applied :unset)
          (calibredb-favorite-keyword kw))
      (cl-letf (((symbol-function 'calibredb-search-keyword-filter)
                 (lambda (k) (setq applied k))))
        (cj/--calibredb-open-to-favorites))
      (should (eq applied :unset)))))

(provide 'test-calibredb-epub-config--open-to-favorites)
;;; test-calibredb-epub-config--open-to-favorites.el ends here
