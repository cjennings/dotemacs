;;; test-prog-lsp--add-file-watch-ignored-extras.el --- Tests for cj/lsp--add-file-watch-ignored-extras -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/lsp--add-file-watch-ignored-extras in prog-lsp.el.
;; The function adds project-agnostic build/cache directory patterns to
;; `lsp-file-watch-ignored-directories' without replacing lsp-mode's
;; defaults.  Patterns are sourced from `cj/lsp-file-watch-ignored-extras'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Declare lsp-mode's defcustom as a special variable so `let' binds it
;; dynamically.  Real definition is lsp-mode's; loaded only when use-package
;; activates lsp-mode.  In the test environment, this stub provides the value
;; cell `add-to-list' needs.
(defvar lsp-file-watch-ignored-directories nil)

(require 'prog-lsp)

;;; Normal Cases

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-normal-adds-all-patterns ()
  "Normal: every entry from `cj/lsp-file-watch-ignored-extras' lands in the list."
  (let ((lsp-file-watch-ignored-directories nil))
    (cj/lsp--add-file-watch-ignored-extras)
    (should (= (length lsp-file-watch-ignored-directories)
               (length cj/lsp-file-watch-ignored-extras)))
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (member pattern lsp-file-watch-ignored-directories)))))

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-normal-extends-not-replaces ()
  "Normal: pre-existing entries (lsp-mode defaults) are preserved."
  (let ((lsp-file-watch-ignored-directories
         '("[/\\\\]\\.git\\'" "[/\\\\]\\.svn\\'" "[/\\\\]\\.idea\\'")))
    (cj/lsp--add-file-watch-ignored-extras)
    (should (member "[/\\\\]\\.git\\'" lsp-file-watch-ignored-directories))
    (should (member "[/\\\\]\\.svn\\'" lsp-file-watch-ignored-directories))
    (should (member "[/\\\\]\\.idea\\'" lsp-file-watch-ignored-directories))
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (member pattern lsp-file-watch-ignored-directories)))))

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-normal-key-patterns-present ()
  "Normal: specific expected directory names appear in the constant."
  (dolist (name '("node_modules" "target" "__pycache__" ".venv" "venv"
                  "dist" "coverage" "test-results" "playwright-report"
                  ".terraform" ".ruff_cache" ".pytest_cache" ".mypy_cache"))
    (should (cl-some (lambda (p) (string-match-p (regexp-quote name) p))
                     cj/lsp-file-watch-ignored-extras))))

;;; Boundary Cases

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-boundary-idempotent ()
  "Boundary: calling twice doesn't duplicate entries."
  (let ((lsp-file-watch-ignored-directories nil))
    (cj/lsp--add-file-watch-ignored-extras)
    (cj/lsp--add-file-watch-ignored-extras)
    (should (= (length lsp-file-watch-ignored-directories)
               (length cj/lsp-file-watch-ignored-extras)))))

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-boundary-all-patterns-non-empty ()
  "Boundary: every pattern is a non-empty string."
  (dolist (pattern cj/lsp-file-watch-ignored-extras)
    (should (stringp pattern))
    (should (not (string-empty-p pattern)))))

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-boundary-all-patterns-valid-regex ()
  "Boundary: every pattern compiles as a valid Emacs regex."
  (dolist (pattern cj/lsp-file-watch-ignored-extras)
    (condition-case err
        ;; string-match-p compiles the regex; invalid syntax raises invalid-regexp.
        (string-match-p pattern "/some/sample/path")
      (invalid-regexp
       (ert-fail (format "Invalid regex %S: %s"
                         pattern (error-message-string err)))))))

;;; Error Cases

(ert-deftest test-prog-lsp--add-file-watch-ignored-extras-error-non-list-target ()
  "Error: non-list target value triggers `add-to-list' wrong-type-argument."
  (let ((lsp-file-watch-ignored-directories "not-a-list"))
    (should-error (cj/lsp--add-file-watch-ignored-extras)
                  :type 'wrong-type-argument)))

(provide 'test-prog-lsp--add-file-watch-ignored-extras)
;;; test-prog-lsp--add-file-watch-ignored-extras.el ends here
