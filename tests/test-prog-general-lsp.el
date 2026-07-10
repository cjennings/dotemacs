;;; test-prog-general-lsp.el --- LSP config + helper tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the LSP configuration in prog-general.el, the single owner of
;; generic LSP policy (prog-lsp.el was folded in and removed 2026-07-10).
;;
;; Two layers:
;; - Load-time invariants that hold the moment the config loads, before any
;;   server starts: lsp-enable-remote stays nil (so TRAMP files don't auto-start
;;   a slow LSP), and no mode accrues a duplicate lsp-deferred entry.
;; - The pure helpers cj/lsp--add-file-watch-ignored-extras and
;;   cj/lsp--remove-eldoc-provider-global, exercised directly with Normal /
;;   Boundary / Error cases.
;;
;; The quiet-UI :config defaults (snippet off, symbol highlighting off,
;; idle-delay 0.5, ...) are deferred to lsp-mode's own load (see the make-test
;; no-package-initialize note in CLAUDE.md), so they aren't asserted here; the
;; daemon verifies them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'use-package)

;; Declare lsp-mode's defcustom / eldoc's hook as special so `let' binds them
;; dynamically.  Real definitions load only when lsp-mode / eldoc activate; in
;; the test environment these provide the value cells the helpers read through.
(defvar lsp-file-watch-ignored-directories nil)
(defvar eldoc-documentation-functions nil)

(defun lsp-eldoc-function (&rest _args)
  "Stub lsp-mode Eldoc function for tests.")

(require 'prog-general)

;;; Load-time invariants

(ert-deftest test-prog-general-lsp-enable-remote-nil ()
  "Normal: lsp-enable-remote is nil so LSP never auto-starts on TRAMP files."
  (should (boundp 'lsp-enable-remote))
  (should (null lsp-enable-remote)))

(ert-deftest test-prog-general-lsp-no-duplicate-mode-hook ()
  "Boundary: a mode never holds more than one lsp-deferred entry.
The per-language modules add lsp-deferred to their own mode hooks; add-hook
dedups identical symbols, and this pins that invariant so a future non-symbol
\(lambda) addition that breaks it gets caught."
  (dolist (hook '(c-mode-hook python-mode-hook go-ts-mode-hook))
    (when (boundp hook)
      (should (>= 1 (cl-count 'lsp-deferred (symbol-value hook)))))))

;;; File-watch ignore helper — Normal

(ert-deftest test-prog-general-lsp-file-watch-adds-all-patterns ()
  "Normal: every entry from `cj/lsp-file-watch-ignored-extras' lands in the list."
  (let ((lsp-file-watch-ignored-directories nil))
    (cj/lsp--add-file-watch-ignored-extras)
    (should (= (length lsp-file-watch-ignored-directories)
               (length cj/lsp-file-watch-ignored-extras)))
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (member pattern lsp-file-watch-ignored-directories)))))

(ert-deftest test-prog-general-lsp-file-watch-extends-not-replaces ()
  "Normal: pre-existing entries (lsp-mode defaults) are preserved."
  (let ((lsp-file-watch-ignored-directories
         '("[/\\\\]\\.git\\'" "[/\\\\]\\.svn\\'" "[/\\\\]\\.idea\\'")))
    (cj/lsp--add-file-watch-ignored-extras)
    (should (member "[/\\\\]\\.git\\'" lsp-file-watch-ignored-directories))
    (should (member "[/\\\\]\\.svn\\'" lsp-file-watch-ignored-directories))
    (should (member "[/\\\\]\\.idea\\'" lsp-file-watch-ignored-directories))
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (member pattern lsp-file-watch-ignored-directories)))))

(ert-deftest test-prog-general-lsp-file-watch-key-patterns-present ()
  "Normal: specific expected directory names appear in the constant."
  (dolist (name '("node_modules" "target" "__pycache__" ".venv" "venv"
                  "dist" "coverage" "test-results" "playwright-report"
                  ".terraform" ".ruff_cache" ".pytest_cache" ".mypy_cache"))
    (should (cl-some (lambda (p) (string-match-p (regexp-quote name) p))
                     cj/lsp-file-watch-ignored-extras))))

;;; File-watch ignore helper — Boundary

(ert-deftest test-prog-general-lsp-file-watch-idempotent ()
  "Boundary: calling twice leaves each pattern present exactly once."
  (let ((lsp-file-watch-ignored-directories nil))
    (cj/lsp--add-file-watch-ignored-extras)
    (cj/lsp--add-file-watch-ignored-extras)
    (should (= (length lsp-file-watch-ignored-directories)
               (length cj/lsp-file-watch-ignored-extras)))
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (= 1 (cl-count pattern lsp-file-watch-ignored-directories
                             :test #'equal))))))

(ert-deftest test-prog-general-lsp-file-watch-all-patterns-non-empty ()
  "Boundary: every pattern is a non-empty string."
  (dolist (pattern cj/lsp-file-watch-ignored-extras)
    (should (stringp pattern))
    (should (not (string-empty-p pattern)))))

(ert-deftest test-prog-general-lsp-file-watch-all-patterns-valid-regex ()
  "Boundary: every pattern compiles as a valid Emacs regex."
  (dolist (pattern cj/lsp-file-watch-ignored-extras)
    (condition-case err
        ;; string-match-p compiles the regex; invalid syntax raises invalid-regexp.
        (string-match-p pattern "/some/sample/path")
      (invalid-regexp
       (ert-fail (format "Invalid regex %S: %s"
                         pattern (error-message-string err)))))))

;;; File-watch ignore helper — Error

(ert-deftest test-prog-general-lsp-file-watch-non-list-target ()
  "Error: non-list target value triggers `add-to-list' wrong-type-argument."
  (let ((lsp-file-watch-ignored-directories "not-a-list"))
    (should-error (cj/lsp--add-file-watch-ignored-extras)
                  :type 'wrong-type-argument)))

;;; Eldoc provider helper

(ert-deftest test-prog-general-lsp-eldoc-provider-removed-globally ()
  "Normal: remove lsp-mode's Eldoc provider from the global hook value.
The per-buffer removal this replaced raced lsp-mode's own buffer-local hook
population; removing globally before any LSP buffer attaches makes the absence
stick for every subsequent lsp-managed buffer."
  (let ((eldoc-documentation-functions
         (list #'lsp-eldoc-function 'eldoc-documentation-default)))
    (cj/lsp--remove-eldoc-provider-global)
    (should-not (memq #'lsp-eldoc-function eldoc-documentation-functions))
    (should (memq 'eldoc-documentation-default eldoc-documentation-functions))))

(ert-deftest test-prog-general-lsp-eldoc-provider-removal-idempotent ()
  "Boundary: re-running the removal after the provider is gone is a no-op."
  (let ((eldoc-documentation-functions '(eldoc-documentation-default)))
    (cj/lsp--remove-eldoc-provider-global)
    (cj/lsp--remove-eldoc-provider-global)
    (should (equal eldoc-documentation-functions
                   '(eldoc-documentation-default)))))

(ert-deftest test-prog-general-lsp-no-obsolete-eldoc-hook-reference ()
  "Regression: prog-general should not reference obsolete `lsp-eldoc-hook'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/prog-general.el" user-emacs-directory))
    (should-not (re-search-forward "\\_<lsp-eldoc-hook\\_>" nil t))))

(provide 'test-prog-general-lsp)
;;; test-prog-general-lsp.el ends here
