;;; test-prog-lsp.el --- Startup smoke test for LSP config resolution -*- lexical-binding: t; -*-

;;; Commentary:
;; A narrow smoke test of prog-lsp.el, the central LSP module.  It pins the
;; invariants that should hold the moment the config loads, before any server
;; starts: lsp-enable-remote stays nil (so TRAMP files don't auto-start a slow
;; LSP), the file-watch-ignore defaults live in one idempotent place, the eldoc
;; provider is stripped from the global hook, and a mode never accrues a
;; duplicate lsp-deferred entry.  The generic :config defaults are deferred to
;; lsp-mode's own load (see the make-test no-package-initialize note in
;; CLAUDE.md), so this tests the top-level :init and helper surface, which runs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'use-package)
(require 'prog-lsp)

;; lsp-mode's defcustom isn't loaded under make test, and prog-lsp's bare
;; `(defvar lsp-file-watch-ignored-directories)' only marks it special within
;; that file's unit.  Declare it special here too so the `let' bindings below
;; bind dynamically (the helper reads it through the symbol via add-to-list).
(defvar lsp-file-watch-ignored-directories nil)

(ert-deftest test-prog-lsp-enable-remote-nil ()
  "Normal: lsp-enable-remote is nil so LSP never auto-starts on TRAMP files."
  (should (boundp 'lsp-enable-remote))
  (should (null lsp-enable-remote)))

(ert-deftest test-prog-lsp-file-watch-adds-extras ()
  "Normal: the build/cache ignore patterns get appended to lsp's watch-ignore list."
  (let ((lsp-file-watch-ignored-directories '("[/\\\\]\\.git\\'")))
    (cj/lsp--add-file-watch-ignored-extras)
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (member pattern lsp-file-watch-ignored-directories)))
    (should (member "[/\\\\]\\.git\\'" lsp-file-watch-ignored-directories))))

(ert-deftest test-prog-lsp-file-watch-idempotent ()
  "Boundary: adding the extras twice leaves each pattern present exactly once."
  (let ((lsp-file-watch-ignored-directories '()))
    (cj/lsp--add-file-watch-ignored-extras)
    (cj/lsp--add-file-watch-ignored-extras)
    (dolist (pattern cj/lsp-file-watch-ignored-extras)
      (should (= 1 (cl-count pattern lsp-file-watch-ignored-directories
                             :test #'equal))))))

(ert-deftest test-prog-lsp-eldoc-provider-removed-globally ()
  "Normal: the global eldoc provider is stripped so lsp can't reattach it."
  (let ((eldoc-documentation-functions
         (list #'lsp-eldoc-function #'ignore)))
    (cj/lsp--remove-eldoc-provider-global)
    (should-not (memq 'lsp-eldoc-function eldoc-documentation-functions))
    (should (memq 'ignore eldoc-documentation-functions))))

(ert-deftest test-prog-lsp-no-duplicate-mode-hook ()
  "Boundary: a mode prog-lsp wires never holds more than one lsp-deferred entry.
prog-lsp and the per-language modules both add lsp-deferred for some modes;
add-hook dedups identical symbols, and this pins that invariant so a future
non-symbol (lambda) addition that breaks it gets caught."
  (dolist (hook '(c-mode-hook python-mode-hook go-ts-mode-hook))
    (when (boundp hook)
      (should (>= 1 (cl-count 'lsp-deferred (symbol-value hook)))))))

(provide 'test-prog-lsp)
;;; test-prog-lsp.el ends here
