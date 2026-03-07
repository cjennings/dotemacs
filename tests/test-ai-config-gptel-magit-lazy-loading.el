;;; test-ai-config-gptel-magit-lazy-loading.el --- Tests for gptel-magit lazy loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the lazy gptel-magit integration in ai-config.el.
;;
;; ai-config.el uses `with-eval-after-load 'magit' to register autoloads
;; and keybindings for gptel-magit functions.  This avoids loading gptel
;; on every magit buffer (the old hook approach) and defers it until the
;; user actually presses a gptel-magit key.
;;
;; Testing approach:
;; - Load ai-config with gptel stubs (via testutil-ai-config) but WITHOUT
;;   providing magit, so the eval-after-load block does not fire yet.
;; - Provide a minimal magit stub (just the keymap and transient helpers),
;;   which triggers the eval-after-load and wires up the bindings.
;; - Verify that gptel-magit functions are registered as autoloads (not
;;   fully loaded), confirming deferred loading works.
;; - Verify the M-g keymap binding in git-commit-mode-map.
;;
;; What is NOT tested here (requires real magit transients):
;; - transient-append-suffix actually adds "g" to magit-commit
;; - transient-append-suffix actually adds "x" to magit-diff
;; These are best verified manually in a running Emacs.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load gptel stubs — this does NOT provide 'magit, so the
;; with-eval-after-load 'magit block in ai-config stays dormant.
(require 'testutil-ai-config)

;; Stub magit's keymap and transient infrastructure.  These must exist
;; before we trigger the eval-after-load, since ai-config uses them to
;; set up keybindings and transient entries.
(defvar git-commit-mode-map (make-sparse-keymap)
  "Stub keymap standing in for magit's git-commit-mode-map.")

;; Stub transient-append-suffix to be a no-op.  We can't test transient
;; integration without real magit transient definitions, so we just
;; verify it doesn't error.
(defvar test-gptel-magit--transient-calls nil
  "Records calls to the stubbed transient-append-suffix for verification.")

(defun transient-append-suffix (prefix loc suffix)
  "Stub: record the call for test inspection instead of modifying transients.
PREFIX is the transient being modified, LOC is the reference suffix,
SUFFIX is the entry being appended."
  (push (list prefix loc suffix) test-gptel-magit--transient-calls))

;; Now load ai-config.  The with-eval-after-load 'magit block is
;; registered but NOT executed yet.
(require 'ai-config)

;; ----------------------------- Setup / Teardown ------------------------------

(defun test-gptel-magit-setup ()
  "Trigger the magit eval-after-load by providing the feature.
This simulates what happens when a user first opens magit."
  (setq test-gptel-magit--transient-calls nil)
  (provide 'magit))

;; NOTE: We cannot un-provide 'magit between tests, and eval-after-load
;; callbacks only fire once, so all tests share the same post-trigger
;; state.  The setup runs once before the first test.

(test-gptel-magit-setup)

;;; Normal Cases — Autoloads

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-generate-message-is-autoload ()
  "After magit loads, gptel-magit-generate-message should be an autoload.
An autoload means the function is registered but gptel-magit.el has not
been loaded yet — it will only load when the function is first called."
  (should (fboundp 'gptel-magit-generate-message))
  (should (autoloadp (symbol-function 'gptel-magit-generate-message))))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-commit-generate-is-autoload ()
  "After magit loads, gptel-magit-commit-generate should be an autoload."
  (should (fboundp 'gptel-magit-commit-generate))
  (should (autoloadp (symbol-function 'gptel-magit-commit-generate))))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-diff-explain-is-autoload ()
  "After magit loads, gptel-magit-diff-explain should be an autoload."
  (should (fboundp 'gptel-magit-diff-explain))
  (should (autoloadp (symbol-function 'gptel-magit-diff-explain))))

;;; Normal Cases — Keymap Binding

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-keymap-binding ()
  "M-g in git-commit-mode-map should be bound to gptel-magit-generate-message.
This binding allows generating a commit message from the commit buffer."
  (let ((bound (lookup-key git-commit-mode-map (kbd "M-g"))))
    (should (eq bound 'gptel-magit-generate-message))))

;;; Normal Cases — Transient Registration

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-transient-commit-registered ()
  "transient-append-suffix should have been called for magit-commit.
Verifies that the 'g' / Generate commit entry was registered."
  (should (cl-find 'magit-commit test-gptel-magit--transient-calls
                   :key #'car)))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-transient-diff-registered ()
  "transient-append-suffix should have been called for magit-diff.
Verifies that the 'x' / Explain entry was registered."
  (should (cl-find 'magit-diff test-gptel-magit--transient-calls
                   :key #'car)))

;;; Boundary Cases

(ert-deftest test-ai-config-gptel-magit-lazy-loading-boundary-gptel-magit-not-loaded ()
  "The gptel-magit feature should NOT be loaded after magit triggers.
Only autoloads are registered — the actual package stays unloaded until
a gptel-magit function is called."
  (should-not (featurep 'gptel-magit)))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-boundary-exactly-two-transient-calls ()
  "Exactly two transient-append-suffix calls should have been made.
One for magit-commit (generate) and one for magit-diff (explain)."
  (should (= 2 (length test-gptel-magit--transient-calls))))

(provide 'test-ai-config-gptel-magit-lazy-loading)
;;; test-ai-config-gptel-magit-lazy-loading.el ends here
