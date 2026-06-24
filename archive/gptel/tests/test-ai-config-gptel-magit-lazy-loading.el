;;; test-ai-config-gptel-magit-lazy-loading.el --- Tests for gptel-magit lazy loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the per-feature lazy gptel-magit integration in ai-config.el.
;;
;; ai-config.el uses three separate `with-eval-after-load' blocks --
;; one per actual dependency -- to wire up its bindings:
;;   git-commit   -> M-g in `git-commit-mode-map'
;;   magit-commit -> "g" suffix in the `magit-commit' transient
;;   magit-diff   -> "x" suffix in the `magit-diff' transient
;;
;; This shape matters: `magit.el' calls `(provide 'magit)' before its
;; `cl-eval-when (load eval) ...' block requires `magit-commit' and
;; `magit-stash', so a single `with-eval-after-load 'magit' would fire
;; while the transient prefixes the wiring references are still
;; undefined.  `transient-append-suffix' silently no-ops on missing
;; prefixes, which is how that bug stayed invisible.
;;
;; Testing approach.  In Emacs 30, `provide' does NOT fire registered
;; `eval-after-load' callbacks in batch mode -- only an actual `load'
;; does.  Rather than work around that with disk-backed stub files, the
;; tests inspect `after-load-alist' directly to verify which features
;; the wiring is gated on.  That's stronger evidence than running the
;; callbacks anyway: the regression we're guarding against is "wiring
;; hooked on `magit'," and the right shape of that check is "no entry
;; for `magit', entries for `git-commit', `magit-commit', `magit-diff'."

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load gptel stubs.  This does NOT provide any of the magit features,
;; so the eval-after-load blocks in ai-config stay dormant.
(require 'testutil-ai-config)

;; Stub the keymap used by the M-g binding.
(defvar git-commit-mode-map (make-sparse-keymap)
  "Stub keymap standing in for magit's git-commit-mode-map.")

;; Stub transient-append-suffix as a recorder.  We don't invoke it
;; through provide in this test file, but the symbol must be fbound so
;; ai-config.el byte-compiles cleanly through `(require 'ai-config)'.
(unless (fboundp 'transient-append-suffix)
  (defun transient-append-suffix (&rest _) nil))

(require 'ai-config)

;; ----------------------------- Regression check ------------------------------

(ert-deftest test-ai-config-gptel-magit-regression-no-after-load-on-magit ()
  "ai-config must NOT register a `with-eval-after-load 'magit' hook.
`magit.el' provides itself BEFORE it loads `magit-commit' and
`magit-stash', so wiring keyed on `magit' would fire while the
transient prefixes are still undefined and `transient-append-suffix'
would silently no-op.  The per-feature hooks side-step the race
entirely -- this test guards against any future regression that
re-introduces a single `'magit' hook."
  ;; Forge installs an after-load entry for 'magit-mode'; magit's own
  ;; code does not register anything keyed on the bare 'magit' symbol.
  ;; Our wiring must not either.
  (let ((entry (assoc 'magit after-load-alist)))
    ;; If something else (e.g. another package) registers under 'magit
    ;; the entry will exist, but it must not contain a closure that
    ;; refers to gptel-magit symbols.  Stringify the entry and grep.
    (when entry
      (should-not (string-match-p "gptel-magit" (format "%s" entry))))))

;; ------------------------------ Wiring registration --------------------------

(ert-deftest test-ai-config-gptel-magit-lazy-loading-git-commit-hook-registered ()
  "ai-config registers an `eval-after-load' hook keyed on `git-commit'.
The hook body binds M-g in `git-commit-mode-map' to
`gptel-magit-generate-message', so the printed closure mentions both."
  (let ((entry (assoc 'git-commit after-load-alist)))
    (should entry)
    (let ((printed (format "%s" entry)))
      (should (string-match-p "git-commit-mode-map" printed))
      (should (string-match-p "gptel-magit-generate-message" printed)))))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-magit-commit-hook-registered ()
  "ai-config registers an `eval-after-load' hook keyed on `magit-commit'.
The hook body calls `transient-append-suffix' for `magit-commit', so
the printed closure mentions both."
  (let ((entry (assoc 'magit-commit after-load-alist)))
    (should entry)
    (let ((printed (format "%s" entry)))
      (should (string-match-p "transient-append-suffix" printed))
      (should (string-match-p "magit-commit" printed))
      (should (string-match-p "gptel-magit-commit-generate" printed)))))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-magit-diff-hook-registered ()
  "ai-config registers an `eval-after-load' hook keyed on `magit-diff'.
The hook body calls `transient-append-suffix' for `magit-diff', so the
printed closure mentions both."
  (let ((entry (assoc 'magit-diff after-load-alist)))
    (should entry)
    (let ((printed (format "%s" entry)))
      (should (string-match-p "transient-append-suffix" printed))
      (should (string-match-p "magit-diff" printed))
      (should (string-match-p "gptel-magit-diff-explain" printed)))))

;;; Normal Cases — Autoloads

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-generate-message-is-autoload ()
  "After ai-config loads, `gptel-magit-generate-message' is an autoload.
An autoload means the function is registered but `gptel-magit.el' has
not been loaded yet -- it loads only when the function is first
called."
  (should (fboundp 'gptel-magit-generate-message))
  (should (autoloadp (symbol-function 'gptel-magit-generate-message))))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-commit-generate-is-autoload ()
  "After ai-config loads, `gptel-magit-commit-generate' is an autoload."
  (should (fboundp 'gptel-magit-commit-generate))
  (should (autoloadp (symbol-function 'gptel-magit-commit-generate))))

(ert-deftest test-ai-config-gptel-magit-lazy-loading-normal-diff-explain-is-autoload ()
  "After ai-config loads, `gptel-magit-diff-explain' is an autoload."
  (should (fboundp 'gptel-magit-diff-explain))
  (should (autoloadp (symbol-function 'gptel-magit-diff-explain))))

;;; Boundary Cases

(ert-deftest test-ai-config-gptel-magit-lazy-loading-boundary-gptel-magit-not-loaded ()
  "After ai-config loads, `gptel-magit' itself stays unloaded.
The autoloads are registered so the package only loads when one of its
entry points is invoked."
  (should-not (featurep 'gptel-magit)))

;;; Error Cases — Install behavior

(ert-deftest test-ai-config-gptel-magit-declared-via-use-package ()
  "ai-config declares gptel-magit via `use-package' so it gets installed.
Raw `(autoload ...)' calls register the function name but leave the
package uninstalled on machines that never ran `package-install'.  The
\\=`use-package' form inherits `use-package-always-ensure' from
early-init, which is how every other package in this config gets
onto `load-path' before its autoloads fire."
  (let ((source-file (expand-file-name "modules/ai-config.el"
                                       user-emacs-directory)))
    (with-temp-buffer
      (insert-file-contents source-file)
      (goto-char (point-min))
      (should (re-search-forward "(use-package gptel-magit\\b" nil t)))))

(provide 'test-ai-config-gptel-magit-lazy-loading)
;;; test-ai-config-gptel-magit-lazy-loading.el ends here
