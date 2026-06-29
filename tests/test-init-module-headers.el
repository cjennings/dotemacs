;;; test-init-module-headers.el --- Validate load-graph header contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Enforces the module load-graph header standard from
;; docs/specs/init-load-graph-spec-doing.org against every module that has been
;; classified so far.  Classification proceeds in batches; a module joins
;; `test-init-header--classified-modules' once its header declares the
;; contract.  When that list reaches parity with the modules required by
;; init.el, the project's Phase 1 exit criterion is met.
;;
;; The contract is seven commentary lines after `;;; Commentary:':
;;   Layer:, Category:, Load shape:, Eager reason: (only when eager),
;;   Top-level side effects:, Runtime requires:, Direct test load:.
;;
;; This test reads module files directly; it does not load them, so it adds
;; no startup or package dependencies of its own.

;;; Code:

(require 'ert)
(require 'seq)

(defconst test-init-header--classified-modules
  '(;; Batch 1 — Foundation (Layer 1)
    "system-lib"
    "user-constants"
    "host-environment"
    "system-defaults"
    "keyboard-compat"
    "keybindings"
    "config-utilities"
    ;; Batch 2 — Text/editing command modules (Layer 2)
    "custom-case"
    "custom-comments"
    "custom-datetime"
    "custom-buffer-file"
    "custom-line-paragraph"
    "custom-counts"
    "custom-format"
    "custom-text-transform"
    "custom-ordering"
    "custom-text-enclose"
    "custom-whitespace"
    ;; Batch 3 — Core libraries and command modules (Layer 1-3)
    "external-open"
    "media-utils"
    "auth-config"
    "keyboard-macros"
    "system-utils"
    "text-config"
    "undead-buffers"
    ;; Batch 4 — UI / core-UX modules (Layer 2)
    "auto-dim-config"
    "ui-config"
    "ui-theme"
    "ui-navigation"
    "font-config"
    "selection-framework"
    "modeline-config"
    "mousetrap-mode"
    "dashboard-config"
    "nerd-icons-config"
    ;; Batch 5 — Dev entry-points, diff, help, lint, VC (Layer 2)
    "coverage-core"
    "coverage-elisp"
    "dev-fkeys"
    "diff-config"
    "help-config"
    "help-utils"
    "flycheck-config"
    "test-runner"
    "vc-config"
    ;; Batch 6 — Programming modules (Layer 2-4)
    "prog-general"
    "prog-c"
    "prog-go"
    "prog-lisp"
    "prog-python"
    "prog-webdev"
    "prog-json"
    "prog-yaml"
    "prog-shell"
    "prog-training"
    ;; Batch 7 — Org modules (Layer 3-4)
    "org-config"
    "org-agenda-config"
    "org-babel-config"
    "org-capture-config"
    "org-contacts-config"
    "org-drill-config"
    "org-export-config"
    "org-noter-config"
    "org-refile-config"
    "org-reveal-config"
    "org-roam-config"
    "org-webclipper"
    "hugo-config"
    ;; Batch 8 — Domain / integration / optional modules (Layer 2-4)
    "ai-term"
    "browser-config"
    "calendar-sync"
    "calibredb-epub-config"
    "chrono-tools"
    "dirvish-config"
    "dwim-shell-config"
    "elfeed-config"
    "erc-config"
    "eshell-config"
    "eww-config"
    "face-diagnostic"
    "flyspell-and-abbrev"
    "games-config"
    "gloss-config"
    "httpd-config"
    "jumper"
    "latex-config"
    ;; Batch 9 — Remaining domain / integration / optional modules (Layer 2-4)
    "pearl-config"
    "local-repository"
    "lorem-optimum"
    "mail-config"
    "markdown-config"
    "music-config"
    "pdf-config"
    "quick-video-capture"
    "reconcile-open-repos"
    "restclient-config"
    "slack-config"
    "system-commands"
    "telega-config"
    "tramp-config"
    "transcription-config"
    "video-audio-recording"
    "weather-config"
    "wrap-up")
  "Modules annotated with the load-graph header contract.
Grows one batch at a time.  Parity with the init.el require set is the
Phase 1 exit criterion.")

(defconst test-init-header--required-labels
  '("Layer:"
    "Category:"
    "Load shape:"
    "Top-level side effects:"
    "Runtime requires:"
    "Direct test load:")
  "Header labels every classified module must declare.
`Eager reason:' is required additionally, but only when the load shape is
eager; it is checked separately.")

(defun test-init-header--header-text (module)
  "Return MODULE's commentary header text (everything before `;;; Code:')."
  (let ((file (expand-file-name (concat module ".el")
                                (expand-file-name "modules" user-emacs-directory))))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties
       (point-min)
       (or (save-excursion
             (goto-char (point-min))
             (when (re-search-forward "^;;; Code:" nil t)
               (match-beginning 0)))
           (point-max))))))

(defun test-init-header--missing-labels (header)
  "Return the list of required labels absent from HEADER.
Includes `Eager reason:' when HEADER declares an eager load shape but
omits the reason."
  (let ((missing
         (seq-remove (lambda (label) (string-match-p (regexp-quote label) header))
                     test-init-header--required-labels)))
    (when (and (string-match-p "Load shape:[ \t]*eager" header)
               (not (string-match-p "Eager reason:" header)))
      (setq missing (append missing '("Eager reason:"))))
    missing))

(ert-deftest test-init-header-classified-modules-declare-contract ()
  "Normal: every classified module declares all required header lines."
  (let (failures)
    (dolist (module test-init-header--classified-modules)
      (let ((missing (test-init-header--missing-labels
                      (test-init-header--header-text module))))
        (when missing
          (push (format "%s missing: %s" module (string-join missing ", "))
                failures))))
    (should-not failures)))

(ert-deftest test-init-header-detects-single-missing-line ()
  "Boundary: a header missing exactly one line is reported by that line's name."
  (let ((header (concat ";; Layer: 1 (Foundation).\n"
                        ";; Category: F.\n"
                        ";; Load shape: command.\n"
                        ";; Top-level side effects: none.\n"
                        ";; Runtime requires: none.\n")))
    ;; Missing only `Direct test load:'.
    (should (equal '("Direct test load:")
                   (test-init-header--missing-labels header)))))

(ert-deftest test-init-header-eager-requires-reason ()
  "Error: an eager load shape with no `Eager reason:' flags the omission."
  (let ((header (concat ";; Layer: 1 (Foundation).\n"
                        ";; Category: F.\n"
                        ";; Load shape: eager.\n"
                        ";; Top-level side effects: none.\n"
                        ";; Runtime requires: none.\n"
                        ";; Direct test load: yes.\n")))
    (should (member "Eager reason:" (test-init-header--missing-labels header)))))

(ert-deftest test-init-header-scoping-only-checks-allowlist ()
  "Error: only allowlisted modules are enforced, and the list stays clean.
A name never added to the allowlist stands in for any not-yet-classified
module: the validator checks allowlist members only. The duplicate guard
keeps the list honest as it grows batch by batch."
  (should-not (member "not-a-classified-module" test-init-header--classified-modules))
  (should (equal (length test-init-header--classified-modules)
                 (length (delete-dups (copy-sequence test-init-header--classified-modules))))))

(provide 'test-init-module-headers)
;;; test-init-module-headers.el ends here
