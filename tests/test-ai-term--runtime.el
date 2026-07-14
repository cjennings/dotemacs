;;; test-ai-term--runtime.el --- Tests for the ai-term runtime selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Multi-backend launch: a fresh agent session can run Claude, Codex, or a
;; local model through codex --oss (ollama).  The runtime names and launch
;; strings mirror the rulesets bin/ai launcher so the two stay one mental
;; model: "claude", "codex", and "local:<model>".
;;
;; Pure pieces tested here:
;; - `cj/--ai-term-runtime-command' maps a runtime name to the full shell
;;   command (agent CLI + the shared opening prompt).
;; - `cj/--ai-term-parse-runtime-lines' parses `ai --print-runtimes' output
;;   into (NAME . LABEL) choices.
;; - `cj/--ai-term-runtime-choices' shells out to `ai' at its boundary
;;   (mocked here) and falls back to the static list when `ai' is absent.
;; The interactive picker is a thin completing-read wrapper and is not
;; tested (Interactive vs Internal split).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

;;; ------------------------- runtime -> command ------------------------------

(ert-deftest test-ai-term-runtime-command-claude-is-agent-command ()
  "Normal: \"claude\" (and nil) return `cj/ai-term-agent-command' verbatim."
  (let ((cj/ai-term-agent-command "claude \"do the thing\""))
    (should (equal (cj/--ai-term-runtime-command "claude")
                   "claude \"do the thing\""))
    (should (equal (cj/--ai-term-runtime-command nil)
                   "claude \"do the thing\""))))

(ert-deftest test-ai-term-runtime-command-codex-composes-prompt ()
  "Normal: \"codex\" is the codex CLI plus the shared opening prompt."
  (let ((cj/ai-term-agent-prompt "read the protocols"))
    (should (equal (cj/--ai-term-runtime-command "codex")
                   (concat "codex " (shell-quote-argument "read the protocols"))))))

(ert-deftest test-ai-term-runtime-command-local-model ()
  "Normal: \"local:<model>\" runs codex --oss against that ollama model.
The model is shell-quoted, and POSIX `shell-quote-argument' backslash-escapes
the colons, so the assertion matches the quoted form."
  (let ((cj/ai-term-agent-prompt "read the protocols"))
    (let ((cmd (cj/--ai-term-runtime-command "local:gpt-oss:120b")))
      (should (string-prefix-p "codex --oss --local-provider=ollama -m " cmd))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "gpt-oss:120b")) cmd))
      (should (string-suffix-p (shell-quote-argument "read the protocols") cmd)))))

(ert-deftest test-ai-term-runtime-command-boundary-empty-model ()
  "Boundary: \"local:\" with no model is rejected, not launched half-formed."
  (should-error (cj/--ai-term-runtime-command "local:") :type 'user-error))

(ert-deftest test-ai-term-runtime-command-error-unknown-runtime ()
  "Error: an unknown runtime name signals a `user-error' naming it."
  (should-error (cj/--ai-term-runtime-command "gemini") :type 'user-error)
  (condition-case err
      (cj/--ai-term-runtime-command "gemini")
    (user-error (should (string-match-p "gemini" (cadr err))))))

;;; --------------------------- choice-list parsing ---------------------------

(ert-deftest test-ai-term-parse-runtime-lines-normal ()
  "Normal: `ai --print-runtimes' lines parse into (NAME . LABEL) pairs."
  (should (equal (cj/--ai-term-parse-runtime-lines
                  "claude — Claude Code\ncodex — ChatGPT (Codex CLI)\nlocal:gpt-oss:120b — ollama\n")
                 '(("claude" . "Claude Code")
                   ("codex" . "ChatGPT (Codex CLI)")
                   ("local:gpt-oss:120b" . "ollama")))))

(ert-deftest test-ai-term-parse-runtime-lines-boundary-junk ()
  "Boundary: blank lines and lines without a separator are dropped."
  (should (equal (cj/--ai-term-parse-runtime-lines
                  "\nclaude — Claude Code\nwarning: something\n\n")
                 '(("claude" . "Claude Code")))))

(ert-deftest test-ai-term-parse-runtime-lines-boundary-empty ()
  "Boundary: empty output parses to nil."
  (should (null (cj/--ai-term-parse-runtime-lines ""))))

;;; ------------------------------ choice list --------------------------------

(ert-deftest test-ai-term-runtime-choices-uses-ai-launcher ()
  "Normal: when the `ai' launcher exists, its runtime list is the choice list."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (prog &rest _) (when (equal prog "ai") "/usr/bin/ai")))
            ((symbol-function 'process-file)
             (lambda (_prog _infile buffer _display &rest _args)
               (with-current-buffer (cond ((eq buffer t) (current-buffer))
                                          ((consp buffer) (car buffer))
                                          (t buffer))
                 (insert "claude — Claude Code\nlocal:q — ollama\n"))
               0)))
    (should (equal (cj/--ai-term-runtime-choices)
                   '(("claude" . "Claude Code") ("local:q" . "ollama"))))))

(ert-deftest test-ai-term-runtime-choices-fallback-without-ai ()
  "Boundary: with no `ai' launcher, the static claude-first list stands."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (let ((choices (cj/--ai-term-runtime-choices)))
      (should (equal (caar choices) "claude"))
      (should (assoc "codex" choices)))))

(ert-deftest test-ai-term-runtime-choices-error-ai-fails ()
  "Error: a nonzero exit from `ai' falls back instead of erroring."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (prog &rest _) (when (equal prog "ai") "/usr/bin/ai")))
            ((symbol-function 'process-file) (lambda (&rest _) 1)))
    (should (equal (caar (cj/--ai-term-runtime-choices)) "claude"))))

;;; --------------------- launch command takes an override --------------------

(ert-deftest test-ai-term-launch-command-runtime-override ()
  "Normal: an explicit agent command is embedded instead of the default.
The inner command is shell-quoted by the launch builder, so the assertion
matches the quoted form."
  (let ((cj/ai-term-agent-command "claude default"))
    (let ((cmd (cj/--ai-term-launch-command "/tmp/proj" "codex prompted")))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "codex prompted; exec bash"))
               cmd))
      (should-not (string-match-p "claude" cmd)))))

(ert-deftest test-ai-term-launch-command-no-override-falls-back ()
  "Boundary: without an override the configured agent command is used."
  (let ((cj/ai-term-agent-command "claude default"))
    (should (string-match-p
             (regexp-quote (shell-quote-argument "claude default; exec bash"))
             (cj/--ai-term-launch-command "/tmp/proj")))))

(provide 'test-ai-term--runtime)
;;; test-ai-term--runtime.el ends here
