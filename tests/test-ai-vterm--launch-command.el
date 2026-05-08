;;; test-ai-vterm--launch-command.el --- Tests for cj/--ai-vterm-launch-command -*- lexical-binding: t; -*-

;;; Commentary:
;; The launch command is what gets typed into a fresh vterm shell to bring
;; up Claude inside a per-project tmux session.  The session is named after
;; the project basename so a second F9 on the same project reattaches to
;; the running Claude rather than spawning a new one.  The trailing
;; `exec bash' keeps the tmux window alive if Claude exits, leaving the
;; session intact for recovery.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--launch-command-uses-new-session-attach ()
  "Normal: starts with `tmux new-session -A' so existing sessions reattach."
  (let ((cj/ai-vterm-claude-command "claude"))
    (should (string-prefix-p
             "tmux new-session -A "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-includes-session-name ()
  "Normal: the session name comes from the basename helper."
  (let ((cj/ai-vterm-claude-command "claude"))
    (should (string-match-p
             " -s foo "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-includes-start-directory ()
  "Normal: `-c <dir>' so the new session's first window starts in DIR."
  (let ((cj/ai-vterm-claude-command "claude"))
    (should (string-match-p
             " -c /code/foo "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-includes-claude-command ()
  "Normal: the configured claude command is in the launched shell command."
  (let ((cj/ai-vterm-claude-command "claude --some-flag"))
    (should (string-match-p
             "claude --some-flag"
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-tails-with-exec-bash ()
  "Boundary: `exec bash' tails so the tmux window survives Claude exiting."
  (let ((cj/ai-vterm-claude-command "claude"))
    (should (string-match-p
             "exec bash"
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-handles-spaces-in-basename ()
  "Boundary: a basename with whitespace becomes hyphenated before quoting."
  (let ((cj/ai-vterm-claude-command "claude"))
    (should (string-match-p
             " -s my-work "
             (cj/--ai-vterm-launch-command "/code/my work")))))

(provide 'test-ai-vterm--launch-command)
;;; test-ai-vterm--launch-command.el ends here
