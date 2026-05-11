;;; test-ai-vterm--launch-command.el --- Tests for cj/--ai-vterm-launch-command -*- lexical-binding: t; -*-

;;; Commentary:
;; The launch command is what gets typed into a fresh vterm shell to bring
;; up the agent inside a per-project tmux session.  The session is named
;; `cj/ai-vterm-tmux-session-prefix' + the project basename, so a second
;; F9 on the same project reattaches to the running agent rather than
;; spawning a new one, and `tmux ls' output can be filtered to AI-vterm's
;; own sessions.  The trailing `exec bash' keeps the tmux window alive if
;; the agent exits, leaving the session intact for recovery.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--launch-command-uses-new-session-attach ()
  "Normal: starts with `tmux new-session -A' so existing sessions reattach."
  (let ((cj/ai-vterm-agent-command "agent"))
    (should (string-prefix-p
             "tmux new-session -A "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-includes-prefixed-session-name ()
  "Normal: the session name is the prefixed form from the name helper."
  (let ((cj/ai-vterm-agent-command "agent")
        (cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (string-match-p
             " -s aiv-foo "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-names-window ()
  "Normal: `-n <window-name>' so the agent window is named distinctly."
  (let ((cj/ai-vterm-agent-command "agent")
        (cj/ai-vterm-tmux-window-name "ai"))
    (should (string-match-p
             " -n ai "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-honors-custom-window-name ()
  "Boundary: a non-default window name is what `-n' gets."
  (let ((cj/ai-vterm-agent-command "agent")
        (cj/ai-vterm-tmux-window-name "agent"))
    (should (string-match-p
             " -n agent "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-includes-start-directory ()
  "Normal: `-c <dir>' so the new session's first window starts in DIR."
  (let ((cj/ai-vterm-agent-command "agent"))
    (should (string-match-p
             " -c /code/foo "
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-includes-agent-command ()
  "Normal: the configured agent command is in the launched shell command."
  (let ((cj/ai-vterm-agent-command "agent --some-flag"))
    (should (string-match-p
             "agent --some-flag"
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-tails-with-exec-bash ()
  "Boundary: `exec bash' tails so the tmux window survives the agent exiting."
  (let ((cj/ai-vterm-agent-command "agent"))
    (should (string-match-p
             "exec bash"
             (cj/--ai-vterm-launch-command "/code/foo")))))

(ert-deftest test-ai-vterm--launch-command-handles-spaces-in-basename ()
  "Boundary: a basename with whitespace becomes hyphenated before quoting."
  (let ((cj/ai-vterm-agent-command "agent")
        (cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (string-match-p
             " -s aiv-my-work "
             (cj/--ai-vterm-launch-command "/code/my work")))))

(provide 'test-ai-vterm--launch-command)
;;; test-ai-vterm--launch-command.el ends here
