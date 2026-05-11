;;; test-ai-vterm--tmux-session-name.el --- Tests for cj/--ai-vterm-tmux-session-name -*- lexical-binding: t; -*-

;;; Commentary:
;; The tmux session name is `cj/ai-vterm-tmux-session-prefix' followed by
;; the project's basename, so reopening the agent on the same project (e.g.
;; after an Emacs crash) reattaches to the same tmux session rather than
;; spawning a new one -- and the prefix lets `tmux ls' output be filtered
;; down to AI-vterm's own sessions.  The basename is sanitized to a form
;; tmux won't re-mangle: runs of whitespace become hyphens, and `.' / `:'
;; (which tmux disallows in session names and silently rewrites to `_')
;; become `_' up front so the computed name matches the real session.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--tmux-session-name-normal-project ()
  "Normal: basename gets the configured prefix."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/projects/foo")
                   "aiv-foo"))))

(ert-deftest test-ai-vterm--tmux-session-name-trailing-slash ()
  "Boundary: trailing slash collapses before basename extraction."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/projects/foo/")
                   "aiv-foo"))))

(ert-deftest test-ai-vterm--tmux-session-name-dots-become-underscores ()
  "Boundary: tmux disallows `.' in session names and rewrites it to `_',
so the basename's dots are sanitized to `_' up front -- `.emacs.d' must
yield `aiv-_emacs_d', matching the session tmux actually creates."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/.emacs.d")
                   "aiv-_emacs_d"))))

(ert-deftest test-ai-vterm--tmux-session-name-colon-becomes-underscore ()
  "Boundary: `:' is also disallowed by tmux in session names -> `_'."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/tmp/a:b")
                   "aiv-a_b"))))

(ert-deftest test-ai-vterm--tmux-session-name-space-becomes-hyphen ()
  "Boundary: a space in the basename is replaced with a hyphen."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/tmp/my work")
                   "aiv-my-work"))))

(ert-deftest test-ai-vterm--tmux-session-name-multiple-spaces-collapse ()
  "Boundary: a run of whitespace collapses to a single hyphen."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/tmp/a   b\tc")
                   "aiv-a-b-c"))))

(ert-deftest test-ai-vterm--tmux-session-name-honors-custom-prefix ()
  "Normal: a non-default prefix is what gets prepended."
  (let ((cj/ai-vterm-tmux-session-prefix "em-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/projects/foo")
                   "em-foo"))))

(provide 'test-ai-vterm--tmux-session-name)
;;; test-ai-vterm--tmux-session-name.el ends here
