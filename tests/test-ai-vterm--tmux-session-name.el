;;; test-ai-vterm--tmux-session-name.el --- Tests for cj/--ai-vterm-tmux-session-name -*- lexical-binding: t; -*-

;;; Commentary:
;; The tmux session name is `cj/ai-vterm-tmux-session-prefix' followed by
;; the project's basename, so reopening Claude on the same project (e.g.
;; after an Emacs crash) reattaches to the same tmux session rather than
;; spawning a new one -- and the prefix lets `tmux ls' output be filtered
;; down to AI-vterm's own sessions.  Whitespace in the basename becomes
;; hyphens so the name is safe to pass on a tmux command line.

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

(ert-deftest test-ai-vterm--tmux-session-name-dot-prefix-dir ()
  "Boundary: dot-prefix dirs preserve the dot (tmux accepts dots)."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/.emacs.d")
                   "aiv-.emacs.d"))))

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
