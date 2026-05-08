;;; test-ai-vterm--tmux-session-name.el --- Tests for cj/--ai-vterm-tmux-session-name -*- lexical-binding: t; -*-

;;; Commentary:
;; The tmux session name is derived from the project's basename so that
;; reopening Claude on the same project (e.g. after an Emacs crash)
;; reattaches to the same tmux session rather than spawning a new one.
;; Whitespace in the basename gets converted to hyphens so the name is
;; safe to pass on a tmux command line.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--tmux-session-name-normal-project ()
  "Normal: a typical project path yields its basename."
  (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/projects/foo")
                 "foo")))

(ert-deftest test-ai-vterm--tmux-session-name-trailing-slash ()
  "Boundary: trailing slash collapses before basename extraction."
  (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/projects/foo/")
                 "foo")))

(ert-deftest test-ai-vterm--tmux-session-name-dot-prefix-dir ()
  "Boundary: dot-prefix dirs preserve the dot (tmux accepts dots)."
  (should (equal (cj/--ai-vterm-tmux-session-name "/home/cjennings/.emacs.d")
                 ".emacs.d")))

(ert-deftest test-ai-vterm--tmux-session-name-space-becomes-hyphen ()
  "Boundary: a space in the basename is replaced with a hyphen."
  (should (equal (cj/--ai-vterm-tmux-session-name "/tmp/my work")
                 "my-work")))

(ert-deftest test-ai-vterm--tmux-session-name-multiple-spaces-collapse ()
  "Boundary: a run of whitespace collapses to a single hyphen."
  (should (equal (cj/--ai-vterm-tmux-session-name "/tmp/a   b\tc")
                 "a-b-c")))

(provide 'test-ai-vterm--tmux-session-name)
;;; test-ai-vterm--tmux-session-name.el ends here
