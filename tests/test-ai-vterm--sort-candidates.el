;;; test-ai-vterm--sort-candidates.el --- Tests for cj/--ai-vterm-sort-candidates -*- lexical-binding: t; -*-

;;; Commentary:
;; The project picker lists candidates with a live tmux session first
;; (so a Claude that survived an Emacs crash is easy to get back to),
;; then everything else.  Within each group the order is alphabetical
;; by abbreviated path.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--sort-candidates-active-first-then-alpha ()
  "Normal: the one project with a live session leads; the rest go alpha."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-sort-candidates
                    '("/c/foo" "/c/bar" "/c/baz")
                    '("aiv-bar"))
                   '("/c/bar" "/c/baz" "/c/foo")))))

(ert-deftest test-ai-vterm--sort-candidates-multiple-active-each-group-alpha ()
  "Normal: both groups sort alphabetically internally."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-sort-candidates
                    '("/c/foo" "/c/bar" "/c/baz")
                    '("aiv-foo" "aiv-bar"))
                   '("/c/bar" "/c/foo" "/c/baz")))))

(ert-deftest test-ai-vterm--sort-candidates-no-sessions-is-plain-alpha ()
  "Boundary: nil session set -> a plain alphabetical list."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (equal (cj/--ai-vterm-sort-candidates
                    '("/c/foo" "/c/bar") nil)
                   '("/c/bar" "/c/foo")))))

(ert-deftest test-ai-vterm--sort-candidates-empty-dirs-yields-nil ()
  "Boundary: no candidates -> nil."
  (should (null (cj/--ai-vterm-sort-candidates nil '("aiv-foo")))))

(ert-deftest test-ai-vterm--session-active-p-matches-by-derived-name ()
  "Normal: a dir is active when its derived session name is in the set."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (cj/--ai-vterm-session-active-p "/c/foo" '("aiv-bar" "aiv-foo")))
    (should-not (cj/--ai-vterm-session-active-p "/c/qux" '("aiv-bar" "aiv-foo")))
    (should-not (cj/--ai-vterm-session-active-p "/c/foo" nil))))

(provide 'test-ai-vterm--sort-candidates)
;;; test-ai-vterm--sort-candidates.el ends here
