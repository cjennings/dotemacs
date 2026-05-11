;;; test-ai-vterm--sort-candidates.el --- Tests for cj/--ai-vterm-sort-candidates -*- lexical-binding: t; -*-

;;; Commentary:
;; The project picker lists candidates with a live tmux session first
;; (so an agent that survived an Emacs crash is easy to get back to),
;; then everything else.  Within the active group, projects opened this
;; session (`cj/--ai-vterm-mru') lead, most-recent first; the rest of the
;; active group, and the whole no-session group, sort alphabetically by
;; abbreviated path.  With an empty MRU it's just active-first-then-alpha.

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

(ert-deftest test-ai-vterm--sort-candidates-active-group-mru-first ()
  "Normal: within the active group, recently-opened projects lead in MRU
order; active dirs not opened this session fall after them alpha; the
no-session group trails, alpha."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-")
        (cj/--ai-vterm-mru '("/c/baz" "/c/foo")))
    (should (equal (cj/--ai-vterm-sort-candidates
                    '("/c/foo" "/c/bar" "/c/baz" "/c/qux")
                    '("aiv-foo" "aiv-bar" "aiv-baz"))
                   '("/c/baz" "/c/foo" "/c/bar" "/c/qux")))))

(ert-deftest test-ai-vterm--sort-candidates-mru-does-not-bump-inactive ()
  "Boundary: an MRU dir whose tmux session has died sorts alpha in the
no-session group, not at the top."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-")
        (cj/--ai-vterm-mru '("/c/zed")))
    (should (equal (cj/--ai-vterm-sort-candidates
                    '("/c/foo" "/c/zed" "/c/bar")
                    '("aiv-foo"))
                   '("/c/foo" "/c/bar" "/c/zed")))))

(ert-deftest test-ai-vterm--session-active-p-matches-by-derived-name ()
  "Normal: a dir is active when its derived session name is in the set."
  (let ((cj/ai-vterm-tmux-session-prefix "aiv-"))
    (should (cj/--ai-vterm-session-active-p "/c/foo" '("aiv-bar" "aiv-foo")))
    (should-not (cj/--ai-vterm-session-active-p "/c/qux" '("aiv-bar" "aiv-foo")))
    (should-not (cj/--ai-vterm-session-active-p "/c/foo" nil))))

(provide 'test-ai-vterm--sort-candidates)
;;; test-ai-vterm--sort-candidates.el ends here
