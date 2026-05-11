;;; test-ai-vterm--record-mru.el --- Tests for the AI-vterm project MRU list -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--ai-vterm-record-mru' tracks which project dirs have been opened via
;; the launcher this session, most-recently-opened first, so the picker can
;; surface recently-used projects at the top of the active-sessions group.
;; `cj/--ai-vterm-mru-rank' reports a dir's position in that list (or nil).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--record-mru-pushes-to-front ()
  "Normal: a freshly recorded dir leads the list, newest first."
  (let ((cj/--ai-vterm-mru nil))
    (cj/--ai-vterm-record-mru "/c/alpha")
    (cj/--ai-vterm-record-mru "/c/beta")
    (should (equal cj/--ai-vterm-mru '("/c/beta" "/c/alpha")))))

(ert-deftest test-ai-vterm--record-mru-dedups-and-moves-to-front ()
  "Normal: re-recording a dir moves it to the front with no duplicate."
  (let ((cj/--ai-vterm-mru nil))
    (cj/--ai-vterm-record-mru "/c/alpha")
    (cj/--ai-vterm-record-mru "/c/beta")
    (cj/--ai-vterm-record-mru "/c/alpha")
    (should (equal cj/--ai-vterm-mru '("/c/alpha" "/c/beta")))))

(ert-deftest test-ai-vterm--record-mru-normalizes-trailing-slash ()
  "Boundary: `/c/foo' and `/c/foo/' are the same MRU entry."
  (let ((cj/--ai-vterm-mru nil))
    (cj/--ai-vterm-record-mru "/c/foo/")
    (cj/--ai-vterm-record-mru "/c/foo")
    (should (equal cj/--ai-vterm-mru '("/c/foo")))))

(ert-deftest test-ai-vterm--mru-rank-returns-index-or-nil ()
  "Normal/Boundary: rank is the list position; nil when the dir isn't there;
the lookup normalizes a trailing slash the same way `record-mru' does."
  (let ((cj/--ai-vterm-mru '("/c/beta" "/c/alpha")))
    (should (= 0 (cj/--ai-vterm-mru-rank "/c/beta")))
    (should (= 1 (cj/--ai-vterm-mru-rank "/c/alpha")))
    (should (= 0 (cj/--ai-vterm-mru-rank "/c/beta/")))
    (should-not (cj/--ai-vterm-mru-rank "/c/gamma"))))

(provide 'test-ai-vterm--record-mru)
;;; test-ai-vterm--record-mru.el ends here
