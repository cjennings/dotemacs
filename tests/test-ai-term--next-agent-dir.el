;;; test-ai-term--next-agent-dir.el --- Tests for cj/--ai-term-next-agent-dir -*- lexical-binding: t; -*-

;;; Commentary:
;; The pure decision helper behind `cj/ai-term-next'.  Given the current
;; active-agent project dir and the ordered list of active-agent dirs, it
;; returns the next dir in the queue, wrapping after the last.  A nil or
;; non-member CURRENT returns the first; an empty list returns nil.  Dirs are
;; matched with `member' (string equality).  No side effects -- list logic only.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defconst test-ai-term--dirs '("/p/a" "/p/b" "/p/c"))

(ert-deftest test-ai-term--next-agent-dir-advances-from-first ()
  "Normal: current is the first element -> returns the second."
  (should (equal "/p/b" (cj/--ai-term-next-agent-dir "/p/a" test-ai-term--dirs))))

(ert-deftest test-ai-term--next-agent-dir-advances-from-middle ()
  "Normal: current in the middle -> returns the following element."
  (should (equal "/p/c" (cj/--ai-term-next-agent-dir "/p/b" test-ai-term--dirs))))

(ert-deftest test-ai-term--next-agent-dir-wraps-after-last ()
  "Boundary: current is the last element -> wraps to the first."
  (should (equal "/p/a" (cj/--ai-term-next-agent-dir "/p/c" test-ai-term--dirs))))

(ert-deftest test-ai-term--next-agent-dir-single-element-returns-itself ()
  "Boundary: a one-agent queue wraps current back to itself."
  (should (equal "/p/a" (cj/--ai-term-next-agent-dir "/p/a" '("/p/a")))))

(ert-deftest test-ai-term--next-agent-dir-nil-current-returns-first ()
  "Boundary: nil current (no agent displayed) -> returns the first."
  (should (equal "/p/a" (cj/--ai-term-next-agent-dir nil '("/p/a" "/p/b")))))

(ert-deftest test-ai-term--next-agent-dir-non-member-current-returns-first ()
  "Error: current not in the queue -> returns the first rather than nil."
  (should (equal "/p/a" (cj/--ai-term-next-agent-dir "/p/stray" '("/p/a" "/p/b")))))

(ert-deftest test-ai-term--next-agent-dir-empty-queue-returns-nil ()
  "Boundary: an empty queue returns nil (nothing to switch to)."
  (should (null (cj/--ai-term-next-agent-dir nil '()))))

(provide 'test-ai-term--next-agent-dir)
;;; test-ai-term--next-agent-dir.el ends here
