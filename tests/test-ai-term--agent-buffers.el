;;; test-ai-term--agent-buffers.el --- Tests for cj/--ai-term-agent-buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; The helper returns the list of buffers whose names start with the
;; literal prefix "agent [".  Order is the same order `buffer-list'
;; gives them (most-recently-selected first).  Non-agent buffers and
;; buffers whose names merely contain the prefix as a substring are
;; excluded.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'testutil-ghostel-buffers)

(ert-deftest test-ai-term--agent-buffers-empty-when-none-exist ()
  "Boundary: no agent-prefixed buffers anywhere -> empty list."
  (cj/test--kill-agent-buffers)
  (unwind-protect
      (should (null (cj/--ai-term-agent-buffers)))
    (cj/test--kill-agent-buffers)))

(ert-deftest test-ai-term--agent-buffers-returns-only-agent-buffers ()
  "Normal: filters to only agent-prefixed buffers, leaves others alone."
  (cj/test--kill-agent-buffers)
  (let ((c1 (get-buffer-create "agent [a]"))
        (c2 (get-buffer-create "agent [b]"))
        (other (get-buffer-create "regular-buffer")))
    (unwind-protect
        (let ((result (cj/--ai-term-agent-buffers)))
          (should (memq c1 result))
          (should (memq c2 result))
          (should-not (memq other result))
          (should (= (length result) 2)))
      (kill-buffer c1)
      (kill-buffer c2)
      (kill-buffer other))))

(ert-deftest test-ai-term--agent-buffers-anchors-prefix-not-substring ()
  "Boundary: 'foo agent [bar]' is not an agent buffer -- prefix anchored."
  (cj/test--kill-agent-buffers)
  (let ((not-agent (get-buffer-create "foo agent [bar]")))
    (unwind-protect
        (should-not (memq not-agent (cj/--ai-term-agent-buffers)))
      (kill-buffer not-agent))))

(ert-deftest test-ai-term--agent-buffers-bare-agent-not-included ()
  "Boundary: 'agent' alone (no bracket) doesn't match the 'agent [' prefix."
  (cj/test--kill-agent-buffers)
  (let ((bare (get-buffer-create "agent")))
    (unwind-protect
        (should-not (memq bare (cj/--ai-term-agent-buffers)))
      (kill-buffer bare))))

(provide 'test-ai-term--agent-buffers)
;;; test-ai-term--agent-buffers.el ends here
