;;; test-ai-vterm--claude-buffers.el --- Tests for cj/--ai-vterm-claude-buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; The helper returns the list of buffers whose names start with the
;; literal prefix "claude [".  Order is the same order `buffer-list'
;; gives them (most-recently-selected first).  Non-claude buffers and
;; buffers whose names merely contain the prefix as a substring are
;; excluded.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(ert-deftest test-ai-vterm--claude-buffers-empty-when-none-exist ()
  "Boundary: no claude-prefixed buffers anywhere -> empty list."
  (cj/test--kill-claude-buffers)
  (unwind-protect
      (should (null (cj/--ai-vterm-claude-buffers)))
    (cj/test--kill-claude-buffers)))

(ert-deftest test-ai-vterm--claude-buffers-returns-only-claude-buffers ()
  "Normal: filters to only claude-prefixed buffers, leaves others alone."
  (cj/test--kill-claude-buffers)
  (let ((c1 (get-buffer-create "claude [a]"))
        (c2 (get-buffer-create "claude [b]"))
        (other (get-buffer-create "regular-buffer")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-claude-buffers)))
          (should (memq c1 result))
          (should (memq c2 result))
          (should-not (memq other result))
          (should (= (length result) 2)))
      (kill-buffer c1)
      (kill-buffer c2)
      (kill-buffer other))))

(ert-deftest test-ai-vterm--claude-buffers-anchors-prefix-not-substring ()
  "Boundary: 'foo claude [bar]' is not a claude buffer -- prefix anchored."
  (cj/test--kill-claude-buffers)
  (let ((not-claude (get-buffer-create "foo claude [bar]")))
    (unwind-protect
        (should-not (memq not-claude (cj/--ai-vterm-claude-buffers)))
      (kill-buffer not-claude))))

(ert-deftest test-ai-vterm--claude-buffers-bare-claude-not-included ()
  "Boundary: 'claude' alone (no bracket) doesn't match the 'claude [' prefix."
  (cj/test--kill-claude-buffers)
  (let ((bare (get-buffer-create "claude")))
    (unwind-protect
        (should-not (memq bare (cj/--ai-vterm-claude-buffers)))
      (kill-buffer bare))))

(provide 'test-ai-vterm--claude-buffers)
;;; test-ai-vterm--claude-buffers.el ends here
