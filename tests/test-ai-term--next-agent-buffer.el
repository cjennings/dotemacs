;;; test-ai-term--next-agent-buffer.el --- Tests for cj/--ai-term-next-agent-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; The pure decision helper behind `cj/ai-term-next' (s-F9).  Given the
;; current agent buffer and the ordered list of live agent buffers, it
;; returns the next buffer in the queue, wrapping after the last.  A nil
;; or non-member CURRENT returns the first; an empty list returns nil.
;; No buffer or window side effects -- list logic only.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--next-agent-buffer-advances-from-first ()
  "Normal: current is the first element -> returns the second."
  (let ((a (get-buffer-create "agent [a]"))
        (b (get-buffer-create "agent [b]"))
        (c (get-buffer-create "agent [c]")))
    (unwind-protect
        (should (eq b (cj/--ai-term-next-agent-buffer a (list a b c))))
      (mapc #'kill-buffer (list a b c)))))

(ert-deftest test-ai-term--next-agent-buffer-advances-from-middle ()
  "Normal: current in the middle -> returns the following element."
  (let ((a (get-buffer-create "agent [a]"))
        (b (get-buffer-create "agent [b]"))
        (c (get-buffer-create "agent [c]")))
    (unwind-protect
        (should (eq c (cj/--ai-term-next-agent-buffer b (list a b c))))
      (mapc #'kill-buffer (list a b c)))))

(ert-deftest test-ai-term--next-agent-buffer-wraps-after-last ()
  "Boundary: current is the last element -> wraps to the first."
  (let ((a (get-buffer-create "agent [a]"))
        (b (get-buffer-create "agent [b]"))
        (c (get-buffer-create "agent [c]")))
    (unwind-protect
        (should (eq a (cj/--ai-term-next-agent-buffer c (list a b c))))
      (mapc #'kill-buffer (list a b c)))))

(ert-deftest test-ai-term--next-agent-buffer-single-element-returns-itself ()
  "Boundary: a one-agent queue wraps current back to itself."
  (let ((a (get-buffer-create "agent [a]")))
    (unwind-protect
        (should (eq a (cj/--ai-term-next-agent-buffer a (list a))))
      (kill-buffer a))))

(ert-deftest test-ai-term--next-agent-buffer-nil-current-returns-first ()
  "Boundary: nil current (no agent displayed) -> returns the first."
  (let ((a (get-buffer-create "agent [a]"))
        (b (get-buffer-create "agent [b]")))
    (unwind-protect
        (should (eq a (cj/--ai-term-next-agent-buffer nil (list a b))))
      (mapc #'kill-buffer (list a b)))))

(ert-deftest test-ai-term--next-agent-buffer-non-member-current-returns-first ()
  "Error: current not in the queue -> returns the first rather than nil."
  (let ((a (get-buffer-create "agent [a]"))
        (b (get-buffer-create "agent [b]"))
        (stray (get-buffer-create "agent [stray]")))
    (unwind-protect
        (should (eq a (cj/--ai-term-next-agent-buffer stray (list a b))))
      (mapc #'kill-buffer (list a b stray)))))

(ert-deftest test-ai-term--next-agent-buffer-empty-queue-returns-nil ()
  "Boundary: an empty queue returns nil (nothing to switch to)."
  (should (null (cj/--ai-term-next-agent-buffer nil '()))))

(provide 'test-ai-term--next-agent-buffer)
;;; test-ai-term--next-agent-buffer.el ends here
