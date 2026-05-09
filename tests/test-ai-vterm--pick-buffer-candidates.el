;;; test-ai-vterm--pick-buffer-candidates.el --- Tests for the M-F9 candidate builder -*- lexical-binding: t; -*-

;;; Commentary:
;; The candidate builder is a pure function: given an MRU list of
;; alive AI-vterm buffers and the currently-displayed buffer (or
;; nil), it returns an alist of (DISPLAY-NAME . BUFFER) cells.
;;
;; Sort rule: non-shown buffers come first in their input order,
;; then the shown buffer (if it's in the list) appears last with a
;; \" [shown]\" suffix.  The intent is that the default `completing-
;; read' selection lands on a non-shown candidate so RET means
;; \"give me the other one\".

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(defun test-ai-vterm--pbc-cleanup ()
  "Kill any leftover claude-prefixed buffers."
  (dolist (b (buffer-list))
    (when (string-prefix-p "claude [" (buffer-name b))
      (kill-buffer b))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-empty-buffers ()
  "Boundary: empty buffer list -> empty alist regardless of shown."
  (test-ai-vterm--pbc-cleanup)
  (should (null (cj/--ai-vterm-pick-buffer-candidates nil nil)))
  (should (null (cj/--ai-vterm-pick-buffer-candidates nil 'sentinel))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-nil ()
  "Normal: shown is nil -> straight alist in input order, no marker."
  (test-ai-vterm--pbc-cleanup)
  (let ((b1 (get-buffer-create "claude [a]"))
        (b2 (get-buffer-create "claude [b]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates (list b1 b2) nil)))
          (should (equal result `(("claude [a]" . ,b1)
                                   ("claude [b]" . ,b2)))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-promotes-non-shown ()
  "Normal: shown buffer sorts last with [shown] suffix; others first."
  (test-ai-vterm--pbc-cleanup)
  (let ((b1 (get-buffer-create "claude [a]"))
        (b2 (get-buffer-create "claude [b]"))
        (b3 (get-buffer-create "claude [c]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates
                       (list b1 b2 b3) b1)))
          (should (equal result
                         `(("claude [b]" . ,b2)
                           ("claude [c]" . ,b3)
                           ("claude [a] [shown]" . ,b1)))))
      (kill-buffer b1)
      (kill-buffer b2)
      (kill-buffer b3))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-only-buffer ()
  "Boundary: shown is the only entry -> single cell with [shown] marker."
  (test-ai-vterm--pbc-cleanup)
  (let ((b1 (get-buffer-create "claude [a]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates (list b1) b1)))
          (should (equal result `(("claude [a] [shown]" . ,b1)))))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-not-in-buffers ()
  "Boundary: stale shown buffer not in list -> all cells are non-shown."
  (test-ai-vterm--pbc-cleanup)
  (let ((b1 (get-buffer-create "claude [a]"))
        (b-stale (get-buffer-create "claude [stale]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates
                       (list b1) b-stale)))
          (should (equal result `(("claude [a]" . ,b1)))))
      (kill-buffer b1)
      (kill-buffer b-stale))))

(provide 'test-ai-vterm--pick-buffer-candidates)
;;; test-ai-vterm--pick-buffer-candidates.el ends here
