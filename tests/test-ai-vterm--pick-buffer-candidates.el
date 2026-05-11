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
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(ert-deftest test-ai-vterm--pick-buffer-candidates-empty-buffers ()
  "Boundary: empty buffer list -> empty alist regardless of shown."
  (cj/test--kill-agent-buffers)
  (should (null (cj/--ai-vterm-pick-buffer-candidates nil nil)))
  (should (null (cj/--ai-vterm-pick-buffer-candidates nil 'sentinel))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-nil ()
  "Normal: shown is nil -> straight alist in input order, no marker."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [a]"))
        (b2 (get-buffer-create "agent [b]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates (list b1 b2) nil)))
          (should (equal result `(("agent [a]" . ,b1)
                                   ("agent [b]" . ,b2)))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-promotes-non-shown ()
  "Normal: shown buffer sorts last with [shown] suffix; others first."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [a]"))
        (b2 (get-buffer-create "agent [b]"))
        (b3 (get-buffer-create "agent [c]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates
                       (list b1 b2 b3) b1)))
          (should (equal result
                         `(("agent [b]" . ,b2)
                           ("agent [c]" . ,b3)
                           ("agent [a] [shown]" . ,b1)))))
      (kill-buffer b1)
      (kill-buffer b2)
      (kill-buffer b3))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-only-buffer ()
  "Boundary: shown is the only entry -> single cell with [shown] marker."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [a]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates (list b1) b1)))
          (should (equal result `(("agent [a] [shown]" . ,b1)))))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--pick-buffer-candidates-shown-not-in-buffers ()
  "Boundary: stale shown buffer not in list -> all cells are non-shown."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [a]"))
        (b-stale (get-buffer-create "agent [stale]")))
    (unwind-protect
        (let ((result (cj/--ai-vterm-pick-buffer-candidates
                       (list b1) b-stale)))
          (should (equal result `(("agent [a]" . ,b1)))))
      (kill-buffer b1)
      (kill-buffer b-stale))))

(provide 'test-ai-vterm--pick-buffer-candidates)
;;; test-ai-vterm--pick-buffer-candidates.el ends here
