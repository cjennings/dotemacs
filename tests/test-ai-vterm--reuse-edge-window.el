;;; test-ai-vterm--reuse-edge-window.el --- Tests for edge-window reuse -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--ai-vterm-reuse-edge-window' is the display-buffer action that
;; reuses the window already forming the half the agent would occupy
;; (the right column on a desktop, the bottom row on a laptop) instead
;; of splitting a third window in.  It runs between
;; `cj/--ai-vterm-reuse-existing-agent' and `cj/--ai-vterm-display-saved'
;; in the rule chain.
;;
;; Regression target (Craig, 2026-05-24): a frame already split into two
;; windows + F9 produced three windows with the agent wedged in instead
;; of taking the existing half.  These tests assert the window *count*
;; stays put -- the dimension the older display-saved tests never checked.
;;
;; Tests build real windows (split-window) and route a fresh agent buffer
;; through the actual `cj/--ai-vterm-display-rule-list', the same pattern
;; as test-ai-vterm--display-saved.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(defun cj/test--displayed-buffer-names ()
  "Return the buffer names shown in the selected frame, left/top to right/bottom."
  (mapcar (lambda (w) (buffer-name (window-buffer w)))
          (window-list nil 'never)))

(ert-deftest test-ai-vterm--reuse-edge-window-2col-desktop-no-third-window ()
  "Normal: F9 in a 2-column split reuses the right column, no third window.
Desktop default direction is `right', so the agent takes the existing
right half: the frame stays at two windows [left | agent]."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [edge-2col]")
        (left-name "*test-edge-left*")
        (right-name "*test-edge-right*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
            (let ((left-buf (get-buffer-create left-name))
                  (right-buf (get-buffer-create right-name))
                  (agent-buf (get-buffer-create agent-name)))
              (set-window-buffer (selected-window) left-buf)
              (let ((rw (split-window (selected-window) nil 'right)))
                (set-window-buffer rw right-buf))
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                (display-buffer agent-buf))
              (should (= (count-windows) 2))
              (let ((bufs (cj/test--displayed-buffer-names)))
                (should (member agent-name bufs))
                (should (member left-name bufs))
                ;; the right column now holds the agent, not the old buffer
                (should-not (member right-name bufs))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--reuse-edge-window-2row-laptop-no-third-window ()
  "Normal: F9 in a 2-row split on a laptop reuses the bottom row.
Laptop default direction is `below', so the agent takes the existing
bottom half: the frame stays at two windows."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [edge-2row]")
        (top-name "*test-edge-top*")
        (bottom-name "*test-edge-bottom*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () t)))
            (let ((top-buf (get-buffer-create top-name))
                  (bottom-buf (get-buffer-create bottom-name))
                  (agent-buf (get-buffer-create agent-name)))
              (set-window-buffer (selected-window) top-buf)
              (let ((bw (split-window (selected-window) nil 'below)))
                (set-window-buffer bw bottom-buf))
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                (display-buffer agent-buf))
              (should (= (count-windows) 2))
              (let ((bufs (cj/test--displayed-buffer-names)))
                (should (member agent-name bufs))
                (should (member top-name bufs))
                (should-not (member bottom-name bufs))))))
      (when (get-buffer top-name) (kill-buffer top-name))
      (when (get-buffer bottom-name) (kill-buffer bottom-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--reuse-edge-window-single-window-splits ()
  "Boundary: a single-window frame still splits to create the half.
No existing edge window to reuse, so the display-saved path runs and
the frame goes from one window to two with the agent present."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [edge-single]")
        (sole-name "*test-edge-sole*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
            (let ((sole-buf (get-buffer-create sole-name))
                  (agent-buf (get-buffer-create agent-name)))
              (set-window-buffer (selected-window) sole-buf)
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                (display-buffer agent-buf))
              (should (= (count-windows) 2))
              (should (member agent-name (cj/test--displayed-buffer-names))))))
      (when (get-buffer sole-name) (kill-buffer sole-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--reuse-edge-window-axis-mismatch-falls-through ()
  "Error/Boundary: a top/bottom split on a desktop has no right half.
Desktop direction is `right' but the frame is split horizontally, so no
single full-height right column exists to reuse.  The chain falls
through to display-saved, which splits a right column -- agent still
ends up displayed."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [edge-mismatch]")
        (top-name "*test-edge-mm-top*")
        (bottom-name "*test-edge-mm-bottom*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
            (let ((top-buf (get-buffer-create top-name))
                  (bottom-buf (get-buffer-create bottom-name))
                  (agent-buf (get-buffer-create agent-name)))
              (set-window-buffer (selected-window) top-buf)
              (let ((bw (split-window (selected-window) nil 'below)))
                (set-window-buffer bw bottom-buf))
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                (display-buffer agent-buf))
              ;; No half to reuse, so a fresh column is split: three windows.
              (should (member agent-name (cj/test--displayed-buffer-names)))
              (should (member top-name (cj/test--displayed-buffer-names)))
              (should (member bottom-name (cj/test--displayed-buffer-names))))))
      (when (get-buffer top-name) (kill-buffer top-name))
      (when (get-buffer bottom-name) (kill-buffer bottom-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--reuse-edge-window-toggle-off-restores-displaced ()
  "Normal: toggle-off after a slot reuse restores the displaced buffer.
=| 1 | 2 |= + show agent -> =| 1 | A |=; toggle off -> =| 1 | 2 |= again,
window count stays 2 (the native `quit-restore-window' puts 2 back)."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [edge-restore]")
        (left-name "*test-restore-left*")
        (right-name "*test-restore-right*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
            (let ((left-buf (get-buffer-create left-name))
                  (right-buf (get-buffer-create right-name))
                  (agent-buf (get-buffer-create agent-name)))
              (set-window-buffer (selected-window) left-buf)
              (let ((rw (split-window (selected-window) nil 'right)))
                (set-window-buffer rw right-buf))
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                (display-buffer agent-buf)
                (should (= (count-windows) 2))
                (should (member agent-name (cj/test--displayed-buffer-names)))
                ;; Toggle off -> the displaced buffer (2) returns to the slot.
                (cj/test--call-as-gui #'cj/ai-vterm)
                (should (= (count-windows) 2))
                (let ((bufs (cj/test--displayed-buffer-names)))
                  (should (member right-name bufs))
                  (should (member left-name bufs))
                  (should-not (member agent-name bufs)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--reuse-edge-window-cycle-keeps-count-and-swaps ()
  "Normal: on/off/on cycle keeps the window count at 2 and swaps the slot.
=| 1 | 2 |= -> on =| 1 | A |= -> off =| 1 | 2 |= -> on =| 1 | A |=, never
creating or deleting a window, and the agent returns to the same slot at
the same width."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [edge-cycle]")
        (left-name "*test-cycle-left*")
        (right-name "*test-cycle-right*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
            (let ((left-buf (get-buffer-create left-name))
                  (right-buf (get-buffer-create right-name))
                  (agent-buf (get-buffer-create agent-name))
                  slot-width)
              (set-window-buffer (selected-window) left-buf)
              (let ((rw (split-window (selected-window) nil 'right)))
                (set-window-buffer rw right-buf)
                (setq slot-width (window-body-width rw)))
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                ;; on
                (display-buffer agent-buf)
                (should (= (count-windows) 2))
                ;; off
                (cj/test--call-as-gui #'cj/ai-vterm)
                (should (= (count-windows) 2))
                (should-not (cj/--ai-vterm-displayed-agent-window))
                ;; on again
                (cj/test--call-as-gui #'cj/ai-vterm)
                (should (= (count-windows) 2))
                (let ((win (cj/--ai-vterm-displayed-agent-window)))
                  (should (windowp win))
                  (should (eq (window-buffer win) agent-buf))
                  ;; reused the same slot -> same body width as the
                  ;; original right column
                  (should (= (window-body-width win) slot-width)))
                (should-not (member right-name (cj/test--displayed-buffer-names)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--reuse-edge-window-toggle-keeps-same-agent-with-multiple ()
  "Regression: with two agents alive, toggle-off then on restores the SAME
agent, not a different one.  Toggle-off must not bury the agent to the end
of the buffer list -- if it does, `cj/--ai-vterm-dispatch' re-shows the
most-recent agent, which would now be the other one."
  (cj/test--kill-agent-buffers)
  (let ((a1-name "agent [multi-1]")
        (a2-name "agent [multi-2]")
        (left-name "*test-multi-left*")
        (right-name "*test-multi-right*")
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
            (let ((a1 (get-buffer-create a1-name))
                  (a2 (get-buffer-create a2-name))
                  (left-buf (get-buffer-create left-name))
                  (right-buf (get-buffer-create right-name)))
              ;; Make A2 the most-recent agent.
              (bury-buffer a1)
              (set-window-buffer (selected-window) left-buf)
              (let ((rw (split-window (selected-window) nil 'right)))
                (set-window-buffer rw right-buf))
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                (display-buffer a2)                 ; | left | A2 |
                (should (eq (window-buffer (cj/--ai-vterm-displayed-agent-window))
                            a2))
                (cj/test--call-as-gui #'cj/ai-vterm)                       ; off -> | left | right |
                (should-not (cj/--ai-vterm-displayed-agent-window))
                (cj/test--call-as-gui #'cj/ai-vterm)                       ; on -> must bring A2 back
                (should (eq (window-buffer (cj/--ai-vterm-displayed-agent-window))
                            a2))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(provide 'test-ai-vterm--reuse-edge-window)
;;; test-ai-vterm--reuse-edge-window.el ends here
