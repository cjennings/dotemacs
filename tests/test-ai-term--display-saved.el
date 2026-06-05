;;; test-ai-term--display-saved.el --- Tests for the display-saved action -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--ai-term-display-saved' is the split path of the F9 display
;; chain -- it runs only when no agent window and no reusable edge slot
;; exist (a single-window frame, or a layout split on the other axis).
;; It reads `cj/--ai-term-last-direction' + `cj/--ai-term-last-size'
;; (with default fallbacks), builds an alist with direction + the
;; matching size key, strips any conflicting entries that came in via the
;; rule, and delegates to `display-buffer-in-direction'.
;;
;; Tests stub `display-buffer-in-direction' to capture the alist that
;; would have reached it.
;;
;; Multi-window toggle round-trips no longer resplit -- they reuse the
;; existing half (see test-ai-term--reuse-edge-window.el), so the former
;; resplit/body-width-preservation round-trip tests were retired with the
;; swap-the-slot model.  The buffer-move teardown test stays here because
;; it exercises the split-window delete path on toggle-off.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'testutil-ghostel-buffers)

(ert-deftest test-ai-term--display-saved-uses-desktop-defaults-when-state-nil ()
  "Normal: nil state on a desktop -> rightmost, size=cj/ai-term-desktop-width.
The cardinal `right' default maps to the frame-edge variant
`rightmost' so agent lands at the frame's right edge regardless of
which window is selected.  `env-laptop-p' is stubbed nil to pin the
desktop branch."
  (let (received-buf received-alist
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil)
        (cj/ai-term-desktop-width 0.5))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil))
              ((symbol-function 'display-buffer-in-direction)
               (lambda (b a)
                 (setq received-buf b received-alist a)
                 'fake-window)))
      (cj/--ai-term-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq received-buf 'fake-buf))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (= (cdr (assq 'window-width received-alist)) 0.5))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-ai-term--display-saved-uses-laptop-defaults-when-state-nil ()
  "Normal: nil state on a laptop -> bottom, size=cj/ai-term-laptop-height.
The cardinal `below' default maps to the frame-edge variant `bottom'
and the size lands on the `window-height' axis.  `env-laptop-p' is
stubbed t to pin the laptop branch."
  (let (received-alist
        (cj/--ai-term-last-direction nil)
        (cj/--ai-term-last-size nil)
        (cj/ai-term-laptop-height 0.75))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () t))
              ((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-term-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.75))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-ai-term--display-saved-uses-saved-direction-and-size-below ()
  "Normal: saved direction=below maps to bottom edge; size=0.4 passes through."
  (let (received-alist
        (cj/--ai-term-last-direction 'below)
        (cj/--ai-term-last-size 0.4))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-term-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.4))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-ai-term--display-saved-uses-saved-direction-and-size-right ()
  "Normal: saved direction=right maps to rightmost edge; size=0.7 passes through."
  (let (received-alist
        (cj/--ai-term-last-direction 'right)
        (cj/--ai-term-last-size 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-term-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (= (cdr (assq 'window-width received-alist)) 0.7))
    (should-not (assq 'window-height received-alist))))

(ert-deftest test-ai-term--display-saved-strips-conflicting-alist-entries ()
  "Boundary: caller-supplied direction/size are stripped, saved values win."
  (let (received-alist
        (cj/--ai-term-last-direction 'right)
        (cj/--ai-term-last-size 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-term-display-saved
       'fake-buf
       '((direction . below)
         (window-width . 0.2)
         (window-height . 0.3)
         (inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (= (cdr (assq 'window-width received-alist)) 0.7))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))
    ;; window-height should not be in the alist when direction is right
    ;; -- the action picks the matching size key based on direction.
    (let ((wh-cells (cl-remove-if-not
                     (lambda (cell) (eq (car-safe cell) 'window-height))
                     received-alist)))
      (should (null wh-cells)))))

(ert-deftest test-ai-term--display-saved-passes-buffer-through ()
  "Normal: BUFFER argument reaches display-buffer-in-direction unchanged."
  (let (received-buf
        (cj/--ai-term-last-direction 'right)
        (cj/--ai-term-last-size 0.5))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (b _a) (setq received-buf b) 'fake-window)))
      (cj/--ai-term-display-saved 'sentinel-buffer nil))
    (should (eq received-buf 'sentinel-buffer))))

(ert-deftest test-ai-term--toggle-after-buffer-move-no-extra-window ()
  "Regression: toggle-off must not leak a window even when buffer-move
has cleared the agent window's `quit-restore' parameter.

Reproduces Craig's repro from 2026-05-09: 3 windows, user uses
buffer-move (C-M-arrows) to relocate agent.  buffer-move swaps
buffers between windows and leaves the receiving window with no
record that it was created for the agent buffer.

Assertion: after toggle-off+toggle-on, the agent is displayed exactly
once and no spurious extra window leaks."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [buffer-move-toggle]")
        (left-name "*test-bm-left*")
        (right-name "*test-bm-right*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (right-buf (get-buffer-create right-name))
                (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((right-win (split-window (selected-window) nil 'right))
                   (agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer right-win right-buf)
              (set-window-buffer agent-win agent-buf)
              ;; Mimic buffer-move's effect: agent lives in this
              ;; window but quit-restore says nothing about it.
              (set-window-parameter agent-win 'quit-restore nil)
              (let ((display-buffer-alist (cj/--ai-term-display-rule-list))
                    (window-count-before (count-windows)))
                (select-window agent-win)
                (cj/test--call-as-gui #'cj/ai-term) ; off
                (cj/test--call-as-gui #'cj/ai-term) ; on
                (should (<= (count-windows) window-count-before))
                ;; Agent must be displayed exactly once.
                (let ((agent-windows
                       (seq-filter
                        (lambda (w)
                          (eq (window-buffer w) agent-buf))
                        (window-list))))
                  (should (= (length agent-windows) 1)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(provide 'test-ai-term--display-saved)
;;; test-ai-term--display-saved.el ends here
