;;; test-cj-window-geometry-lib.el --- Tests for the shared window-geometry helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the pure helpers in `cj-window-geometry-lib.el':
;; `cj/window-direction', `cj/window-replay-size',
;; `cj/cardinal-to-edge-direction', and `cj/window-at-edge'.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'cj-window-geometry-lib)

(ert-deftest test-cj-window-geometry--direction-right-split ()
  "Normal: 2-window vertical split, right-side window -> right."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right)))
      (should (eq (cj/window-direction right) 'right)))))

(ert-deftest test-cj-window-geometry--direction-left-split ()
  "Normal: 2-window vertical split, left-side window -> left."
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'right)
    (should (eq (cj/window-direction (selected-window)) 'left))))

(ert-deftest test-cj-window-geometry--direction-below-split ()
  "Normal: 2-window horizontal split, bottom window -> below."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below)))
      (should (eq (cj/window-direction below) 'below)))))

(ert-deftest test-cj-window-geometry--direction-above-split ()
  "Normal: 2-window horizontal split, top window -> above."
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'below)
    (should (eq (cj/window-direction (selected-window)) 'above))))

(ert-deftest test-cj-window-geometry--direction-single-window-default-right ()
  "Boundary: single-window frame, no DEFAULT arg -> 'right."
  (save-window-excursion
    (delete-other-windows)
    (should (eq (cj/window-direction (selected-window)) 'right))))

(ert-deftest test-cj-window-geometry--direction-single-window-with-default ()
  "Boundary: single-window frame with DEFAULT='below -> 'below."
  (save-window-excursion
    (delete-other-windows)
    (should (eq (cj/window-direction (selected-window) 'below) 'below))))

(ert-deftest test-cj-window-geometry--replay-size-right-returns-body-cols ()
  "Normal: right window with direction='right -> body-width in cols."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right)))
      (should (= (cj/window-replay-size right 'right)
                 (window-body-width right))))))

(ert-deftest test-cj-window-geometry--replay-size-below-returns-total-lines ()
  "Normal: below window with direction='below -> total-height in lines.
The vertical axis captures total-height (not body-height) so the capture/
replay round-trip is immune to the mode line's pixel height."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below)))
      (should (= (cj/window-replay-size below 'below)
                 (window-total-height below))))))

(ert-deftest test-cj-window-geometry--replay-size-narrow-window ()
  "Normal: deliberately narrow right window -> matching body cols."
  (save-window-excursion
    (delete-other-windows)
    (let* ((frame-w (frame-width))
           (target-cols (/ frame-w 4))
           (right (split-window (selected-window) (- target-cols) 'right)))
      (should (= (cj/window-replay-size right 'right)
                 (window-body-width right))))))

(ert-deftest test-cj-window-geometry--cardinal-to-edge-right ()
  "Normal: 'right -> 'rightmost."
  (should (eq (cj/cardinal-to-edge-direction 'right) 'rightmost)))

(ert-deftest test-cj-window-geometry--cardinal-to-edge-left ()
  "Normal: 'left -> 'leftmost."
  (should (eq (cj/cardinal-to-edge-direction 'left) 'leftmost)))

(ert-deftest test-cj-window-geometry--cardinal-to-edge-below ()
  "Normal: 'below -> 'bottom."
  (should (eq (cj/cardinal-to-edge-direction 'below) 'bottom)))

(ert-deftest test-cj-window-geometry--cardinal-to-edge-above ()
  "Normal: 'above -> 'top."
  (should (eq (cj/cardinal-to-edge-direction 'above) 'top)))

(ert-deftest test-cj-window-geometry--cardinal-to-edge-unknown-returns-nil ()
  "Boundary: an unknown direction symbol -> nil."
  (should (null (cj/cardinal-to-edge-direction 'sideways)))
  (should (null (cj/cardinal-to-edge-direction nil))))

;; ----------------------------- cj/window-at-edge -----------------------------

(ert-deftest test-cj-window-geometry--at-edge-2col-right-returns-right-column ()
  "Normal: 2-column split -> the right column is the right-edge half."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right)))
      (should (eq (cj/window-at-edge 'right) right)))))

(ert-deftest test-cj-window-geometry--at-edge-2col-left-returns-left-column ()
  "Normal: 2-column split -> the left column is the left-edge half."
  (save-window-excursion
    (delete-other-windows)
    (let ((left (selected-window)))
      (split-window (selected-window) nil 'right)
      (should (eq (cj/window-at-edge 'left) left)))))

(ert-deftest test-cj-window-geometry--at-edge-2row-below-returns-bottom-row ()
  "Normal: 2-row split -> the bottom row is the below-edge half."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below)))
      (should (eq (cj/window-at-edge 'below) below)))))

(ert-deftest test-cj-window-geometry--at-edge-2row-above-returns-top-row ()
  "Normal: 2-row split -> the top row is the above-edge half."
  (save-window-excursion
    (delete-other-windows)
    (let ((top (selected-window)))
      (split-window (selected-window) nil 'below)
      (should (eq (cj/window-at-edge 'above) top)))))

(ert-deftest test-cj-window-geometry--at-edge-single-window-returns-nil ()
  "Boundary: a single-window frame has no distinct half -> nil for all sides."
  (save-window-excursion
    (delete-other-windows)
    (dolist (dir '(right left below above))
      (should (null (cj/window-at-edge dir))))))

(ert-deftest test-cj-window-geometry--at-edge-axis-mismatch-returns-nil ()
  "Boundary: a 2-row split has no right/left column; a 2-col split has no row."
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'below)
    (should (null (cj/window-at-edge 'right)))
    (should (null (cj/window-at-edge 'left))))
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'right)
    (should (null (cj/window-at-edge 'below)))
    (should (null (cj/window-at-edge 'above)))))

(ert-deftest test-cj-window-geometry--at-edge-nested-right-split-returns-nil ()
  "Boundary: when the right side is itself split into rows, no single
window forms the full-height right half -> nil."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right)))
      (split-window right nil 'below)
      (should (null (cj/window-at-edge 'right))))))

(ert-deftest test-cj-window-geometry--at-edge-unknown-direction-returns-nil ()
  "Error: an unknown direction symbol -> nil even in a split frame."
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'right)
    (should (null (cj/window-at-edge 'sideways)))
    (should (null (cj/window-at-edge nil)))))

;; ----------------------------- window-size-fraction -----------------------------

(ert-deftest test-cj-window-geometry-size-fraction-normal ()
  "Normal: a window half the frame returns 0.5."
  (should (= (cj/window-size-fraction 20 40) 0.5)))

(ert-deftest test-cj-window-geometry-size-fraction-clamps-high ()
  "Boundary: a near-full window is clamped to the 0.95 ceiling."
  (should (= (cj/window-size-fraction 40 40) 0.95)))

(ert-deftest test-cj-window-geometry-size-fraction-clamps-low ()
  "Boundary: a vanishingly small window is clamped to the 0.05 floor."
  (should (= (cj/window-size-fraction 0 40) 0.05)))

(ert-deftest test-cj-window-geometry-size-fraction-custom-bounds ()
  "Boundary: explicit MIN-FRAC/MAX-FRAC override the defaults."
  (should (= (cj/window-size-fraction 1 40 0.1 0.9) 0.1))
  (should (= (cj/window-size-fraction 39 40 0.1 0.9) 0.9)))

(ert-deftest test-cj-window-geometry-size-fraction-zero-frame-nil ()
  "Error: a non-positive frame size returns nil (no divide-by-zero)."
  (should (null (cj/window-size-fraction 20 0)))
  (should (null (cj/window-size-fraction 20 -5))))

(ert-deftest test-cj-window-geometry-size-fraction-non-number-nil ()
  "Error: non-numeric arguments return nil."
  (should (null (cj/window-size-fraction nil 40)))
  (should (null (cj/window-size-fraction 20 nil))))

;; ----------------------------- preferred-dock-direction -----------------------------

(ert-deftest test-cj-window-geometry-dock-wide-frame-is-right ()
  "Normal: a frame wide enough for both panes to clear 80 docks right."
  (should (eq (cj/preferred-dock-direction 200 0.5) 'right)))

(ert-deftest test-cj-window-geometry-dock-narrow-frame-is-below ()
  "Normal: an 0.5 split on a 138-col frame leaves ~68-col panes -> below."
  (should (eq (cj/preferred-dock-direction 138 0.5) 'below)))

(ert-deftest test-cj-window-geometry-dock-boundary-exactly-min-is-right ()
  "Boundary: when the narrower pane lands exactly on 80, dock right."
  ;; 161 cols, 0.5: panel 80, main 161-80-1 = 80, narrower 80 -> right.
  (should (eq (cj/preferred-dock-direction 161 0.5) 'right)))

(ert-deftest test-cj-window-geometry-dock-boundary-one-under-min-is-below ()
  "Boundary: one column short of the floor stacks instead."
  ;; 160 cols, 0.5: panel 80, main 160-80-1 = 79, narrower 79 -> below.
  (should (eq (cj/preferred-dock-direction 160 0.5) 'below)))

(ert-deftest test-cj-window-geometry-dock-narrow-panel-fraction-governs ()
  "Normal: a slim panel fraction makes the panel the narrower pane."
  ;; 200 cols, 0.3: panel 60 < 80 -> below, even though main (139) is wide.
  (should (eq (cj/preferred-dock-direction 200 0.3) 'below))
  ;; 300 cols, 0.3: panel 90, main 209 -> right.
  (should (eq (cj/preferred-dock-direction 300 0.3) 'right)))

(ert-deftest test-cj-window-geometry-dock-honors-explicit-min-cols ()
  "Boundary: an explicit MIN-COLS overrides the default floor."
  ;; 138 cols, 0.5 -> ~68-col panes: passes a 60-floor, fails the 80-default.
  (should (eq (cj/preferred-dock-direction 138 0.5 60) 'right))
  (should (eq (cj/preferred-dock-direction 138 0.5 80) 'below)))

(ert-deftest test-cj-window-geometry-dock-honors-custom-default-var ()
  "Boundary: the default floor reads `cj/window-dock-min-columns'."
  (let ((cj/window-dock-min-columns 30))
    (should (eq (cj/preferred-dock-direction 138 0.5) 'right))))

(ert-deftest test-cj-window-geometry-dock-degenerate-input-is-below ()
  "Error: non-positive cols or out-of-range fraction stacks (safe fallback)."
  (should (eq (cj/preferred-dock-direction 0 0.5) 'below))
  (should (eq (cj/preferred-dock-direction -10 0.5) 'below))
  (should (eq (cj/preferred-dock-direction 200 0) 'below))
  (should (eq (cj/preferred-dock-direction 200 1) 'below))
  (should (eq (cj/preferred-dock-direction nil 0.5) 'below))
  (should (eq (cj/preferred-dock-direction 200 nil) 'below)))

(provide 'test-cj-window-geometry-lib)
;;; test-cj-window-geometry-lib.el ends here
