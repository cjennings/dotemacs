;;; test-cj-window-geometry-lib.el --- Tests for the shared window-geometry helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the pure helpers in `cj-window-geometry-lib.el':
;; `cj/window-direction', `cj/window-body-size',
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

(ert-deftest test-cj-window-geometry--body-size-right-returns-body-cols ()
  "Normal: right window with direction='right -> body-width in cols."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right)))
      (should (= (cj/window-body-size right 'right)
                 (window-body-width right))))))

(ert-deftest test-cj-window-geometry--body-size-below-returns-body-lines ()
  "Normal: below window with direction='below -> body-height in lines."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below)))
      (should (= (cj/window-body-size below 'below)
                 (window-body-height below))))))

(ert-deftest test-cj-window-geometry--body-size-narrow-window ()
  "Normal: deliberately narrow right window -> matching body cols."
  (save-window-excursion
    (delete-other-windows)
    (let* ((frame-w (frame-width))
           (target-cols (/ frame-w 4))
           (right (split-window (selected-window) (- target-cols) 'right)))
      (should (= (cj/window-body-size right 'right)
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

(provide 'test-cj-window-geometry-lib)
;;; test-cj-window-geometry-lib.el ends here
