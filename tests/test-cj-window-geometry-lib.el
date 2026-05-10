;;; test-cj-window-geometry-lib.el --- Tests for the shared window-geometry helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the three pure helpers in `cj-window-geometry-lib.el':
;; `cj/window-direction', `cj/window-body-size', and
;; `cj/cardinal-to-edge-direction'.

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

(provide 'test-cj-window-geometry-lib)
;;; test-cj-window-geometry-lib.el ends here
