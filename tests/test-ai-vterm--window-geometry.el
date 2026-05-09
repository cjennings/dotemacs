;;; test-ai-vterm--window-geometry.el --- Tests for direction + fraction helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Two pure helpers used by F9's geometry-preservation feature:
;;
;; - `cj/--ai-vterm-window-direction' classifies a window's position
;;   relative to its frame as right / below / left / above (with a
;;   right fallback when the window fills the frame).
;;
;; - `cj/--ai-vterm-window-fraction' returns the window's size on
;;   the matching axis as a fraction of the frame.
;;
;; Tests use real window splits in `save-window-excursion' rather
;; than mocking, since the helpers consume `window-edges' and
;; `frame-width' / `frame-height' directly.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--window-direction-right-split ()
  "Normal: 2-window vertical split, right-side window -> right."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right)))
      (should (eq (cj/--ai-vterm-window-direction right) 'right)))))

(ert-deftest test-ai-vterm--window-direction-left-split ()
  "Normal: 2-window vertical split, left-side window -> left."
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'right)
    (should (eq (cj/--ai-vterm-window-direction (selected-window)) 'left))))

(ert-deftest test-ai-vterm--window-direction-below-split ()
  "Normal: 2-window horizontal split, bottom window -> below."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below)))
      (should (eq (cj/--ai-vterm-window-direction below) 'below)))))

(ert-deftest test-ai-vterm--window-direction-above-split ()
  "Normal: 2-window horizontal split, top window -> above."
  (save-window-excursion
    (delete-other-windows)
    (split-window (selected-window) nil 'below)
    (should (eq (cj/--ai-vterm-window-direction (selected-window)) 'above))))

(ert-deftest test-ai-vterm--window-direction-single-window-fallback ()
  "Boundary: single-window frame -> default right."
  (save-window-excursion
    (delete-other-windows)
    (should (eq (cj/--ai-vterm-window-direction (selected-window)) 'right))))

(ert-deftest test-ai-vterm--window-fraction-right-split-half ()
  "Normal: right window of equal vertical split -> ~0.5 width fraction."
  (save-window-excursion
    (delete-other-windows)
    (let* ((right (split-window (selected-window) nil 'right))
           (frac (cj/--ai-vterm-window-fraction right 'right)))
      (should (and (> frac 0.4) (< frac 0.6))))))

(ert-deftest test-ai-vterm--window-fraction-below-split-half ()
  "Normal: bottom window of equal horizontal split -> ~0.5 height fraction."
  (save-window-excursion
    (delete-other-windows)
    (let* ((below (split-window (selected-window) nil 'below))
           (frac (cj/--ai-vterm-window-fraction below 'below)))
      (should (and (> frac 0.4) (< frac 0.6))))))

(ert-deftest test-ai-vterm--window-fraction-narrow-right-split ()
  "Normal: right window at 1/4 width -> fraction within that range."
  (save-window-excursion
    (delete-other-windows)
    (let* ((frame-w (frame-width))
           (target-cols (/ frame-w 4))
           (right (split-window (selected-window) (- target-cols) 'right))
           (frac (cj/--ai-vterm-window-fraction right 'right)))
      (should (and (> frac 0.15) (< frac 0.35))))))

(provide 'test-ai-vterm--window-geometry)
;;; test-ai-vterm--window-geometry.el ends here
