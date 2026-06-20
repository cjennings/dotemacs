;;; test-ai-term--default-geometry.el --- Tests for host-aware display defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; ai-term's default display geometry is chosen from the frame's column
;; width: the agent docks from the right (a width fraction) only when a
;; side-by-side split would leave both panes at least
;; `cj/window-dock-min-columns' wide, otherwise from the bottom (a height
;; fraction).  `cj/--ai-term-default-direction' reads the frame width and
;; delegates the decision to `cj/preferred-dock-direction' (tested in
;; test-cj-window-geometry-lib.el); `cj/--ai-term-default-size' pairs the
;; size fraction with that direction.  They feed the default fallbacks in
;; `cj/--ai-term-capture-state' and `cj/--ai-term-display-saved'.
;;
;; The direction is tested by stubbing `cj/preferred-dock-direction' (an
;; ordinary defun -- safe to `cl-letf', unlike the frame-* subrs, which
;; would trip the native-comp trampoline trap); the size helper is tested
;; by stubbing the direction defun.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--default-direction-delegates-to-dock-rule ()
  "Normal: default-direction passes the desktop-width fraction to the dock rule
and returns its verdict."
  (let ((cj/ai-term-desktop-width 0.5)
        captured)
    (cl-letf (((symbol-function 'cj/preferred-dock-direction)
               (lambda (cols frac &rest _)
                 (setq captured (list cols frac))
                 'below)))
      (should (eq (cj/--ai-term-default-direction) 'below))
      ;; the fraction passed is the agent's desktop-width
      (should (= (nth 1 captured) 0.5))
      ;; the first argument is a column count (the frame width)
      (should (integerp (nth 0 captured))))))

(ert-deftest test-ai-term--default-direction-returns-right-when-rule-says ()
  "Normal: when the dock rule returns `right', so does default-direction."
  (cl-letf (((symbol-function 'cj/preferred-dock-direction)
             (lambda (&rest _) 'right)))
    (should (eq (cj/--ai-term-default-direction) 'right))))

(ert-deftest test-ai-term--default-size-pairs-width-with-right ()
  "Normal: when the direction is `right' the size is the width fraction."
  (let ((cj/ai-term-laptop-height 0.75)
        (cj/ai-term-desktop-width 0.5))
    (cl-letf (((symbol-function 'cj/--ai-term-default-direction) (lambda (&rest _) 'right)))
      (should (= (cj/--ai-term-default-size) 0.5)))))

(ert-deftest test-ai-term--default-size-pairs-height-with-below ()
  "Normal: when the direction is `below' the size is the height fraction."
  (let ((cj/ai-term-laptop-height 0.75)
        (cj/ai-term-desktop-width 0.5))
    (cl-letf (((symbol-function 'cj/--ai-term-default-direction) (lambda (&rest _) 'below)))
      (should (= (cj/--ai-term-default-size) 0.75)))))

(ert-deftest test-ai-term--default-size-respects-custom-values ()
  "Boundary: the helper returns the customized values, not the literals."
  (let ((cj/ai-term-laptop-height 0.6)
        (cj/ai-term-desktop-width 0.33))
    (cl-letf (((symbol-function 'cj/--ai-term-default-direction) (lambda (&rest _) 'below)))
      (should (= (cj/--ai-term-default-size) 0.6)))
    (cl-letf (((symbol-function 'cj/--ai-term-default-direction) (lambda (&rest _) 'right)))
      (should (= (cj/--ai-term-default-size) 0.33)))))

(provide 'test-ai-term--default-geometry)
;;; test-ai-term--default-geometry.el ends here
