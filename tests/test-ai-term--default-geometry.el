;;; test-ai-term--default-geometry.el --- Tests for host-aware display defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; ai-term's default display geometry is chosen from the frame's pixel aspect
;; ratio: a landscape frame docks the agent from the right (a width fraction), a
;; square or portrait frame docks it from the bottom (a height fraction).
;; `cj/--ai-term-direction-for-aspect' is the pure decision;
;; `cj/--ai-term-default-direction' reads the frame and delegates to it;
;; `cj/--ai-term-default-size' pairs the size fraction with that direction.
;; They feed the default fallbacks in `cj/--ai-term-capture-state' and
;; `cj/--ai-term-display-saved'.
;;
;; The direction is tested on the pure helper (no frame mocking, which would
;; trip the native-comp trampoline trap on the frame-pixel-* subrs); the size
;; helper is tested by stubbing the direction defun.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--direction-for-aspect-landscape-is-right ()
  "Normal: a wider-than-tall frame docks from the right."
  (should (eq (cj/--ai-term-direction-for-aspect 1920 1080) 'right)))

(ert-deftest test-ai-term--direction-for-aspect-portrait-is-below ()
  "Normal: a taller-than-wide frame docks from the bottom."
  (should (eq (cj/--ai-term-direction-for-aspect 1080 1920) 'below)))

(ert-deftest test-ai-term--direction-for-aspect-square-is-below ()
  "Boundary: a square frame docks from the bottom (the conserving tie-break)."
  (should (eq (cj/--ai-term-direction-for-aspect 1000 1000) 'below)))

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
