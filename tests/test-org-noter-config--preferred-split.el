;;; test-org-noter-config--preferred-split.el --- Tests for cj/org-noter--preferred-split -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/org-noter--preferred-split'. The function returns
;; `horizontal-split' when the frame's width-to-height ratio exceeds
;; 1.4, otherwise `vertical-split'. Tests mock the frame-pixel-*
;; primitives to walk the threshold from both sides.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'org-noter-config)

(defmacro test-org-noter-config--with-frame-size (width height &rest body)
  "Run BODY with `frame-pixel-width' / `frame-pixel-height' mocked to WIDTH and HEIGHT."
  (declare (indent 2) (debug t))
  `(cl-letf (((symbol-function 'frame-pixel-width) (lambda (&optional _) ,width))
             ((symbol-function 'frame-pixel-height) (lambda (&optional _) ,height)))
     ,@body))

(ert-deftest test-org-noter-config-preferred-split-wide-returns-horizontal ()
  "Normal: a 16:9 wide frame returns horizontal-split (side-by-side)."
  (test-org-noter-config--with-frame-size 1920 1080
    (should (eq 'horizontal-split (cj/org-noter--preferred-split)))))

(ert-deftest test-org-noter-config-preferred-split-tall-returns-vertical ()
  "Normal: a tall portrait frame returns vertical-split (stacked)."
  (test-org-noter-config--with-frame-size 800 1200
    (should (eq 'vertical-split (cj/org-noter--preferred-split)))))

(ert-deftest test-org-noter-config-preferred-split-square-returns-vertical ()
  "Boundary: a square frame is below the 1.4 threshold; returns vertical."
  (test-org-noter-config--with-frame-size 1000 1000
    (should (eq 'vertical-split (cj/org-noter--preferred-split)))))

(ert-deftest test-org-noter-config-preferred-split-just-above-threshold-horizontal ()
  "Boundary: a width-to-height ratio of 1.5 returns horizontal."
  (test-org-noter-config--with-frame-size 1500 1000
    (should (eq 'horizontal-split (cj/org-noter--preferred-split)))))

(ert-deftest test-org-noter-config-preferred-split-just-below-threshold-vertical ()
  "Boundary: a width-to-height ratio of 1.3 returns vertical."
  (test-org-noter-config--with-frame-size 1300 1000
    (should (eq 'vertical-split (cj/org-noter--preferred-split)))))

(provide 'test-org-noter-config--preferred-split)
;;; test-org-noter-config--preferred-split.el ends here
