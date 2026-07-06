;;; test-dirvish-config--quantize-thumb-size.el --- thumbnail width-bucket tests -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--dirvish-quantize-thumb-size' rounds dirvish's computed thumbnail size to
;; a coarse pixel bucket so small preview-window jitter maps to one stable cache
;; key instead of a fresh miss + regenerate (the webm thumbnail-flash bug).  Pure
;; math; the :filter-return advice that wires it onto `dirvish-media--img-size'
;; is verified live.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dirvish-config)

(declare-function cj/--dirvish-quantize-thumb-size "dirvish-config" (size bucket))

;;; ------------------------------- rounding -----------------------------------

(ert-deftest test-dirvish-quantize-rounds-down ()
  "Normal: a size below the bucket midpoint rounds down to the bucket."
  (should (= (cj/--dirvish-quantize-thumb-size 931 100) 900)))

(ert-deftest test-dirvish-quantize-rounds-up ()
  "Normal: a size above the bucket midpoint rounds up to the next bucket."
  (should (= (cj/--dirvish-quantize-thumb-size 552 100) 600)))

(ert-deftest test-dirvish-quantize-on-bucket-unchanged ()
  "Normal: a size already on a bucket boundary is returned unchanged."
  (should (= (cj/--dirvish-quantize-thumb-size 600 100) 600)))

(ert-deftest test-dirvish-quantize-jitter-maps-to-one-key ()
  "Boundary: nearby sizes within a bucket collapse to the same cache key."
  (should (= (cj/--dirvish-quantize-thumb-size 931 100)
             (cj/--dirvish-quantize-thumb-size 949 100))))

(ert-deftest test-dirvish-quantize-float-input ()
  "Boundary: a float size (pre-floor) still yields an integer bucket."
  (let ((r (cj/--dirvish-quantize-thumb-size 931.4 100)))
    (should (integerp r))
    (should (= r 900))))

;;; ------------------------------- clamping -----------------------------------

(ert-deftest test-dirvish-quantize-tiny-clamps-to-bucket ()
  "Boundary: a size that would round to 0 clamps up to one bucket, never 0."
  (should (= (cj/--dirvish-quantize-thumb-size 1 100) 100)))

;;; ------------------------------ disabled path -------------------------------

(ert-deftest test-dirvish-quantize-zero-bucket-passthrough ()
  "Error/disabled: a zero bucket returns the size unchanged."
  (should (= (cj/--dirvish-quantize-thumb-size 931 0) 931)))

(ert-deftest test-dirvish-quantize-nil-bucket-passthrough ()
  "Error/disabled: a nil bucket returns the size unchanged."
  (should (= (cj/--dirvish-quantize-thumb-size 931 nil) 931)))

(provide 'test-dirvish-config--quantize-thumb-size)
;;; test-dirvish-config--quantize-thumb-size.el ends here
