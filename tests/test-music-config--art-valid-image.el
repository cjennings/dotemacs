;;; test-music-config--art-valid-image.el --- Tests for fetched-image validation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music-art--valid-image-p': recognize whether fetched bytes
;; are actually a displayable image, so an empty body, an HTML error page served
;; 200, or a text response is rejected before it lands in the cache.  Detection
;; is by image header (`image-type-from-data'), which works headless.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(defconst test-art--png-1x1
  (base64-decode-string
   (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk"
           "YPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="))
  "A minimal valid 1x1 PNG, as raw bytes.")

(ert-deftest test-music-config--art-valid-image-normal-png ()
  "Normal: real PNG bytes are recognized as a valid image."
  (should (cj/music-art--valid-image-p test-art--png-1x1)))

(ert-deftest test-music-config--art-valid-image-error-html ()
  "Error: an HTML error page served 200 is not a valid image."
  (should-not (cj/music-art--valid-image-p "<html><body>502 Bad Gateway</body></html>")))

(ert-deftest test-music-config--art-valid-image-error-text ()
  "Error: arbitrary text is not a valid image."
  (should-not (cj/music-art--valid-image-p "this is not an image")))

(ert-deftest test-music-config--art-valid-image-boundary-empty ()
  "Boundary: an empty body is not a valid image."
  (should-not (cj/music-art--valid-image-p "")))

(ert-deftest test-music-config--art-valid-image-boundary-nil ()
  "Boundary: nil is not a valid image."
  (should-not (cj/music-art--valid-image-p nil)))

(provide 'test-music-config--art-valid-image)
;;; test-music-config--art-valid-image.el ends here
