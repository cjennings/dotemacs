;;; test-music-config--playlist-side.el --- Tests for the F10 dock-side helper -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--music-playlist-side' maps the shared dock rule's verdict to a
;; `display-buffer-in-side-window' side: `right' stays `right', anything
;; else becomes `bottom'.  The decision itself lives in
;; `cj/preferred-dock-direction' (tested in test-cj-window-geometry-lib.el);
;; here we stub it (an ordinary defun -- safe to `cl-letf', unlike the
;; frame-* subrs) to prove the mapping and that the width fraction is
;; passed through.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'music-config)

(ert-deftest test-music-config--playlist-side-right-verdict-is-right ()
  "Normal: a `right' verdict from the dock rule docks the playlist right."
  (cl-letf (((symbol-function 'cj/preferred-dock-direction)
             (lambda (&rest _) 'right)))
    (should (eq (cj/--music-playlist-side) 'right))))

(ert-deftest test-music-config--playlist-side-below-verdict-is-bottom ()
  "Normal: a `below' verdict maps to the `bottom' side window."
  (cl-letf (((symbol-function 'cj/preferred-dock-direction)
             (lambda (&rest _) 'below)))
    (should (eq (cj/--music-playlist-side) 'bottom))))

(ert-deftest test-music-config--playlist-side-passes-width-fraction ()
  "Normal: the playlist's width fraction reaches the dock rule."
  (let ((cj/music-playlist-window-width 0.4)
        captured)
    (cl-letf (((symbol-function 'cj/preferred-dock-direction)
               (lambda (cols frac &rest _)
                 (setq captured (list cols frac))
                 'below)))
      (cj/--music-playlist-side)
      (should (= (nth 1 captured) 0.4))
      (should (integerp (nth 0 captured))))))

(provide 'test-music-config--playlist-side)
;;; test-music-config--playlist-side.el ends here
