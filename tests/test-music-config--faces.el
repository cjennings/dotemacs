;;; test-music-config--faces.el --- music playlist face definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; The playlist header propertizes text with cj/music-* faces.  Each must be a
;; defined face (defface) or the reference is invalid -- an undefined face spams
;; "Invalid face reference" on every header render.  The faces inherit from
;; themed base faces so the theme still owns their colors.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'music-config)

(ert-deftest test-music-config-header-faces-are-defined ()
  "Normal: every cj/music face the playlist header uses is a defined face."
  (dolist (f '(cj/music-header-face
               cj/music-header-value-face
               cj/music-mode-on-face
               cj/music-mode-off-face
               cj/music-keyhint-face))
    (should (facep f))))

(provide 'test-music-config--faces)
;;; test-music-config--faces.el ends here
