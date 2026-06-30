;;; test-nov-reading--palette.el --- nov reading-palette tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-logic tests for the nov-mode reading-palette selector: name->face
;; resolution and the cycle order (palettes, then the no-palette state, wrapping).
;; The buffer-local face-remap application is exercised live, not here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'nov-reading)

(declare-function cj/nov--reading-palette-face "nov-reading" (name))
(declare-function cj/nov--next-reading-palette "nov-reading" (current names))
(defvar cj/nov-reading-palettes)

;;; ----------------------- cj/nov--reading-palette-face -----------------------

(ert-deftest test-nov-reading-palette-face-known ()
  "Normal: a known palette name resolves to its face."
  (let ((cj/nov-reading-palettes '(("sepia" . cj/nov-reading-sepia)
                                   ("dark"  . cj/nov-reading-dark))))
    (should (eq (cj/nov--reading-palette-face "sepia") 'cj/nov-reading-sepia))
    (should (eq (cj/nov--reading-palette-face "dark") 'cj/nov-reading-dark))))

(ert-deftest test-nov-reading-palette-face-unknown ()
  "Error: an unknown name resolves to nil."
  (let ((cj/nov-reading-palettes '(("sepia" . cj/nov-reading-sepia))))
    (should-not (cj/nov--reading-palette-face "nope"))))

(ert-deftest test-nov-reading-palette-face-nil ()
  "Boundary: a nil name resolves to nil."
  (let ((cj/nov-reading-palettes '(("sepia" . cj/nov-reading-sepia))))
    (should-not (cj/nov--reading-palette-face nil))))

;;; ----------------------- cj/nov--next-reading-palette -----------------------

(ert-deftest test-nov-reading-next-palette-advances ()
  "Normal: cycles to the next palette in order."
  (should (equal (cj/nov--next-reading-palette "sepia" '("sepia" "dark" "light"))
                 "dark")))

(ert-deftest test-nov-reading-next-palette-last-to-none ()
  "Boundary: the last palette cycles to the no-palette state (nil)."
  (should-not (cj/nov--next-reading-palette "light" '("sepia" "dark" "light"))))

(ert-deftest test-nov-reading-next-palette-none-to-first ()
  "Boundary: the no-palette state (nil) cycles to the first palette."
  (should (equal (cj/nov--next-reading-palette nil '("sepia" "dark" "light"))
                 "sepia")))

(ert-deftest test-nov-reading-next-palette-unknown-current-falls-to-first ()
  "Error: an unknown current palette falls back to the first."
  (should (equal (cj/nov--next-reading-palette "gone" '("sepia" "dark" "light"))
                 "sepia")))

(provide 'test-nov-reading--palette)
;;; test-nov-reading--palette.el ends here
