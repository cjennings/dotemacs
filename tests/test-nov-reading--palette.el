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
(declare-function cj/nov--reading-palette-plist "nov-reading" (name))
(declare-function cj/nov--next-reading-palette "nov-reading" (current names))
(defvar cj/nov-reading-palettes)

;; Each palette entry is a property list: :face supplies bg/fg, :heading and
;; :link recolor shr's heading/link faces.  Structural keys are optional.
(defconst test-nov-reading--palettes
  '(("sepia" :face cj/nov-reading-sepia
             :heading cj/nov-reading-sepia-heading
             :link cj/nov-reading-sepia-link)
    ("dark"  :face cj/nov-reading-dark))
  "Bundle-shaped palette fixture: sepia carries structural faces, dark omits them.")

;;; ----------------------- cj/nov--reading-palette-face -----------------------

(ert-deftest test-nov-reading-palette-face-known ()
  "Normal: a known palette name resolves to its :face."
  (let ((cj/nov-reading-palettes test-nov-reading--palettes))
    (should (eq (cj/nov--reading-palette-face "sepia") 'cj/nov-reading-sepia))
    (should (eq (cj/nov--reading-palette-face "dark") 'cj/nov-reading-dark))))

(ert-deftest test-nov-reading-palette-face-unknown ()
  "Error: an unknown name resolves to nil."
  (let ((cj/nov-reading-palettes test-nov-reading--palettes))
    (should-not (cj/nov--reading-palette-face "nope"))))

(ert-deftest test-nov-reading-palette-face-nil ()
  "Boundary: a nil name resolves to nil."
  (let ((cj/nov-reading-palettes test-nov-reading--palettes))
    (should-not (cj/nov--reading-palette-face nil))))

;;; ---------------------- cj/nov--reading-palette-plist -----------------------

(ert-deftest test-nov-reading-palette-plist-structural-faces ()
  "Normal: a palette's :heading and :link faces are retrievable from its plist."
  (let ((cj/nov-reading-palettes test-nov-reading--palettes))
    (should (eq (plist-get (cj/nov--reading-palette-plist "sepia") :heading)
                'cj/nov-reading-sepia-heading))
    (should (eq (plist-get (cj/nov--reading-palette-plist "sepia") :link)
                'cj/nov-reading-sepia-link))))

(ert-deftest test-nov-reading-palette-plist-omitted-structural ()
  "Boundary: a palette that omits structural keys yields nil for them."
  (let ((cj/nov-reading-palettes test-nov-reading--palettes))
    (should (eq (plist-get (cj/nov--reading-palette-plist "dark") :face)
                'cj/nov-reading-dark))
    (should-not (plist-get (cj/nov--reading-palette-plist "dark") :heading))
    (should-not (plist-get (cj/nov--reading-palette-plist "dark") :link))))

(ert-deftest test-nov-reading-palette-plist-unknown ()
  "Error: an unknown palette name yields a nil plist."
  (let ((cj/nov-reading-palettes test-nov-reading--palettes))
    (should-not (cj/nov--reading-palette-plist "nope"))))

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
