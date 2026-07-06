;;; test-pdf-config--reading-palette.el --- pdf reading-palette tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-logic tests for the pdf-view reading-palette layer: name->colors
;; resolution, the shipped default and order, and the cycle (palettes, then the
;; no-palette state, wrapping).  The live application (which drives
;; `pdf-view-midnight-minor-mode') is exercised in the daemon, not here.
;;
;; Requires pdf-config, which uses use-package for pdf-tools; `make test' runs
;; without the implicit package-initialize, so bootstrap `package' first (the
;; shared format-wiring helper).  pdf-tools is loaded lazily (:defer t), so this
;; only needs the pure top-level helpers, not the pdf-view runtime.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
(require 'pdf-config)

(declare-function cj/pdf--reading-palette-colors "pdf-config" (name))
(declare-function cj/pdf--next-reading-palette "pdf-config" (current names))
(defvar cj/pdf-reading-palettes)
(defvar cj/pdf-reading-default-palette)

;;; ------------------------ cj/pdf--reading-palette-colors --------------------

(ert-deftest test-pdf-reading-palette-colors-known ()
  "Normal: a known palette resolves to a (FG . BG) cons of two strings."
  (let ((colors (cj/pdf--reading-palette-colors "dark")))
    (should (consp colors))
    (should (stringp (car colors)))
    (should (stringp (cdr colors)))))

(ert-deftest test-pdf-reading-palette-colors-unknown ()
  "Error: an unknown palette name resolves to nil."
  (should-not (cj/pdf--reading-palette-colors "nope")))

(ert-deftest test-pdf-reading-palette-colors-nil ()
  "Boundary: a nil name (the no-palette state) resolves to nil."
  (should-not (cj/pdf--reading-palette-colors nil)))

;;; --------------------------- shipped default + order ------------------------

(ert-deftest test-pdf-reading-default-is-dark ()
  "Normal: a fresh PDF opens on the dark palette."
  (should (equal cj/pdf-reading-default-palette "dark")))

(ert-deftest test-pdf-reading-order-is-dark-sepia-light ()
  "Normal: the shipped palette order is dark, then sepia, then light."
  (should (equal (mapcar #'car cj/pdf-reading-palettes)
                 '("dark" "sepia" "light"))))

;;; ------------------------- cj/pdf--next-reading-palette ---------------------

(ert-deftest test-pdf-reading-next-palette-advances ()
  "Normal: cycles to the next palette in order."
  (should (equal (cj/pdf--next-reading-palette "dark" '("dark" "sepia" "light"))
                 "sepia")))

(ert-deftest test-pdf-reading-next-palette-last-to-none ()
  "Boundary: the last palette cycles to the no-palette state (nil)."
  (should-not (cj/pdf--next-reading-palette "light" '("dark" "sepia" "light"))))

(ert-deftest test-pdf-reading-next-palette-none-to-first ()
  "Boundary: the no-palette state (nil) cycles to the first palette."
  (should (equal (cj/pdf--next-reading-palette nil '("dark" "sepia" "light"))
                 "dark")))

(ert-deftest test-pdf-reading-next-palette-unknown-current-falls-to-first ()
  "Error: an unknown current palette falls back to the first."
  (should (equal (cj/pdf--next-reading-palette "gone" '("dark" "sepia" "light"))
                 "dark")))

(ert-deftest test-pdf-reading-cycle-from-default ()
  "Normal: cycling from the default advances dark -> sepia -> light -> none -> dark."
  (let ((names (mapcar #'car cj/pdf-reading-palettes)))
    (should (equal (cj/pdf--next-reading-palette "dark" names) "sepia"))
    (should (equal (cj/pdf--next-reading-palette "sepia" names) "light"))
    (should-not (cj/pdf--next-reading-palette "light" names))
    (should (equal (cj/pdf--next-reading-palette nil names) "dark"))))

(provide 'test-pdf-config--reading-palette)
;;; test-pdf-config--reading-palette.el ends here
