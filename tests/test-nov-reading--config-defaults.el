;;; test-nov-reading--config-defaults.el --- nov reading default/order tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Asserts the shipped reading-view defaults: a fresh EPUB opens dark, and the
;; `c' cycle runs dark -> sepia -> light -> none -> back.  The pure-logic file
;; test-nov-reading--palette.el covers the cycle mechanics on fixtures; this file
;; pins the real `cj/nov-reading-palettes' / `cj/nov-reading-default-palette'
;; values so a reorder can't silently drift the default or the cycle order.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'nov-reading)

(declare-function cj/nov--next-reading-palette "nov-reading" (current names))
(defvar cj/nov-reading-palettes)
(defvar cj/nov-reading-default-palette)
(defvar cj/nov-reading-profile)
(defvar cj/nov--typography-remap-cookies)

(ert-deftest test-nov-reading-config-default-is-dark ()
  "Normal: a fresh nov buffer opens on the dark palette."
  (should (equal cj/nov-reading-default-palette "dark")))

(ert-deftest test-nov-reading-config-order-is-dark-sepia-light ()
  "Normal: the shipped palette order is dark, then sepia, then light."
  (should (equal (mapcar #'car cj/nov-reading-palettes)
                 '("dark" "sepia" "light"))))

(ert-deftest test-nov-reading-config-cycle-from-default ()
  "Normal: cycling from the default advances dark -> sepia -> light -> none -> dark."
  (let ((names (mapcar #'car cj/nov-reading-palettes)))
    (should (equal (cj/nov--next-reading-palette "dark" names) "sepia"))
    (should (equal (cj/nov--next-reading-palette "sepia" names) "light"))
    (should-not (cj/nov--next-reading-palette "light" names))
    (should (equal (cj/nov--next-reading-palette nil names) "dark"))))

(ert-deftest test-nov-reading-config-uses-reading-font-profile ()
  "Normal: nov typography names the shared Reading profile."
  (should (eq cj/nov-reading-profile 'reading)))

(ert-deftest test-nov-reading-depends-on-pure-profile-layer ()
  "Boundary: loading nov shares profile data without loading Fontaine config."
  (should (featurep 'font-profiles))
  (should-not (featurep 'font-config)))

(ert-deftest test-nov-reading-typography-remaps-shared-profile-locally ()
  "Normal: nov applies Reading locally at its own base height without stacking."
  (let ((cj/nov--typography-remap-cookies '(old-default old-fixed))
        (removed nil)
        (applied nil))
    (cl-letf (((symbol-function 'face-remap-remove-relative)
               (lambda (cookie) (push cookie removed)))
              ((symbol-function 'cj/font-profile-remap-buffer)
               (lambda (profile height)
                 (setq applied (list profile height))
                 '(new-default new-fixed))))
      (cj/nov-reading-apply-typography))
    (should (equal applied '(reading 180)))
    (should (equal (sort removed #'string-lessp)
                   '(old-default old-fixed)))
    (should (equal cj/nov--typography-remap-cookies
                   '(new-default new-fixed)))))

(provide 'test-nov-reading--config-defaults)
;;; test-nov-reading--config-defaults.el ends here
