;;; test-cj-window-toggle.el --- Tests for shared toggle-state helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj-window-toggle.el provides parameterized capture-state and
;; display-saved helpers shared by ai-vterm.el (F9) and
;; eshell-vterm-config.el (F12).  Each consumer holds its own pair of
;; state variables (last-direction symbol + last-size integer/float)
;; and passes the variable symbols to the helpers.  These tests cover
;; the helpers in isolation against a fresh pair of test-only vars.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'cj-window-toggle)

(defvar test-cj-window-toggle--last-direction nil)
(defvar test-cj-window-toggle--last-size nil)

(ert-deftest test-cj-window-toggle-capture-records-right-split ()
  "Normal: right-split window writes direction=right and integer body-cols."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right))
          (test-cj-window-toggle--last-direction nil)
          (test-cj-window-toggle--last-size nil))
      (cj/window-toggle-capture-state
       right 'right
       'test-cj-window-toggle--last-direction
       'test-cj-window-toggle--last-size)
      (should (eq test-cj-window-toggle--last-direction 'right))
      (should (integerp test-cj-window-toggle--last-size))
      (should (= test-cj-window-toggle--last-size
                 (window-body-width right))))))

(ert-deftest test-cj-window-toggle-capture-records-below-split ()
  "Normal: below-split window writes direction=below and integer body-lines."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below))
          (test-cj-window-toggle--last-direction nil)
          (test-cj-window-toggle--last-size nil))
      (cj/window-toggle-capture-state
       below 'below
       'test-cj-window-toggle--last-direction
       'test-cj-window-toggle--last-size)
      (should (eq test-cj-window-toggle--last-direction 'below))
      (should (integerp test-cj-window-toggle--last-size))
      (should (= test-cj-window-toggle--last-size
                 (window-body-height below))))))

(ert-deftest test-cj-window-toggle-capture-falls-back-to-default-direction ()
  "Boundary: window filling the frame uses the supplied default direction."
  (save-window-excursion
    (delete-other-windows)
    (let ((root (selected-window))
          (test-cj-window-toggle--last-direction nil)
          (test-cj-window-toggle--last-size nil))
      (cj/window-toggle-capture-state
       root 'below
       'test-cj-window-toggle--last-direction
       'test-cj-window-toggle--last-size)
      (should (eq test-cj-window-toggle--last-direction 'below)))))

(ert-deftest test-cj-window-toggle-capture-noop-on-nil-window ()
  "Error: nil window leaves both state vars unchanged."
  (let ((test-cj-window-toggle--last-direction 'sentinel-dir)
        (test-cj-window-toggle--last-size 0.123))
    (cj/window-toggle-capture-state
     nil 'right
     'test-cj-window-toggle--last-direction
     'test-cj-window-toggle--last-size)
    (should (eq test-cj-window-toggle--last-direction 'sentinel-dir))
    (should (= test-cj-window-toggle--last-size 0.123))))

(ert-deftest test-cj-window-toggle-capture-noop-on-deleted-window ()
  "Error: a deleted window leaves both state vars unchanged."
  (let ((test-cj-window-toggle--last-direction 'sentinel-dir)
        (test-cj-window-toggle--last-size 0.123)
        (dead (save-window-excursion
                (delete-other-windows)
                (let ((w (split-window (selected-window) nil 'right)))
                  (delete-window w)
                  w))))
    (cj/window-toggle-capture-state
     dead 'right
     'test-cj-window-toggle--last-direction
     'test-cj-window-toggle--last-size)
    (should (eq test-cj-window-toggle--last-direction 'sentinel-dir))
    (should (= test-cj-window-toggle--last-size 0.123))))

(ert-deftest test-cj-window-toggle-display-saved-uses-defaults-when-state-nil ()
  "Normal: nil state -> direction=edge of default, size=default."
  (let (received-alist
        (test-cj-window-toggle--last-direction nil)
        (test-cj-window-toggle--last-size nil))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/window-toggle-display-saved
       'fake-buf
       '((inhibit-same-window . t))
       'test-cj-window-toggle--last-direction
       'below
       'test-cj-window-toggle--last-size
       0.7))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.7))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-cj-window-toggle-display-saved-maps-below-to-bottom ()
  "Normal: saved below + integer size -> bottom edge, body-lines cons."
  (let (received-alist
        (test-cj-window-toggle--last-direction 'below)
        (test-cj-window-toggle--last-size 12))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/window-toggle-display-saved
       'fake-buf nil
       'test-cj-window-toggle--last-direction
       'below
       'test-cj-window-toggle--last-size
       0.7))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (equal (cdr (assq 'window-height received-alist))
                   '(body-lines . 12)))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-cj-window-toggle-display-saved-maps-right-to-rightmost ()
  "Normal: saved right + integer size -> rightmost edge, body-columns cons."
  (let (received-alist
        (test-cj-window-toggle--last-direction 'right)
        (test-cj-window-toggle--last-size 80))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/window-toggle-display-saved
       'fake-buf nil
       'test-cj-window-toggle--last-direction
       'right
       'test-cj-window-toggle--last-size
       0.5))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (equal (cdr (assq 'window-width received-alist))
                   '(body-columns . 80)))
    (should-not (assq 'window-height received-alist))))

(ert-deftest test-cj-window-toggle-display-saved-strips-conflicting-entries ()
  "Boundary: caller-supplied direction/size are removed; saved values win."
  (let (received-alist
        (test-cj-window-toggle--last-direction 'right)
        (test-cj-window-toggle--last-size 30))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/window-toggle-display-saved
       'fake-buf
       '((direction . above)
         (window-width . 0.2)
         (window-height . 0.3)
         (inhibit-same-window . t))
       'test-cj-window-toggle--last-direction
       'right
       'test-cj-window-toggle--last-size
       0.5))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (equal (cdr (assq 'window-width received-alist))
                   '(body-columns . 30)))
    (should-not (assq 'window-height received-alist))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-cj-window-toggle-display-saved-passes-float-size-through ()
  "Boundary: float size passes through as a fraction (no body-N wrapping)."
  (let (received-alist
        (test-cj-window-toggle--last-direction 'below)
        (test-cj-window-toggle--last-size nil))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/window-toggle-display-saved
       'fake-buf nil
       'test-cj-window-toggle--last-direction
       'below
       'test-cj-window-toggle--last-size
       0.4))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.4))))

(provide 'test-cj-window-toggle)
;;; test-cj-window-toggle.el ends here
