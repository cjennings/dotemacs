;;; test-term-toggle--display.el --- Tests for the F12 display-saved action -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the F12-side equivalents of the ai-term display tests:
;; geometry capture (window-direction, window-size with 'below
;; default), capture-state writing module-level vars, and the custom
;; display action mapping cardinal -> edge directions.  Tests stub
;; `display-buffer-in-direction' to capture the alist that would
;; have reached it.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eat-config)

(ert-deftest test-term-toggle--capture-state-records-direction-and-size ()
  "Normal: capture-state writes direction and integer size.
The vertical axis captures total-height (not body-height) so the toggle
round-trip is immune to the mode line's pixel height."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below))
          (cj/--term-toggle-last-direction nil)
          (cj/--term-toggle-last-size nil))
      (cj/--term-toggle-capture-state below)
      (should (eq cj/--term-toggle-last-direction 'below))
      (should (integerp cj/--term-toggle-last-size))
      (should (= cj/--term-toggle-last-size (window-total-height below))))))

(ert-deftest test-term-toggle--capture-state-noop-on-dead-window ()
  "Boundary: nil window -> state remains unchanged."
  (let ((cj/--term-toggle-last-direction 'sentinel)
        (cj/--term-toggle-last-size 0.123))
    (cj/--term-toggle-capture-state nil)
    (should (eq cj/--term-toggle-last-direction 'sentinel))
    (should (= cj/--term-toggle-last-size 0.123))))

(ert-deftest test-term-toggle--display-saved-defaults-when-state-nil ()
  "Normal: nil state -> direction=bottom, size=cj/term-toggle-window-height."
  (let (received-alist
        (cj/--term-toggle-last-direction nil)
        (cj/--term-toggle-last-size nil)
        (cj/term-toggle-window-height 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--term-toggle-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.7))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-term-toggle--display-saved-maps-cardinal-to-edge ()
  "Normal: saved 'below maps to bottom edge; integer size is a plain total-line count.
The height axis replays a total-line integer (not a body-lines cons) so the
round-trip is immune to the mode line's pixel height."
  (let (received-alist
        (cj/--term-toggle-last-direction 'below)
        (cj/--term-toggle-last-size 12))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--term-toggle-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (equal (cdr (assq 'window-height received-alist)) 12))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-term-toggle--display-saved-strips-conflicting-alist-entries ()
  "Boundary: caller-supplied direction/size are stripped, saved values win."
  (let (received-alist
        (cj/--term-toggle-last-direction 'right)
        (cj/--term-toggle-last-size 30))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--term-toggle-display-saved
       'fake-buf
       '((direction . above)
         (window-width . 0.2)
         (window-height . 0.3)
         (inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (equal (cdr (assq 'window-width received-alist))
                   '(body-columns . 30)))
    (let ((wh-cells (cl-remove-if-not
                     (lambda (cell) (eq (car-safe cell) 'window-height))
                     received-alist)))
      (should (null wh-cells)))))

(ert-deftest test-term-toggle--default-size-pairs-width-with-right ()
  "Normal: the default size for `right' is the width fraction."
  (let ((cj/term-toggle-window-width 0.5)
        (cj/term-toggle-window-height 0.7))
    (should (= (cj/--term-toggle-default-size 'right) 0.5))))

(ert-deftest test-term-toggle--default-size-pairs-height-with-below ()
  "Normal: the default size for `below' is the height fraction."
  (let ((cj/term-toggle-window-width 0.5)
        (cj/term-toggle-window-height 0.7))
    (should (= (cj/--term-toggle-default-size 'below) 0.7))))

(ert-deftest test-term-toggle--default-direction-delegates-to-dock-rule ()
  "Normal: default-direction passes the width fraction to the dock rule."
  (let ((cj/term-toggle-window-width 0.5)
        captured)
    (cl-letf (((symbol-function 'cj/preferred-dock-direction)
               (lambda (cols frac &rest _)
                 (setq captured (list cols frac))
                 'right)))
      (should (eq (cj/--term-toggle-default-direction) 'right))
      (should (= (nth 1 captured) 0.5))
      (should (integerp (nth 0 captured))))))

(provide 'test-term-toggle--display)
;;; test-term-toggle--display.el ends here
