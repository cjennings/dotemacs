;;; test-vterm-toggle--display.el --- Tests for the F12 display-saved action -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the F12-side equivalents of the ai-vterm display tests:
;; geometry capture (window-direction, window-size with 'below
;; default), capture-state writing module-level vars, and the custom
;; display action mapping cardinal -> edge directions.  Tests stub
;; `display-buffer-in-direction' to capture the alist that would
;; have reached it.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'vterm-config)

(ert-deftest test-vterm-toggle--capture-state-records-direction-and-size ()
  "Normal: capture-state writes direction and integer body size."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below))
          (cj/--vterm-toggle-last-direction nil)
          (cj/--vterm-toggle-last-size nil))
      (cj/--vterm-toggle-capture-state below)
      (should (eq cj/--vterm-toggle-last-direction 'below))
      (should (integerp cj/--vterm-toggle-last-size))
      (should (= cj/--vterm-toggle-last-size (window-body-height below))))))

(ert-deftest test-vterm-toggle--capture-state-noop-on-dead-window ()
  "Boundary: nil window -> state remains unchanged."
  (let ((cj/--vterm-toggle-last-direction 'sentinel)
        (cj/--vterm-toggle-last-size 0.123))
    (cj/--vterm-toggle-capture-state nil)
    (should (eq cj/--vterm-toggle-last-direction 'sentinel))
    (should (= cj/--vterm-toggle-last-size 0.123))))

(ert-deftest test-vterm-toggle--display-saved-defaults-when-state-nil ()
  "Normal: nil state -> direction=bottom, size=cj/vterm-toggle-window-height."
  (let (received-alist
        (cj/--vterm-toggle-last-direction nil)
        (cj/--vterm-toggle-last-size nil)
        (cj/vterm-toggle-window-height 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--vterm-toggle-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.7))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-vterm-toggle--display-saved-maps-cardinal-to-edge ()
  "Normal: saved 'below maps to bottom edge; integer size wraps in body-lines."
  (let (received-alist
        (cj/--vterm-toggle-last-direction 'below)
        (cj/--vterm-toggle-last-size 12))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--vterm-toggle-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (equal (cdr (assq 'window-height received-alist))
                   '(body-lines . 12)))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-vterm-toggle--display-saved-strips-conflicting-alist-entries ()
  "Boundary: caller-supplied direction/size are stripped, saved values win."
  (let (received-alist
        (cj/--vterm-toggle-last-direction 'right)
        (cj/--vterm-toggle-last-size 30))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--vterm-toggle-display-saved
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

(provide 'test-vterm-toggle--display)
;;; test-vterm-toggle--display.el ends here
