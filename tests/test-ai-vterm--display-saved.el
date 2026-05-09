;;; test-ai-vterm--display-saved.el --- Tests for the display-saved action -*- lexical-binding: t; -*-

;;; Commentary:
;; The action reads `cj/--ai-vterm-last-direction' +
;; `cj/--ai-vterm-last-size' (with default fallbacks), builds an
;; alist with direction + the matching size key, strips any
;; conflicting entries that came in via the rule, and delegates to
;; `display-buffer-in-direction'.
;;
;; Tests stub `display-buffer-in-direction' to capture the alist
;; that would have reached it.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--display-saved-uses-defaults-when-state-nil ()
  "Normal: nil state -> direction=right, size=cj/ai-vterm-window-width."
  (let (received-buf received-alist
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil)
        (cj/ai-vterm-window-width 0.5))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (b a)
                 (setq received-buf b received-alist a)
                 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq received-buf 'fake-buf))
    (should (eq (cdr (assq 'direction received-alist)) 'right))
    (should (= (cdr (assq 'window-width received-alist)) 0.5))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-ai-vterm--display-saved-uses-saved-direction-and-size-below ()
  "Normal: saved direction=below, size=0.4 -> below + window-height 0.4."
  (let (received-alist
        (cj/--ai-vterm-last-direction 'below)
        (cj/--ai-vterm-last-size 0.4))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'below))
    (should (= (cdr (assq 'window-height received-alist)) 0.4))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-ai-vterm--display-saved-uses-saved-direction-and-size-right ()
  "Normal: saved direction=right, size=0.7 -> right + window-width 0.7."
  (let (received-alist
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'right))
    (should (= (cdr (assq 'window-width received-alist)) 0.7))
    (should-not (assq 'window-height received-alist))))

(ert-deftest test-ai-vterm--display-saved-strips-conflicting-alist-entries ()
  "Boundary: caller-supplied direction/size are stripped, saved values win."
  (let (received-alist
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved
       'fake-buf
       '((direction . below)
         (window-width . 0.2)
         (window-height . 0.3)
         (inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'right))
    (should (= (cdr (assq 'window-width received-alist)) 0.7))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))
    ;; window-height should not be in the alist when direction is right
    ;; -- the action picks the matching size key based on direction.
    (let ((wh-cells (cl-remove-if-not
                     (lambda (cell) (eq (car-safe cell) 'window-height))
                     received-alist)))
      (should (null wh-cells)))))

(ert-deftest test-ai-vterm--display-saved-passes-buffer-through ()
  "Normal: BUFFER argument reaches display-buffer-in-direction unchanged."
  (let (received-buf
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 0.5))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (b _a) (setq received-buf b) 'fake-window)))
      (cj/--ai-vterm-display-saved 'sentinel-buffer nil))
    (should (eq received-buf 'sentinel-buffer))))

(provide 'test-ai-vterm--display-saved)
;;; test-ai-vterm--display-saved.el ends here
