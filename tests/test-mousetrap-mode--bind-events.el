;;; test-mousetrap-mode--bind-events.el --- Tests for mouse-trap--bind-events-to-ignore -*- lexical-binding: t; -*-

;;; Commentary:
;; mouse-trap--bind-events-to-ignore is the per-category binding loop extracted
;; from mouse-trap--build-keymap-1 (which previously nested it five deep).  It
;; binds a category's events, across modifier prefixes, to `ignore'.  The full
;; keymap build stays covered by test-mousetrap-mode--build-keymap.el.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mousetrap-mode)

(ert-deftest test-mousetrap-bind-events-wheel ()
  "Normal: wheel events are bound to ignore across every prefix variant."
  (let ((map (make-sparse-keymap))
        (spec '((wheel . ("wheel-up" "wheel-down")))))
    (mouse-trap--bind-events-to-ignore spec '("" "C-") map)
    (should (eq (lookup-key map (kbd "<wheel-up>")) #'ignore))
    (should (eq (lookup-key map (kbd "<C-wheel-up>")) #'ignore))
    (should (eq (lookup-key map (kbd "<wheel-down>")) #'ignore))))

(ert-deftest test-mousetrap-bind-events-click ()
  "Normal: type x button click events are bound to ignore."
  (let ((map (make-sparse-keymap))
        (spec '((types . ("mouse" "down-mouse")) (buttons . (1 3)))))
    (mouse-trap--bind-events-to-ignore spec '("") map)
    (should (eq (lookup-key map (kbd "<mouse-1>")) #'ignore))
    (should (eq (lookup-key map (kbd "<mouse-3>")) #'ignore))
    (should (eq (lookup-key map (kbd "<down-mouse-1>")) #'ignore))))

(ert-deftest test-mousetrap-bind-events-empty-spec-no-op ()
  "Boundary: a spec with neither wheel nor types/buttons binds nothing."
  (let ((map (make-sparse-keymap)))
    (mouse-trap--bind-events-to-ignore '((other . t)) '("") map)
    (should (null (lookup-key map (kbd "<mouse-1>"))))))

(provide 'test-mousetrap-mode--bind-events)
;;; test-mousetrap-mode--bind-events.el ends here
