;;; test-ui-navigation-split-follow-undo-kill.el --- Tests for the split-and-follow + undo-kill-buffer commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover `toggle-window-split' and the window-resize
;; sticky map.  This file covers the remaining three commands:
;;
;;   cj/split-and-follow-right
;;   cj/split-and-follow-below
;;   cj/undo-kill-buffer
;;
;; `split-window-*', `consult-buffer', `find-file', and `recentf'
;; primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ui-navigation)

;; Top-level defvar so let-bindings reach the dynamic var under
;; lexical scope.
(defvar recentf-mode nil)
(defvar recentf-list nil)

;;; cj/split-and-follow-right

(ert-deftest test-ui-navigation-split-right-splits-and-prompts ()
  "Normal: split-and-follow-right calls split-window-right then consult-buffer."
  (let ((sequence nil))
    (cl-letf (((symbol-function 'split-window-right)
               (lambda (&rest _) (push 'split sequence)))
              ((symbol-function 'other-window)
               (lambda (&rest _) (push 'other sequence)))
              ((symbol-function 'consult-buffer)
               (lambda (&rest _) (push 'consult sequence))))
      (cj/split-and-follow-right))
    (should (equal (reverse sequence) '(split other consult)))))

;;; cj/split-and-follow-below

(ert-deftest test-ui-navigation-split-below-splits-and-prompts ()
  "Normal: split-and-follow-below calls split-window-below then consult-buffer."
  (let ((sequence nil))
    (cl-letf (((symbol-function 'split-window-below)
               (lambda (&rest _) (push 'split sequence)))
              ((symbol-function 'other-window)
               (lambda (&rest _) (push 'other sequence)))
              ((symbol-function 'consult-buffer)
               (lambda (&rest _) (push 'consult sequence))))
      (cj/split-and-follow-below))
    (should (equal (reverse sequence) '(split other consult)))))

;;; cj/undo-kill-buffer

(ert-deftest test-ui-navigation-undo-kill-buffer-opens-most-recent ()
  "Normal: with no arg, opens the head of recentf-list that isn't currently visited."
  (let ((opened nil)
        (recentf-mode t)
        (recentf-list '("/tmp/dead.org" "/tmp/alive.txt")))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'recentf-mode) (lambda (&rest _) t))
              ;; Pretend /tmp/alive.txt is open in some buffer.
              ((symbol-function 'buffer-list)
               (lambda (&rest _)
                 (list (let ((b (get-buffer-create "*test-alive*")))
                         (with-current-buffer b
                           (setq buffer-file-name "/tmp/alive.txt"))
                         b))))
              ((symbol-function 'find-file)
               (lambda (f) (setq opened f))))
      (unwind-protect
          (cj/undo-kill-buffer 0)
        (when (get-buffer "*test-alive*") (kill-buffer "*test-alive*"))))
    (should (equal opened "/tmp/dead.org"))))

(ert-deftest test-ui-navigation-undo-kill-buffer-honors-numeric-arg ()
  "Normal: with N=1, opens the second non-visited entry from recentf-list."
  (let ((opened nil)
        (recentf-mode t)
        (recentf-list '("/tmp/a.org" "/tmp/b.org" "/tmp/c.org")))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'recentf-mode) (lambda (&rest _) t))
              ((symbol-function 'buffer-list) (lambda (&rest _) nil))
              ((symbol-function 'find-file)
               (lambda (f) (setq opened f))))
      ;; cj/undo-kill-buffer takes a prefix `arg' and indexes into the list
      ;; with `(nth arg ...)` when arg is non-nil.  Passing 1 grabs the 2nd
      ;; entry.
      (cj/undo-kill-buffer 1))
    (should (equal opened "/tmp/b.org"))))

(ert-deftest test-ui-navigation-undo-kill-buffer-no-op-when-list-empty ()
  "Boundary: when the recently-killed list is empty, find-file isn't called."
  (let ((opened nil)
        (recentf-mode t)
        (recentf-list nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'recentf-mode) (lambda (&rest _) t))
              ((symbol-function 'buffer-list) (lambda (&rest _) nil))
              ((symbol-function 'find-file)
               (lambda (f) (setq opened f))))
      (cj/undo-kill-buffer 0))
    (should-not opened)))

(provide 'test-ui-navigation-split-follow-undo-kill)
;;; test-ui-navigation-split-follow-undo-kill.el ends here
