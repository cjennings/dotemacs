;;; test-ui-navigation--toggle-window-split.el --- Tests for toggle-window-split -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `toggle-window-split' in ui-navigation.el.
;;
;; The function flips a 2-window split between horizontal and vertical
;; orientation.  Historically it failed when one of the two windows was
;; marked strongly-dedicated (e.g. *Org Agenda*) -- `set-window-buffer'
;; rejects buffer swaps on strongly-dedicated windows, so the
;; non-dedicated buffer never made it across and both windows ended up
;; showing the dedicated one.  The fix clears dedicated on both windows
;; before the swap.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ui-navigation)

(defmacro test-toggle--with-buffers (&rest body)
  "Bind `buf-a' and `buf-b' to fresh buffers around BODY, killing both on exit.
The buffers are visible to BODY by name so it can pass them into the
window-setup macro without re-binding."
  (declare (indent 0))
  `(let ((buf-a (generate-new-buffer "*test-toggle-a*"))
         (buf-b (generate-new-buffer "*test-toggle-b*")))
     (unwind-protect
         (progn ,@body)
       (kill-buffer buf-a)
       (kill-buffer buf-b))))

(defmacro test-toggle--with-two-windows (buf-a buf-b split-fn &rest body)
  "Set up two windows displaying BUF-A and BUF-B via SPLIT-FN, run BODY.
BUF-A goes in the top/left window, BUF-B in the bottom/right.  Point
ends in the BUF-B window -- the 2nd / lower-or-right one -- because
that's the position that triggers the dedicated-window bug.  The
original repro is F8-then-toggle, which leaves selection in the agenda
(below the source buffer) when the toggle runs.  Restores the outer
window config when BODY returns.  SPLIT-FN is `split-window-below' or
`split-window-right'."
  (declare (indent 3))
  `(save-window-excursion
     (delete-other-windows)
     (set-window-buffer (selected-window) ,buf-a)
     (funcall ,split-fn)
     (other-window 1)
     (set-window-buffer (selected-window) ,buf-b)
     ,@body))

;;; Normal Cases

(ert-deftest test-ui-navigation-toggle-split-normal-no-dedicated-keeps-both-buffers ()
  "Normal: vertical split, no dedicated -- toggle preserves both buffers."
  (test-toggle--with-buffers
    (test-toggle--with-two-windows buf-a buf-b #'split-window-below
      (toggle-window-split)
      (let ((bufs (mapcar #'window-buffer (window-list))))
        (should (memq buf-a bufs))
        (should (memq buf-b bufs))))))

(ert-deftest test-ui-navigation-toggle-split-normal-dedicated-selected-window-keeps-both-buffers ()
  "Normal: selected window dedicated -- toggle still distributes both buffers.
This is the bug scenario from the *Org Agenda* report: when the
selected window is strongly-dedicated, the internal
`set-window-buffer' for the non-matching buffer is rejected and both
windows end up showing the dedicated buffer.  After the fix, dedicated
is cleared first and both buffers reach their target windows."
  (test-toggle--with-buffers
    (test-toggle--with-two-windows buf-a buf-b #'split-window-below
      (set-window-dedicated-p (selected-window) t)
      (toggle-window-split)
      (let ((bufs (mapcar #'window-buffer (window-list))))
        (should (memq buf-a bufs))
        (should (memq buf-b bufs))))))

(ert-deftest test-ui-navigation-toggle-split-normal-clears-dedicated-state ()
  "Normal: dedicated state is cleared after toggle.
The user invoked a layout change; preserving per-window dedicated
through the swap would re-introduce the same wedge on the next toggle."
  (test-toggle--with-buffers
    (test-toggle--with-two-windows buf-a buf-b #'split-window-below
      (set-window-dedicated-p (selected-window) t)
      (toggle-window-split)
      (dolist (w (window-list))
        (should-not (window-dedicated-p w))))))

;;; Boundary Cases

(ert-deftest test-ui-navigation-toggle-split-boundary-single-window-noop ()
  "Boundary: a single window in the frame -- toggle is a no-op."
  (save-window-excursion
    (delete-other-windows)
    (toggle-window-split)
    (should (= 1 (length (window-list))))))

(ert-deftest test-ui-navigation-toggle-split-boundary-three-windows-noop ()
  "Boundary: three windows in the frame -- toggle is a no-op."
  (test-toggle--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (set-window-buffer (selected-window) buf-a)
      (split-window-below)
      (split-window-right)
      (let ((before-count (length (window-list))))
        (toggle-window-split)
        (should (= before-count (length (window-list))))))))

(provide 'test-ui-navigation--toggle-window-split)
;;; test-ui-navigation--toggle-window-split.el ends here
