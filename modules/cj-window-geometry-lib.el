;;; cj-window-geometry-lib.el --- Pure window-geometry helpers -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Pure helpers for classifying a window's position in its frame and
;; computing body sizes.  Shared between `ai-vterm.el' (F9 dispatch)
;; and `vterm-config.el' (F12 dispatch); the geometry-
;; preservation pattern in both modules captures direction + body
;; size at toggle-off and replays them on the next toggle-on.
;;
;; All functions are pure: they read window/frame edges and return
;; classifications.  No side effects, no state.  Consumers wrap them
;; with consumer-specific state vars and display logic.

;;; Code:

(defun cj/window-direction (window &optional default)
  "Return the side WINDOW occupies in its frame.

Returns one of right, below, left, above.  Falls back to DEFAULT
(or right when DEFAULT is nil) when WINDOW fills its frame's
root area.  Comparison uses `frame-root-window' edges so the
minibuffer doesn't make every full-area window look like it
fails to span the full height."
  (let* ((root (frame-root-window (window-frame window)))
         (edges (window-edges window))
         (root-edges (window-edges root))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges))
         (root-left (nth 0 root-edges))
         (root-top (nth 1 root-edges))
         (root-right (nth 2 root-edges))
         (root-bottom (nth 3 root-edges))
         (spans-full-width (and (= left root-left) (= right root-right)))
         (spans-full-height (and (= top root-top) (= bottom root-bottom))))
    (cond
     ((not spans-full-width) (if (= left root-left) 'left 'right))
     ((not spans-full-height) (if (= top root-top) 'above 'below))
     (t (or default 'right)))))

(defun cj/window-body-size (window direction)
  "Return WINDOW's body size on the axis matching DIRECTION.

Returns body-width (columns) when DIRECTION is right or left.
Returns body-height (lines) when DIRECTION is below or above.

Body size, not total size, is the right thing to capture for
geometry replay: total-width includes the right-side divider when
the window has a right sibling but excludes it at the frame edge,
so a captured rightmost window replayed into a middle position
would leave the body 1 col short.  Body size is divider-
independent and matches what the user actually sees."
  (if (memq direction '(right left))
      (window-body-width window)
    (window-body-height window)))

(defun cj/cardinal-to-edge-direction (direction)
  "Map cardinal DIRECTION to its `display-buffer-in-direction' edge variant.

Returns rightmost/leftmost/bottom/top for right/left/below/above
respectively.  Returns nil for any other input.

The edge variants route splits relative to the frame's main
window rather than the selected window, so a toggle-on lands at
the same frame edge regardless of which window is selected.  The
cardinal variants would split the selected window's tree branch
instead, putting the new window mid-frame in multi-window
layouts."
  (pcase direction
    ('right 'rightmost)
    ('left 'leftmost)
    ('below 'bottom)
    ('above 'top)
    (_ nil)))

(provide 'cj-window-geometry-lib)
;;; cj-window-geometry-lib.el ends here
