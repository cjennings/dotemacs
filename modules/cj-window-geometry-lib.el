;;; cj-window-geometry-lib.el --- Pure window-geometry helpers -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Pure helpers for classifying a window's position in its frame and
;; computing body sizes.  Shared between `ai-term.el' (F9 dispatch)
;; and `term-config.el' (F12 dispatch); the geometry-
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

(defun cj/window-at-edge (direction &optional frame)
  "Return the window forming FRAME's half on the side named by DIRECTION.

DIRECTION is one of right, left, below, above.  A window qualifies as
the half when it sits flush against that frame edge and spans the full
perpendicular extent: for right or left it is a full-height side column
(nothing above, below, or further toward the edge, but a sibling on the
far side); for below or above it is a full-width row.  FRAME defaults to
the selected frame.

Existence is tested with `window-in-direction' rather than raw edge
arithmetic -- frame-internal geometry makes the root window's edges
disagree with its children's by a row, so an `=' comparison against the
root edges never matches.  Asking \"is there a window on each side?\"
sidesteps that.

Returns nil when no window qualifies: a single-window frame (no sibling
on the far side, so not a distinct half), an axis mismatch (a top/bottom
split when DIRECTION is right has no full-height right column), or a
nested edge no one window fills.  The caller then falls back to splitting
a fresh half."
  (when (memq direction '(right left below above))
    (let ((far (pcase direction
                 ('right 'left) ('left 'right) ('below 'above) ('above 'below)))
          (perp (pcase direction
                  ((or 'right 'left) '(above below))
                  ((or 'below 'above) '(left right)))))
      (seq-find
       (lambda (w)
         (and (window-in-direction far w)
              (not (window-in-direction direction w))
              (not (window-in-direction (nth 0 perp) w))
              (not (window-in-direction (nth 1 perp) w))))
       (window-list (or frame (selected-frame)) 'never)))))

(defun cj/window-size-fraction (window-size frame-size &optional min-frac max-frac)
  "Return WINDOW-SIZE as a fraction of FRAME-SIZE, clamped to [MIN-FRAC, MAX-FRAC].

WINDOW-SIZE and FRAME-SIZE are line or column counts.  MIN-FRAC and
MAX-FRAC default to 0.05 and 0.95: a side window pinned to either extreme
is almost certainly a mistake, and a 0.0 fraction makes
`display-buffer-in-side-window' unusable.  Returns nil when FRAME-SIZE is
not a positive number, or WINDOW-SIZE is not a number, so a caller can
fall back to its own default instead of dividing by zero.

This is the kernel for remembering a side window's user-resized size: capture
the fraction at toggle-off, replay it on the next toggle-on."
  (when (and (numberp window-size) (numberp frame-size) (> frame-size 0))
    (let ((lo (or min-frac 0.05))
          (hi (or max-frac 0.95)))
      (max lo (min hi (/ (float window-size) frame-size))))))

(defcustom cj/window-dock-min-columns 80
  "Minimum body columns each pane must keep for a side-by-side dock.

`cj/preferred-dock-direction' docks a companion panel as a side-by-side
column only when both the panel and the main window would stay at least
this wide; otherwise it stacks the panel below.  80 is the classic
terminal/code width."
  :type 'integer
  :group 'windows)

(defun cj/preferred-dock-direction (frame-cols fraction &optional min-cols)
  "Return the dock direction for a companion panel beside the main window.

Returns `right' (a side-by-side column) when a split that gives the panel
FRACTION of FRAME-COLS would leave both panes at least MIN-COLS columns
wide; otherwise `below' (a stacked panel).  FRAME-COLS is the frame's
total column count; FRACTION is the panel's share of the width, in the
open interval (0, 1).  MIN-COLS defaults to `cj/window-dock-min-columns'.

The narrower of the two resulting panes governs: the panel takes
round(FRACTION * FRAME-COLS) columns, the main window takes the rest less
one divider column, and `right' is returned only when the smaller of the
two clears MIN-COLS.  Returns `below' for degenerate input (non-positive
FRAME-COLS, or FRACTION outside (0, 1)) so a caller always gets a usable
stacked fallback."
  (let ((min-cols (or min-cols cj/window-dock-min-columns)))
    (if (and (numberp frame-cols) (> frame-cols 0)
             (numberp fraction) (< 0 fraction 1))
        (let* ((panel (round (* fraction frame-cols)))
               (main (- frame-cols panel 1))
               (narrower (min panel main)))
          (if (>= narrower min-cols) 'right 'below))
      'below)))

(provide 'cj-window-geometry-lib)
;;; cj-window-geometry-lib.el ends here
