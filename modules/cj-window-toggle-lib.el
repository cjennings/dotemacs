;;; cj-window-toggle-lib.el --- Shared toggle-state helpers for display-buffer dispatchers -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Parameterized helpers used by ai-term.el (F9) and
;; term-config.el (F12) to capture a window's geometry at
;; toggle-off and replay it on the next toggle-on.  Each consumer
;; holds its own pair of state variables (last-direction symbol +
;; last-size integer/float) and passes the variable symbols to the
;; helpers.  Both helpers are pure with respect to their arguments;
;; the side effects are confined to the named state variables.
;;
;; Pulls the geometry primitives in from cj-window-geometry-lib.el.

;;; Code:

(require 'cl-lib)
(require 'cj-window-geometry-lib)

(defun cj/window-toggle-capture-state (window default-direction
                                              direction-var size-var
                                              &optional allowed)
  "Write WINDOW's direction and body size into DIRECTION-VAR and SIZE-VAR.

DEFAULT-DIRECTION is the symbol used by `cj/window-direction' when
WINDOW fills its frame's root area.  DIRECTION-VAR and SIZE-VAR are
the symbols of the consumer's state variables; they receive the
captured values via `set'.

ALLOWED, when non-nil, is a list of permitted direction symbols.  If
WINDOW's captured direction isn't in it, fall back to DEFAULT-DIRECTION
and clear SIZE-VAR (set to nil) so the consumer's default size applies --
the captured body size was measured on the disallowed axis and wouldn't
transfer meaningfully.  A consumer that wants to forbid a placement (e.g.
an agent window that should never be remembered at the top of the frame)
passes the directions it does allow.  Omit ALLOWED to keep every
direction.

No-op when WINDOW is nil or not live."
  (when (window-live-p window)
    (let ((dir (cj/window-direction window default-direction)))
      (if (or (null allowed) (memq dir allowed))
          (progn
            (set direction-var dir)
            (set size-var (cj/window-replay-size window dir)))
        (set direction-var default-direction)
        (set size-var nil)))))

(defun cj/window-toggle-display-saved (buffer alist
                                              direction-var default-direction
                                              size-var default-size)
  "Display-buffer action: split per saved DIRECTION-VAR and SIZE-VAR.

Reads the consumer's stored direction and size through DIRECTION-VAR
and SIZE-VAR (symbols); falls back to DEFAULT-DIRECTION and
DEFAULT-SIZE when the stored values are nil.  The cardinal direction
is mapped to its frame-edge variant via
`cj/cardinal-to-edge-direction' so the new buffer always lands at
the same frame edge regardless of the selected window.  An integer
size is wrapped per axis: a width size as a `(body-columns . N)'
cons (divider-independent body width), a height size as a plain
integer total-line count.  Height uses total rather than body so the
capture/replay round-trip is immune to the mode line's pixel height
(see `cj/window-replay-size').  A float size passes through as a
fraction of the new window's parent.

Caller-supplied ALIST entries for direction, window-width, or
window-height are stripped before delegating to
`display-buffer-in-direction' so the saved-state values control
placement; the remaining alist entries are passed through."
  (let* ((stored-dir (and (boundp direction-var) (symbol-value direction-var)))
         (stored-size (and (boundp size-var) (symbol-value size-var)))
         (direction (or stored-dir default-direction))
         (edge-direction (or (cj/cardinal-to-edge-direction direction)
                             (cj/cardinal-to-edge-direction default-direction)))
         (size (or stored-size default-size))
         (width-axis (memq direction '(right left)))
         (size-key (if width-axis 'window-width 'window-height))
         ;; A width integer is a body-column count (divider-independent); a
         ;; height integer is a plain total-line count (mode-line-pixel-
         ;; independent -- see `cj/window-replay-size').  Floats pass through.
         (size-value (cond
                      ((not (integerp size)) size)
                      (width-axis (cons 'body-columns size))
                      (t size)))
         (filtered (cl-remove-if
                    (lambda (cell)
                      (memq (car-safe cell)
                            '(direction window-width window-height)))
                    alist))
         (effective (append
                     (list (cons 'direction edge-direction)
                           (cons size-key size-value))
                     filtered)))
    (display-buffer-in-direction buffer effective)))

;; --------------------------- side-window helpers ---------------------------
;;
;; A second, simpler pattern for `display-buffer-in-side-window' consumers.
;; Side windows are atomic (they can't be split), so there's no direction to
;; capture -- only a size on the side's axis.  These helpers remember that
;; size across toggles: capture the user's mouse-resized size at toggle-off,
;; replay it on the next toggle-on.  The remembered value is held in the
;; consumer's own state variable (passed by symbol) and is in-memory only.

(defun cj/side-window-capture-size (window side size-var)
  "Write WINDOW's size on SIDE's axis, as a frame fraction, into SIZE-VAR.

SIDE is a `display-buffer-in-side-window' side symbol: left or right
capture width, top or bottom capture height.  SIZE-VAR is the symbol of the
consumer's stored-fraction variable; it receives the captured value via
`set'.  Capturing at toggle-off and replaying on the next toggle-on is what
makes a manual mouse-resize stick for the rest of the session.

No-op when WINDOW is nil or not live, or when the fraction can't be computed
\(see `cj/window-size-fraction') -- SIZE-VAR keeps its prior value so the
caller's default still applies."
  (when (window-live-p window)
    (let* ((horizontal (memq side '(left right)))
           (frame (window-frame window))
           (win-size (if horizontal
                         (window-total-width window)
                       (window-total-height window)))
           (frame-size (if horizontal (frame-width frame) (frame-height frame)))
           (frac (cj/window-size-fraction win-size frame-size)))
      (when frac (set size-var frac)))))

(defun cj/side-window-display (buffer side size-var default-size)
  "Display BUFFER in a SIDE side window at the remembered or DEFAULT-SIZE size.

SIDE is a `display-buffer-in-side-window' side symbol.  SIZE-VAR is the
symbol of the consumer's stored-fraction variable; its value is used when
bound and non-nil, otherwise DEFAULT-SIZE.  The size lands under
`window-width' for a left/right side and `window-height' for top/bottom.
Returns the displayed window (or nil if display fails)."
  (let* ((stored (and (boundp size-var) (symbol-value size-var)))
         (size (or stored default-size))
         (size-key (if (memq side '(left right)) 'window-width 'window-height)))
    (display-buffer-in-side-window
     buffer (list (cons 'side side) (cons size-key size)))))

(provide 'cj-window-toggle-lib)
;;; cj-window-toggle-lib.el ends here
