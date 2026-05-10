;;; cj-window-toggle-lib.el --- Shared toggle-state helpers for display-buffer dispatchers -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Parameterized helpers used by ai-vterm.el (F9) and
;; vterm-config.el (F12) to capture a window's geometry at
;; toggle-off and replay it on the next toggle-on.  Each consumer
;; holds its own pair of state variables (last-direction symbol +
;; last-size integer/float) and passes the variable symbols to the
;; helpers.  Both helpers are pure with respect to their arguments;
;; the side effects are confined to the named state variables.
;;
;; Pulls the geometry primitives in from cj-window-geometry.el.

;;; Code:

(require 'cl-lib)
(require 'cj-window-geometry)

(defun cj/window-toggle-capture-state (window default-direction
                                              direction-var size-var)
  "Write WINDOW's direction and body size into DIRECTION-VAR and SIZE-VAR.

DEFAULT-DIRECTION is the symbol used by `cj/window-direction' when
WINDOW fills its frame's root area.  DIRECTION-VAR and SIZE-VAR are
the symbols of the consumer's state variables; they receive the
captured values via `set'.

No-op when WINDOW is nil or not live."
  (when (window-live-p window)
    (let* ((dir (cj/window-direction window default-direction))
           (size (cj/window-body-size window dir)))
      (set direction-var dir)
      (set size-var size))))

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
size is wrapped in a `(body-columns . N)' / `(body-lines . N)' cons
so `display-buffer-in-direction' sets the body explicitly,
divider-independent.  A float size passes through as a fraction of
the new window's parent.

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
         (size-key (if (memq direction '(right left))
                       'window-width
                     'window-height))
         (body-tag (if (memq direction '(right left))
                       'body-columns
                     'body-lines))
         (size-value (if (integerp size)
                         (cons body-tag size)
                       size))
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

(provide 'cj-window-toggle-lib)
;;; cj-window-toggle-lib.el ends here
