;;; test-host-environment--display-predicates.el --- Tests for env-x/x11/wayland/terminal/gui-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the display-environment predicates in host-environment.el.
;; Mocks `window-system', `getenv', and `display-graphic-p' at the
;; framework boundary so each predicate can be exercised under every
;; relevant combination of (window-system, WAYLAND_DISPLAY, graphic-p).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'host-environment)

(defmacro test-host-env--with-display (window-system-value
                                       wayland-display
                                       graphic-p
                                       &rest body)
  "Run BODY with display state stubbed.
WINDOW-SYSTEM-VALUE becomes the return of `(window-system)'.
WAYLAND-DISPLAY becomes the value of $WAYLAND_DISPLAY (nil for unset).
GRAPHIC-P becomes the return of `(display-graphic-p)'."
  (declare (indent 3) (debug t))
  `(cl-letf (((symbol-function 'window-system)
              (lambda (&optional _) ,window-system-value))
             ((symbol-function 'getenv)
              (lambda (name)
                (when (string= name "WAYLAND_DISPLAY") ,wayland-display)))
             ((symbol-function 'display-graphic-p)
              (lambda (&optional _) ,graphic-p)))
     ,@body))

;;; env-x-p

(ert-deftest test-host-environment-x-p-true-when-window-system-is-x ()
  "Normal: env-x-p returns t when window-system is `x'."
  (test-host-env--with-display 'x nil t
    (should (env-x-p))))

(ert-deftest test-host-environment-x-p-false-when-window-system-is-pgtk ()
  "Boundary: env-x-p returns nil when window-system is `pgtk'."
  (test-host-env--with-display 'pgtk nil t
    (should-not (env-x-p))))

(ert-deftest test-host-environment-x-p-false-when-no-window-system ()
  "Boundary: env-x-p returns nil under no window system (terminal)."
  (test-host-env--with-display nil nil nil
    (should-not (env-x-p))))

;;; env-x11-p

(ert-deftest test-host-environment-x11-p-true-on-x-without-wayland-display ()
  "Normal: env-x11-p returns t under X with no WAYLAND_DISPLAY."
  (test-host-env--with-display 'x nil t
    (should (env-x11-p))))

(ert-deftest test-host-environment-x11-p-false-under-xwayland ()
  "Boundary: env-x11-p returns nil when running through XWayland.
Detected via WAYLAND_DISPLAY being set even though window-system is `x'."
  (test-host-env--with-display 'x "wayland-0" t
    (should-not (env-x11-p))))

(ert-deftest test-host-environment-x11-p-false-on-pgtk ()
  "Boundary: env-x11-p returns nil under pure-GTK Wayland."
  (test-host-env--with-display 'pgtk "wayland-0" t
    (should-not (env-x11-p))))

;;; env-wayland-p

(ert-deftest test-host-environment-wayland-p-true-when-wayland-display-set ()
  "Normal: env-wayland-p returns t when WAYLAND_DISPLAY is set."
  (test-host-env--with-display 'pgtk "wayland-0" t
    (should (env-wayland-p))))

(ert-deftest test-host-environment-wayland-p-false-when-wayland-display-unset ()
  "Boundary: env-wayland-p returns nil when WAYLAND_DISPLAY is unset."
  (test-host-env--with-display 'x nil t
    (should-not (env-wayland-p))))

(ert-deftest test-host-environment-wayland-p-true-under-xwayland ()
  "Boundary: env-wayland-p returns t even under XWayland.
WAYLAND_DISPLAY is set by the compositor regardless of which X surface
Emacs uses, so XWayland counts as Wayland by this predicate."
  (test-host-env--with-display 'x "wayland-0" t
    (should (env-wayland-p))))

;;; env-terminal-p / env-gui-p

(ert-deftest test-host-environment-terminal-p-true-when-not-graphical ()
  "Normal: env-terminal-p returns t when display-graphic-p is nil."
  (test-host-env--with-display nil nil nil
    (should (env-terminal-p))))

(ert-deftest test-host-environment-terminal-p-false-when-graphical ()
  "Boundary: env-terminal-p returns nil under a graphical frame."
  (test-host-env--with-display 'x nil t
    (should-not (env-terminal-p))))

(ert-deftest test-host-environment-gui-p-true-when-graphical ()
  "Normal: env-gui-p returns t under a graphical frame."
  (test-host-env--with-display 'x nil t
    (should (env-gui-p))))

(ert-deftest test-host-environment-gui-p-false-in-terminal ()
  "Boundary: env-gui-p returns nil under a terminal frame."
  (test-host-env--with-display nil nil nil
    (should-not (env-gui-p))))

(provide 'test-host-environment--display-predicates)
;;; test-host-environment--display-predicates.el ends here
