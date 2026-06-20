;;; test-ui-navigation--window-resize.el --- Tests for the C-; b <arrow> resize keys -*- lexical-binding: t; -*-

;;; Commentary:
;; `C-; b <left>/<right>/<up>/<down>' moves the active window's divider in the
;; arrow's direction (via `windsize'), then keeps `cj/window-resize-map' active
;; so bare arrows keep nudging until any other key.  We own the dispatch
;; (`cj/window-resize-sticky' -- pick the matching `windsize-*' command, run it,
;; arm the loop) and the keymaps; the resize math is `windsize's job and isn't
;; tested here.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ui-navigation)

(ert-deftest test-ui-navigation-window-resize-map-bindings ()
  "Normal: the sticky-resize map maps the four arrows to the `windsize' commands."
  (should (eq (keymap-lookup cj/window-resize-map "<left>")  #'windsize-left))
  (should (eq (keymap-lookup cj/window-resize-map "<right>") #'windsize-right))
  (should (eq (keymap-lookup cj/window-resize-map "<up>")    #'windsize-up))
  (should (eq (keymap-lookup cj/window-resize-map "<down>")  #'windsize-down)))

(ert-deftest test-ui-navigation-window-resize-sticky-dispatches-and-arms ()
  "Normal: with more than one window, `cj/window-resize-sticky' runs the
`windsize' command matching the arrow key that triggered it, then arms the
sticky-repeat map.  `one-window-p' is forced nil so the resize path is taken
deterministically -- in `--batch' the sole frame is one-window-p, which would
otherwise route to the pull-away path."
  (dolist (case '((left  . windsize-left)
                  (right . windsize-right)
                  (up    . windsize-up)
                  (down  . windsize-down)))
    (let ((ran nil)
          (overriding-terminal-local-map nil)
          (pre-command-hook nil))
      (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) nil))
                ((symbol-function (cdr case))
                 (lambda (&rest _) (interactive) (setq ran t))))
        (let ((last-command-event (car case)))
          (cj/window-resize-sticky)))
      (should ran)                              ; dispatched to the right command
      (should overriding-terminal-local-map)))) ; loop armed

(ert-deftest test-ui-navigation-window-arrow-direction ()
  "Normal/Error: each arrow maps to its split direction; anything else is nil."
  (should (eq (cj/window-arrow-direction "<left>")  'left))
  (should (eq (cj/window-arrow-direction "<right>") 'right))
  (should (eq (cj/window-arrow-direction "<up>")    'above))
  (should (eq (cj/window-arrow-direction "<down>")  'below))
  (should (null (cj/window-arrow-direction "<prior>")))
  (should (null (cj/window-arrow-direction "x"))))

(ert-deftest test-ui-navigation-window-resize-sticky-sole-window-pulls-away ()
  "Normal: with a single window, the arrow pulls a window away toward its
direction (via `cj/window--pull-away') rather than resizing, then arms the
loop.  `cj/window--pull-away' is stubbed to capture the direction so no real
window split happens under `--batch'."
  (dolist (case '((left  . left)
                  (right . right)
                  (up    . above)
                  (down  . below)))
    (let ((pulled nil)
          (overriding-terminal-local-map nil)
          (pre-command-hook nil))
      (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t))
                ((symbol-function 'cj/window--pull-away)
                 (lambda (dir) (setq pulled dir))))
        (let ((last-command-event (car case)))
          (cj/window-resize-sticky)))
      (should (eq pulled (cdr case)))           ; pulled toward the arrow
      (should overriding-terminal-local-map)))) ; loop armed

(ert-deftest test-ui-navigation-window-resize-bound-under-c-semicolon-b ()
  "Normal: `C-; b <arrow>' (each direction) reaches the sticky-resize command."
  (require 'custom-buffer-file)
  (dolist (arrow '("<left>" "<right>" "<up>" "<down>"))
    (should (eq (keymap-lookup cj/buffer-and-file-map arrow)
                #'cj/window-resize-sticky))))

(provide 'test-ui-navigation--window-resize)
;;; test-ui-navigation--window-resize.el ends here
