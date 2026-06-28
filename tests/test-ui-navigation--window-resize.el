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

(ert-deftest test-ui-navigation-window-pull-side ()
  "Normal/Error: each arrow maps to the *opposite* side (where the revealed
window opens, so the current window keeps the arrow's edge); anything else
is nil."
  (should (eq (cj/window-pull-side "<down>")  'above))
  (should (eq (cj/window-pull-side "<up>")    'below))
  (should (eq (cj/window-pull-side "<left>")  'right))
  (should (eq (cj/window-pull-side "<right>") 'left))
  (should (null (cj/window-pull-side "<prior>")))
  (should (null (cj/window-pull-side "x"))))

(ert-deftest test-ui-navigation-window-resize-sticky-sole-window-pulls-away ()
  "Normal: with a single window, the arrow pulls a sliver away on the side
opposite the arrow (via `cj/window--pull-away') rather than resizing, then
arms the loop.  `cj/window--pull-away' is stubbed to capture the side so no
real window split happens under `--batch'."
  (dolist (case '((down  . above)
                  (up    . below)
                  (left  . right)
                  (right . left)))
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

(ert-deftest test-ui-navigation-window-resize-sticky-meta-arrow-pulls-away ()
  "Normal: M-<arrow> reaches the same pull-away as the bare arrow.  The
direction is derived with `event-basic-type', so the Meta modifier is stripped
and a sole window pulls a sliver to the side opposite the arrow, exactly as the
bare-arrow path does."
  (dolist (case '((M-down  . above)
                  (M-up    . below)
                  (M-left  . right)
                  (M-right . left)))
    (let ((pulled nil)
          (overriding-terminal-local-map nil)
          (pre-command-hook nil))
      (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t))
                ((symbol-function 'cj/window--pull-away)
                 (lambda (dir) (setq pulled dir))))
        (let ((last-command-event (car case)))
          (cj/window-resize-sticky)))
      (should (eq pulled (cdr case)))           ; meta stripped, pulled to opposite side
      (should overriding-terminal-local-map)))) ; loop armed

(ert-deftest test-ui-navigation-window-resize-sticky-meta-arrow-resizes ()
  "Normal: with more than one window, M-<arrow> dispatches the matching
`windsize' command, same as the bare arrow -- the Meta modifier is stripped
before the resize-map lookup."
  (dolist (case '((M-left  . windsize-left)
                  (M-right . windsize-right)
                  (M-up    . windsize-up)
                  (M-down  . windsize-down)))
    (let ((ran nil)
          (overriding-terminal-local-map nil)
          (pre-command-hook nil))
      (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) nil))
                ((symbol-function (cdr case))
                 (lambda (&rest _) (interactive) (setq ran t))))
        (let ((last-command-event (car case)))
          (cj/window-resize-sticky)))
      (should ran)
      (should overriding-terminal-local-map))))

(ert-deftest test-ui-navigation-window-resize-bound-under-meta-arrow ()
  "Normal: each global `M-<arrow>' reaches the sticky-resize command."
  (dolist (arrow '("M-<left>" "M-<right>" "M-<up>" "M-<down>"))
    (should (eq (keymap-lookup (current-global-map) arrow)
                #'cj/window-resize-sticky))))

(provide 'test-ui-navigation--window-resize)
;;; test-ui-navigation--window-resize.el ends here
