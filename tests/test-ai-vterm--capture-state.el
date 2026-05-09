;;; test-ai-vterm--capture-state.el --- Tests for cj/--ai-vterm-capture-state -*- lexical-binding: t; -*-

;;; Commentary:
;; The capture helper writes WINDOW's direction and size to module-
;; level state vars `cj/--ai-vterm-last-direction' and
;; `cj/--ai-vterm-last-size'.  Called from `cj/ai-vterm''s toggle-off
;; branch so the next F9 display can restore the user's chosen
;; orientation and size.  No-op on a dead window.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--capture-state-right-split-sets-direction ()
  "Normal: right-split window -> direction=right, size in (0.4, 0.6)."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right))
          (cj/--ai-vterm-last-direction nil)
          (cj/--ai-vterm-last-size nil))
      (cj/--ai-vterm-capture-state right)
      (should (eq cj/--ai-vterm-last-direction 'right))
      (should (numberp cj/--ai-vterm-last-size))
      (should (and (> cj/--ai-vterm-last-size 0.4)
                   (< cj/--ai-vterm-last-size 0.6))))))

(ert-deftest test-ai-vterm--capture-state-below-split-sets-direction ()
  "Normal: below-split window -> direction=below, size in (0.4, 0.6)."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below))
          (cj/--ai-vterm-last-direction nil)
          (cj/--ai-vterm-last-size nil))
      (cj/--ai-vterm-capture-state below)
      (should (eq cj/--ai-vterm-last-direction 'below))
      (should (and (> cj/--ai-vterm-last-size 0.4)
                   (< cj/--ai-vterm-last-size 0.6))))))

(ert-deftest test-ai-vterm--capture-state-noop-on-dead-window ()
  "Boundary: nil window -> state remains unchanged."
  (let ((cj/--ai-vterm-last-direction 'sentinel-dir)
        (cj/--ai-vterm-last-size 0.123))
    (cj/--ai-vterm-capture-state nil)
    (should (eq cj/--ai-vterm-last-direction 'sentinel-dir))
    (should (= cj/--ai-vterm-last-size 0.123))))

(ert-deftest test-ai-vterm--capture-state-noop-on-deleted-window ()
  "Boundary: deleted window -> state remains unchanged."
  (let ((cj/--ai-vterm-last-direction 'sentinel-dir)
        (cj/--ai-vterm-last-size 0.123)
        (dead-win (save-window-excursion
                    (delete-other-windows)
                    (let ((w (split-window (selected-window) nil 'right)))
                      (delete-window w)
                      w))))
    (cj/--ai-vterm-capture-state dead-win)
    (should (eq cj/--ai-vterm-last-direction 'sentinel-dir))
    (should (= cj/--ai-vterm-last-size 0.123))))

(provide 'test-ai-vterm--capture-state)
;;; test-ai-vterm--capture-state.el ends here
