;;; test-ai-term--capture-state.el --- Tests for cj/--ai-term-capture-state -*- lexical-binding: t; -*-

;;; Commentary:
;; The capture helper writes WINDOW's direction and size to module-
;; level state vars `cj/--ai-term-last-direction' and
;; `cj/--ai-term-last-size'.  Called from `cj/ai-term''s toggle-off
;; branch so the next F9 display can restore the user's chosen
;; orientation and size.  No-op on a dead window.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(ert-deftest test-ai-term--capture-state-right-split-sets-direction ()
  "Normal: right-split window -> direction=right, integer body-cols matching window."
  (save-window-excursion
    (delete-other-windows)
    (let ((right (split-window (selected-window) nil 'right))
          (cj/--ai-term-last-direction nil)
          (cj/--ai-term-last-size nil))
      (cj/--ai-term-capture-state right)
      (should (eq cj/--ai-term-last-direction 'right))
      (should (integerp cj/--ai-term-last-size))
      (should (= cj/--ai-term-last-size (window-body-width right))))))

(ert-deftest test-ai-term--capture-state-below-split-sets-direction ()
  "Normal: below-split window -> direction=below, integer body-lines matching window."
  (save-window-excursion
    (delete-other-windows)
    (let ((below (split-window (selected-window) nil 'below))
          (cj/--ai-term-last-direction nil)
          (cj/--ai-term-last-size nil))
      (cj/--ai-term-capture-state below)
      (should (eq cj/--ai-term-last-direction 'below))
      (should (integerp cj/--ai-term-last-size))
      (should (= cj/--ai-term-last-size (window-body-height below))))))

(ert-deftest test-ai-term--capture-state-noop-on-dead-window ()
  "Boundary: nil window -> state remains unchanged."
  (let ((cj/--ai-term-last-direction 'sentinel-dir)
        (cj/--ai-term-last-size 0.123))
    (cj/--ai-term-capture-state nil)
    (should (eq cj/--ai-term-last-direction 'sentinel-dir))
    (should (= cj/--ai-term-last-size 0.123))))

(ert-deftest test-ai-term--capture-state-noop-on-deleted-window ()
  "Boundary: deleted window -> state remains unchanged."
  (let ((cj/--ai-term-last-direction 'sentinel-dir)
        (cj/--ai-term-last-size 0.123)
        (dead-win (save-window-excursion
                    (delete-other-windows)
                    (let ((w (split-window (selected-window) nil 'right)))
                      (delete-window w)
                      w))))
    (cj/--ai-term-capture-state dead-win)
    (should (eq cj/--ai-term-last-direction 'sentinel-dir))
    (should (= cj/--ai-term-last-size 0.123))))

(provide 'test-ai-term--capture-state)
;;; test-ai-term--capture-state.el ends here
