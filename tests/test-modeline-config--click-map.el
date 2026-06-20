;;; test-modeline-config--click-map.el --- Tests for cj/--modeline-click-map -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--modeline-click-map is the shared mode-line `local-map' builder extracted
;; from three clickable segments (buffer-name, vc, major-mode) that each spelled
;; out the same make-sparse-keymap + define-key dance.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'modeline-config)

(ert-deftest test-modeline-click-map-binds-mouse-1-and-3 ()
  "Normal: with both commands, mouse-1 and mouse-3 are bound."
  (let ((map (cj/--modeline-click-map 'vc-diff 'vc-root-diff)))
    (should (keymapp map))
    (should (eq (lookup-key map [mode-line mouse-1]) 'vc-diff))
    (should (eq (lookup-key map [mode-line mouse-3]) 'vc-root-diff))))

(ert-deftest test-modeline-click-map-mouse-1-only ()
  "Boundary: with no MOUSE-3, only mouse-1 is bound."
  (let ((map (cj/--modeline-click-map 'describe-mode)))
    (should (eq (lookup-key map [mode-line mouse-1]) 'describe-mode))
    (should (null (lookup-key map [mode-line mouse-3])))))

(provide 'test-modeline-config--click-map)
;;; test-modeline-config--click-map.el ends here
