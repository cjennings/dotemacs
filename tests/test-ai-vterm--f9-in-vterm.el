;;; test-ai-vterm--f9-in-vterm.el --- F9 reaches Emacs from inside an agent buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; vterm binds <f1>..<f12> to `vterm--self-insert', so a plain <f9> typed
;; while point is in an agent buffer is sent to the terminal program instead
;; of toggling the agent -- which is exactly the case when the agent buffer
;; fills the frame.  `ai-vterm.el' re-binds the F9 family in `vterm-mode-map'.
;; These tests load real vterm so `vterm-mode-map' exists, then confirm the
;; bindings landed (and the global ones are still there).

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'vterm)
(require 'ai-vterm)

(ert-deftest test-ai-vterm-f9-bound-in-vterm-mode-map ()
  "Normal: <f9> in `vterm-mode-map' runs the agent toggle, not `vterm--self-insert'."
  (should (eq (keymap-lookup vterm-mode-map "<f9>") #'cj/ai-vterm)))

(ert-deftest test-ai-vterm-f9-family-bound-in-vterm-mode-map ()
  "Normal: the C-/M- F9 variants are bound in `vterm-mode-map' too."
  (should (eq (keymap-lookup vterm-mode-map "C-<f9>") #'cj/ai-vterm-pick-project))
  (should (eq (keymap-lookup vterm-mode-map "M-<f9>") #'cj/ai-vterm-pick-buffer)))

(ert-deftest test-ai-vterm-f9-not-self-insert-in-vterm ()
  "Boundary: vterm's default <f9> -> `vterm--self-insert' was overridden."
  (should-not (eq (keymap-lookup vterm-mode-map "<f9>") 'vterm--self-insert)))

(ert-deftest test-ai-vterm-f9-still-bound-globally ()
  "Normal: the global F9 family bindings are intact."
  (should (eq (lookup-key (current-global-map) (kbd "<f9>")) #'cj/ai-vterm))
  (should (eq (lookup-key (current-global-map) (kbd "C-<f9>")) #'cj/ai-vterm-pick-project))
  (should (eq (lookup-key (current-global-map) (kbd "M-<f9>")) #'cj/ai-vterm-pick-buffer)))

(provide 'test-ai-vterm--f9-in-vterm)
;;; test-ai-vterm--f9-in-vterm.el ends here
