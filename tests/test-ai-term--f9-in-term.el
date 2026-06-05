;;; test-ai-term--f9-in-term.el --- F9 reaches Emacs from inside an agent buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; ghostel's semi-char mode forwards keys not in `ghostel-keymap-exceptions' to
;; the terminal program, so a plain <f9> typed while point is in an agent
;; buffer would be sent to the program instead of toggling the agent -- exactly
;; the case when the agent buffer fills the frame.  `ai-term.el' re-binds the F9
;; family in `ghostel-mode-map'.  These tests require ghostel (which defines
;; `ghostel-mode-map' and lets ai-term's `with-eval-after-load' fire) BEFORE
;; ai-term, then confirm the bindings landed (and the global ones are intact).
;; `(require 'ghostel)' does not load the native module, so this stays light.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ghostel)
(require 'ai-term)

(ert-deftest test-ai-term-f9-bound-in-ghostel-mode-map ()
  "Normal: <f9> in `ghostel-mode-map' runs the agent toggle."
  (should (eq (keymap-lookup ghostel-mode-map "<f9>") #'cj/ai-term)))

(ert-deftest test-ai-term-f9-family-bound-in-ghostel-mode-map ()
  "Normal: the C-/M-/C-S- F9 variants are bound in `ghostel-mode-map' too.
`M-<f9>' and `C-S-<f9>' both close an agent via `cj/ai-term-close'."
  (should (eq (keymap-lookup ghostel-mode-map "C-<f9>") #'cj/ai-term-pick-project))
  (should (eq (keymap-lookup ghostel-mode-map "M-<f9>") #'cj/ai-term-close))
  (should (eq (keymap-lookup ghostel-mode-map "C-S-<f9>") #'cj/ai-term-close)))

(ert-deftest test-ai-term-f9-still-bound-globally ()
  "Normal: the global F9 family bindings are intact.
`<f9>' toggles the ai-term agent window; `C-<f9>' picks a project
agent; `M-<f9>' and `C-S-<f9>' close an agent via `cj/ai-term-close'."
  (should (eq (lookup-key (current-global-map) (kbd "<f9>")) #'cj/ai-term))
  (should (eq (lookup-key (current-global-map) (kbd "C-<f9>")) #'cj/ai-term-pick-project))
  (should (eq (lookup-key (current-global-map) (kbd "M-<f9>")) #'cj/ai-term-close))
  (should (eq (lookup-key (current-global-map) (kbd "C-S-<f9>")) #'cj/ai-term-close)))

(provide 'test-ai-term--f9-in-term)
;;; test-ai-term--f9-in-term.el ends here
