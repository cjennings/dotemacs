;;; test-ai-term--keybindings.el --- ai-term keybinding placement -*- lexical-binding: t; -*-

;;; Commentary:
;; ai-term lives under the C-; a prefix (vacated when gptel was archived), with
;; the frequent "swap to the next agent" also on M-SPC for a fast chord.  M-SPC
;; must reach Emacs from inside an agent buffer, so it is bound in
;; `ghostel-mode-map' and added to `ghostel-keymap-exceptions' (the semi-char
;; map otherwise forwards it to the pty).  C-; is already an exception via
;; term-config, so the C-; a family resolves through the global prefix.  These
;; tests require ghostel (so ai-term's `with-eval-after-load' fires) before
;; ai-term, then confirm the bindings landed and the old F9 family is gone.
;; `(require 'ghostel)' does not load the native module, so this stays light.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ghostel)
(require 'ai-term)

(ert-deftest test-ai-term-keymap-leaf-bindings ()
  "Normal: the ai-term keymap binds toggle/select/next/kill on a/s/n/k."
  (should (eq (keymap-lookup cj/ai-term-keymap "a") #'cj/ai-term))
  (should (eq (keymap-lookup cj/ai-term-keymap "s") #'cj/ai-term-pick-project))
  (should (eq (keymap-lookup cj/ai-term-keymap "n") #'cj/ai-term-next))
  (should (eq (keymap-lookup cj/ai-term-keymap "k") #'cj/ai-term-close)))

(ert-deftest test-ai-term-keymap-registered-under-custom-prefix ()
  "Normal: the ai-term keymap is registered under C-; a."
  (should (eq (keymap-lookup cj/custom-keymap "a") cj/ai-term-keymap)))

(ert-deftest test-ai-term-next-bound-to-meta-space-globally ()
  "Normal: M-SPC runs `cj/ai-term-next' (the fast swap chord)."
  (should (eq (lookup-key (current-global-map) (kbd "M-SPC")) #'cj/ai-term-next)))

(ert-deftest test-ai-term-meta-space-bound-in-ghostel-mode-map ()
  "Normal: M-SPC is bound in `ghostel-mode-map' so swap works inside an agent."
  (should (eq (keymap-lookup ghostel-mode-map "M-SPC") #'cj/ai-term-next)))

(ert-deftest test-ai-term-meta-space-in-keymap-exceptions ()
  "Regression: M-SPC is in `ghostel-keymap-exceptions' so semi-char mode lets it
reach Emacs instead of forwarding it to the pty."
  (should (member "M-SPC" ghostel-keymap-exceptions))
  (should-not (eq (keymap-lookup ghostel-semi-char-mode-map "M-SPC")
                  'ghostel--send-event)))

(ert-deftest test-ai-term-f9-family-removed-globally ()
  "Regression: the old F9 family no longer binds the ai-term commands globally."
  (should-not (eq (lookup-key (current-global-map) (kbd "<f9>")) #'cj/ai-term))
  (should-not (eq (lookup-key (current-global-map) (kbd "C-<f9>")) #'cj/ai-term-pick-project))
  (should-not (eq (lookup-key (current-global-map) (kbd "s-<f9>")) #'cj/ai-term-next))
  (should-not (eq (lookup-key (current-global-map) (kbd "M-<f9>")) #'cj/ai-term-close)))

(provide 'test-ai-term--keybindings)
;;; test-ai-term--keybindings.el ends here
