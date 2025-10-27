;;; diff-config.el --- diff Configuration  -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; I've configured Ediff for a clean and efficient diff experience.

;; • Ediff will use a plain control window, horizontal splits, ignore whitespace, and only highlight the current change.
;; • A single keymap under "C-c D" has bindings:
;;   - ediff-files (f)
;;   - ediff-buffers (b)
;;   - ediff-revision (r)
;;   - ediff-directories (D)
;; • An Ediff hook that remaps j/k to next/previous differences for easier navigation
;; • The winner-mode functionality ensures window layouts are restored after quitting Ediff

;; Note: Here's a highly useful setup for configuring ediff.
;; https://oremacs.com/2015/01/17/setting-up-ediff/

;;; Code:

(use-package ediff
  :ensure nil ;; built-in
  :defer t
  :custom
  (ediff-window-setup-function  'ediff-setup-windows-plain)
  (ediff-split-window-function  'split-window-horizontally)
  (ediff-diff-options            "-w")
  (ediff-highlight-all-diffs    nil)
  :bind-keymap ("C-c D" . cj/ediff-map)
  :init
  ;; adding this to a hook to make sure ediff is loaded due to :defer
  (defvar cj/ediff-map
	(let ((m (make-sparse-keymap)))
	  (keymap-set m "f" #'ediff-files)        ; C-c D f
	  (keymap-set m "b" #'ediff-buffers)      ; C-c D b
	  (keymap-set m "r" #'ediff-revision)     ; C-c D r
	  (keymap-set m "D" #'ediff-directories)  ; C-c D D
	  m)
	"Prefix map for quick Ediff commands under C-c D.")
  :config
  (defun cj/ediff-hook ()
	"Use j/k to navigate differences in Ediff."
	(ediff-setup-keymap)  ;; keep the defaults…
	(keymap-set ediff-mode-map "j" #'ediff-next-difference)
	(keymap-set ediff-mode-map "k" #'ediff-previous-difference))

  (add-hook 'ediff-mode-hook               #'cj/ediff-hook)
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c D" "ediff menu"
    "C-c D f" "ediff files"
    "C-c D b" "ediff buffers"
    "C-c D r" "ediff revision"
    "C-c D D" "ediff directories"))

(provide 'diff-config)
;;; diff-config.el ends here
