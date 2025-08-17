;;; ediff-config.el --- diff Configuration  -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; I've tied Ediff with ZTree into a single keybinding for convenience.

;; • Ediff will use a plain control window, horizontal splits, ignore whitespace, and only highlight the current change.
;; • A single keymap under “C-c e” has bindings:
;;   - ediff-files,
;;   - ediff-buffers (b)
;;   - ediff-revision (r)
;;   - ediff-directories (D)
;;   - ztree directory diffs (d)
;; • An Ediff hook that remaps j/k to next/previous differences and q to quit immediately
;; • Einner-mode enabled so window layouts are restored after quitting Ediff

;; An accompanying ERT test ensures that the “d” key in the ediff map is correctly bound to ztree-diff.

;; Note: Here's a highly useful setup for configuring ediff.
;; https://oremacs.com/2015/01/17/setting-up-ediff/

;;; Code:

(use-package ediff
  :ensure nil ;; built-in
  :defer 0.5
  :custom
  (ediff-window-setup-function  'ediff-setup-windows-plain)
  (ediff-split-window-function  'split-window-horizontally)
  (ediff-diff-options            "-w")
  (ediff-highlight-all-diffs    nil)
  :bind-keymap ("C-c e" . cj/ediff-map)
  :init
  ;; adding this to a hook to make sure ediff is loaded due to :defer
  (defvar cj/ediff-map
	(let ((m (make-sparse-keymap)))
	  (define-key m "f" #'ediff-files)        ; C-c e f
	  (define-key m "b" #'ediff-buffers)      ; C-c e b
	  (define-key m "r" #'ediff-revision)     ; C-c e r
	  (define-key m "D" #'ediff-directories)  ; C-c e D
	  m)
	"Prefix map for quick Ediff commands under C-c e.")
  (winner-mode 1)
  :config
  (defun cj/ediff-hook ()
	"Use j/k to navigate and q to quit immediately in Ediff."
	(ediff-setup-keymap)  ;; keep the defaults…
	(define-key ediff-mode-map "j" #'ediff-next-difference)
	(define-key ediff-mode-map "k" #'ediff-previous-difference)
	(define-key ediff-mode-map "q" #'ediff-quit))

  (add-hook 'ediff-mode-hook               #'cj/ediff-hook)
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;; ----------------------------------- Ztree -----------------------------------
;; diff two directories

(use-package ztree
  :after ediff
  :defer 0.5
  :bind
  (:map cj/ediff-map
		("d" . ztree-diff)))

(provide 'diff-config)
;;; diff-config.el ends here.


;; --------------------------------- ERT Tests ---------------------------------

(ert-deftest ediff-config/ediff-map-includes-ztree-d ()
  "Ensure that \"d\" in `cj/ediff-map' is bound to `ztree-diff'."
  (should (eq (lookup-key cj/ediff-map "d") #'ztree-diff)))

(ert-deftest ediff-config/ediff-mode-map-has-j-k-q ()
  (with-temp-buffer
    ;; force-load ediff
    (require 'ediff)
    (mapc (lambda (binding)
            (let ((key (kbd (car binding)))
                  (fn  (cdr binding)))
              (should (eq (lookup-key ediff-mode-map key) fn))))
          '(("j" . ediff-next-difference)
            ("k" . ediff-previous-difference)
            ("q" . ediff-quit)))))
