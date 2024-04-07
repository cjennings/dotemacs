;;; modeline-config --- Modeline Settings -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; I'm currently working on refactoring and bug fixing for
;; tautologyclub/feebleline. In transition, I use moodline. Both are good, but I
;; prefer the slimmer modeline which just uses the echo area.

;; This code allows me to load and test feebleline when it's working directory
;; exists. Otherwise, it loads and configures moodline.

;;; Code:

(defvar feebleline-working-directory "~/code/feebleline"
  "The working directory for the feebleline mode line.")

;; --------------------------------- Feebleline --------------------------------
;; ultrathin simple modeline. adds only useful info to echo area.

(use-package feebleline
  :if (file-readable-p feebleline-working-directory)
  :load-path feebleline-working-directory
  :config
  (feebleline-mode))

;; ---------------------------------- Moodline ---------------------------------
;; a sleek and minimalistic modeline.

(use-package mood-line
  :unless (file-readable-p feebleline-working-directory)
  :config
  (setq mood-line-glyph-alist mood-line-glyphs-ascii)
  (mood-line-mode))

(provide 'modeline-config)
;;; modeline-config.el ends here
