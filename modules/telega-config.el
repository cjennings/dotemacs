;;; telega-config.el --- Telega Telegram client config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Configures telega.el (https://github.com/zevlg/telega.el) as an
;; in-Emacs Telegram client.
;;
;; TDLib (Telegram Database Library) runs in a docker container via
;; `telega-use-docker' so a fresh-clone install does not need a
;; system-level TDLib build.  =scripts/setup-telega.sh= prepares the
;; container the first time; afterwards telega.el reattaches
;; automatically.
;;
;; First-run auth (phone number + Telegram verification code) is
;; interactive and happens inside `M-x telega'.  This module does not
;; script it.
;;
;; Install:
;;
;;   M-x package-refresh-contents
;;   M-x package-install RET telega
;;
;; The refresh is important.  MELPA rotates dated snapshot tarballs out
;; from under the cached archive index periodically, so if the local
;; archive-contents file points at a snapshot that no longer exists on
;; the server the install fails with a 404.  Refreshing pulls a current
;; index.  This module deliberately sets `:ensure nil' so a stale
;; archive doesn't take Emacs init down at startup; if the package
;; isn't installed yet, `C-; G' will signal a void-function until the
;; install runs once.
;;
;; Launcher: =C-; G= (mnemonic: teleGram).  Neither =C-; t= (test-runner
;; menu) nor =C-; m t= (music "repeat track") were available, so the
;; launcher lives at a free top-level letter rather than under a
;; messaging sub-prefix.

;;; Code:

(require 'keybindings)

(use-package telega
  :defer t
  :ensure nil
  :commands (telega)
  :custom
  (telega-use-docker t))

(keymap-set cj/custom-keymap "G" #'telega)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; G" "telegram (telega)"))

(provide 'telega-config)
;;; telega-config.el ends here
