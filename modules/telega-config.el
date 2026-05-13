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
;; Launcher: =C-; G= (mnemonic: teleGram).  Neither =C-; t= (test-runner
;; menu) nor =C-; m t= (music "repeat track") were available, so the
;; launcher lives at a free top-level letter rather than under a
;; messaging sub-prefix.

;;; Code:

(require 'keybindings)

(use-package telega
  :defer t
  :commands (telega)
  :custom
  (telega-use-docker t))

(keymap-set cj/custom-keymap "G" #'telega)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; G" "telegram (telega)"))

(provide 'telega-config)
;;; telega-config.el ends here
