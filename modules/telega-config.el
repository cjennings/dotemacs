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
;; isn't installed yet, `C-; T' surfaces a clear "install telega" error
;; until the install runs once.
;;
;; Launcher: =C-; T= (mnemonic: Telegram).  Previously `C-; G' because
;; `T' was contested between org-table and transcription menus -- both
;; have been moved (org-table flattened under `C-; O', transcription
;; cleared to M-x), so `T' is now telega's outright.

;;; Code:

(require 'keybindings)

(use-package telega
  :defer t
  :ensure nil
  :commands (telega)
  :custom
  (telega-use-docker t))

(defun cj/telega ()
  "Launch telega.el with a helpful message when it isn't installed yet.

The =telega= Emacs package uses =:ensure nil= in this config so a
stale MELPA archive index can't take startup down with a 404.  The
trade-off: a fresh clone needs a one-time install before this
launcher works.  Without this wrapper, the autoload stub fails with
the cryptic =Cannot open load file: telega=; with it, the user gets
pointed at =scripts/setup-telega.sh= and the manual fallback."
  (interactive)
  (if (or (featurep 'telega)
          (locate-library "telega"))
      (telega)
    (user-error
     (concat "telega not installed -- run scripts/setup-telega.sh, "
             "or `M-x package-install RET telega'"))))

(keymap-set cj/custom-keymap "T" #'cj/telega)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; T" "telegram (telega)"))

(provide 'telega-config)
;;; telega-config.el ends here
