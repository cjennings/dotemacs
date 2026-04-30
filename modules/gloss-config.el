;;; gloss-config.el --- Gloss personal-glossary configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; gloss — Glossary Lookup with Online-Sourced Selection.
;;
;; Personal glossary on `C-h g'.  Looks up terms in a single git-tracked
;; org file.  On a local miss, fetches candidate definitions from
;; Wiktionary and prompts to pick one.  Same org file feeds `org-drill'
;; for spaced-repetition study.
;;
;; v1 in shakedown.  After a few weeks of real use, this module can move
;; out of the "Modules In Test" section in init.el.

;;; Code:

(use-package gloss
  :vc (:url "git@cjennings.net:gloss.git"
       :branch "main"
       :rev :newest)
  :demand t
  :commands (gloss-lookup gloss-add gloss-edit
             gloss-fetch-online gloss-list-terms gloss-stats
             gloss-reload gloss-drill-export gloss-toggle-debug)
  :config
  (gloss-install-prefix))

(provide 'gloss-config)
;;; gloss-config.el ends here
