;;; eww-config --- EWW Text Browser Settings -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ------------------------------------ EWW ------------------------------------

(use-package eww
  :ensure nil ;; built-in
  :bind
  ("M-E" . eww)
  (:map eww-mode-map
        ("<" . eww-back-url)    ;; in addition to 'l'
        (">" . eww-forward-url) ;; in addition to 'n'
        ("i" . eww-toggle-images)
        ("o" . eww-open-in-new-buffer))
  :config
  (setq shr-use-colors nil)                          ;; respect colors in the html
  (setq shr-bullet "• ")                             ;; unordered lists use bullet glyph
  (setq shr-folding-mode t)
  (setq eww-search-prefix "http://frogfind.com/?q=") ;; use Frog Find as search engine
  ;; (setq eww-search-prefix "https://duckduckgo.com/html?q=") ;; use Duck Duck Go as search engine
  (setq url-cookie-file "~/.local/share/cookies.txt")
  (setq url-privacy-level '(email agent lastloc)))   ;; don't send any info listed here

(provide 'eww-config)
;;; eww-config.el ends here
