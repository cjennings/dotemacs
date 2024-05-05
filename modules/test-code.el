;;; test-code.el --- test code -*- lexical-binding: t; -*-

;;; Commentary:
;; This is where you should put config code you want to test.
;; I recommend calling this file from the end of your init.el
;; if something breaks, you have most of your Emacs config loaded.

;;; Code:

;; ----------------------------------- Mpdel -----------------------------------

(use-package mpdel
  :defer .5
  :config
  (setq mpdel-prefix-key (kbd "M-p"))
  (mpdel-mode))


;; ---------------------------------- Yeetube ----------------------------------
;; youtube frontend for emacs

(use-package yeetube
  :init (define-prefix-command 'cj/yeetube-map)
  :bind (("C-c y" . 'cj/yeetube-map)
		 :map cj/yeetube-map
		 ("s" . 'yeetube-search)
		 ("b" . 'yeetube-play-saved-video)
		 ("d" . 'yeetube-download-videos)
		 ("p" . 'yeetube-mpv-toggle-pause)
		 ("v" . 'yeetube-mpv-toggle-video)
		 ("V" . 'yeetube-mpv-toggle-no-video-flag)
		 ("k" . 'yeetube-remove-saved-video))
  :custom
  (yeetube-results-limit 50)
  (yeetube-download-directory (expand-file-name "videos" "~"))
  (yeetube-filter "Views")
  (setq yeetube-display-thumbnails nil)
  :config
  (setf yeetube-mpv-disable-video nil))

;; --------------------------------- Easy Hugo ---------------------------------

(use-package easy-hugo
  :defer .5
  :init
  (setq easy-hugo-basedir "~/code/cjennings-net/")
  (setq easy-hugo-url "https://cjennings.net")
  (setq easy-hugo-sshdomain "cjennings.net")
  (setq easy-hugo-root "/var/www/cjennings/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-postdir "content")
  (setq easy-hugo-server-flags "-D --noHTTPCache --disableFastRender")
  (setq easy-hugo-default-ext ".md")
  :bind ("C-c H" . easy-hugo)
  :config
  (easy-hugo-enable-menu))

;; -------------------------------- Google This --------------------------------

(use-package google-this
  :load-path "~/code/emacs-google-this/"
  :defer 1
  :bind
  ("C-h g" . 'google-this-search)
  :config
  (google-this-mode 1)
  (setq google-this-browse-url-function 'eww-browse-url))

;; ----------------------------------- Wttrin ----------------------------------
;; show the weather forecast in an Emacs buffer

(use-package wttrin
  :defer .5
  :load-path ("~/code/emacs-wttrin")
  :ensure nil ;; local package
  :preface
  ;; dependency for wttrin
  (use-package xterm-color
	:demand t)
  :bind
  ("M-W" . wttrin))

;; ------------------------------ ERC Yank To Gist -----------------------------
;; automatically create a Gist if pasting more than 5 lines
;; this module requires https://github.com/defunkt/gist
;; via ruby: 'gem install gist' via the aur: yay -S gist

(use-package erc-yank
  :defer 1
  :after erc
  :bind
  (:map erc-mode-map
		("C-y" . erc-yank)))

;; --------------------------------- Ob-Racket ---------------------------------

;; (use-package ob-racket
;;   :load-path "~/code/ob-racket"
;;   :defer .5
;;   :after racket-mode
;;   :commands (org-babel-execute:racket)
;;   :quelpa (ob-racket
;;       :fetcher github
;;       :repo "hasu/emacs-ob-racket"
;;       :files ("*.el" "*.rkt")))

(provide 'test-code)
;;; test-code.el ends here.
