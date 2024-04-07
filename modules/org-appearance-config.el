;;; org-appearance-config.el --- Org-Mode UI Appearance Settings -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(with-eval-after-load 'org

  ;; face settings need to be called every time org mode is loaded.
  (defun cj/set-org-face-settings()
	"Sets foreground, background, and font styles for org mode.
To be called every time org is loaded."
	(interactive)
	;; org-hide should use fix-pitch to align indents for proportional fonts
	(set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-meta-line nil :inherit 'shadow)

	;; Remove foreground and background from block faces
	(set-face-attribute 'org-block nil :foreground 'unspecified :background 'unspecified)
	(set-face-attribute 'org-block-begin-line nil :foreground 'unspecified :background 'unspecified)
	(set-face-attribute 'org-block-end-line nil :foreground 'unspecified :background 'unspecified)

	;; Get rid of the background on column views
	(set-face-attribute 'org-column nil :background 'unspecified)
	(set-face-attribute 'org-column-title nil :background 'unspecified)

	;; make sure org-links are underlined
	(set-face-attribute 'org-link nil :underline t)

	;; remove hook after first run to avoid calling function  everytime a frame is made
	(if (daemonp)
		(remove-hook 'server-after-make-frame-hook  #'cj/set-org-face-settings)))

  ;; if emacsclient, setup hook to run font settings function, otherwise, run it now
  (if (daemonp)
	  (add-hook 'server-after-make-frame-hook #'cj/set-org-face-settings)
	(cj/set-org-face-settings))


  ;; settings need to be called only once
  (setq org-ellipsis " ▾")                                  ;; change ellipses to down arrow
  (setq org-hide-emphasis-markers t)                        ;; remove emphasis markers to keep the screen clean
  (setq org-hide-leading-stars t)                           ;; hide leading stars, just show one per line
  (setq org-pretty-entities t)                              ;; render special symbols
  (setq org-pretty-entities-include-sub-superscripts nil)   ;; ...except superscripts and subscripts
  (setq org-fontify-emphasized-text nil)                    ;; ...and don't render bold and italic markup
  (setq org-fontify-whole-heading-line t)                   ;; fontify the whole line for headings (for face-backgrounds)
  (add-hook 'org-mode-hook 'prettify-symbols-mode)



  ;; nicer bullets than simple asterisks.
  (use-package org-superstar
	:after org
	:config
	(org-superstar-configure-like-org-bullets)
	(setq org-superstar-leading-bullet ?\s)
	(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

  ) ;; end with-eval-after-load

(provide 'org-appearance-config)
;;; org-appearance-config.el ends here.
