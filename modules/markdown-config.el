;;; markdown-config --- Settings for Editing Markdown -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;;;; ------------------------- Markdown-Mode -------------------------

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
			  ("<f2>" . markdown-preview)) ;; use same key as compile for consistency
  :init (setq markdown-command "multimarkdown"))

;;;; ------------------------- Impatient-Mode ------------------------

;; allows for live previews of your html
;; see: https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :defer t
  :config
  (setq imp-set-user-filter 'markdown-html))

;;;; --------------------- WIP: Markdown-Preview ---------------------

;; the filter to apply to markdown before impatient-mode pushes it to the server
(defun markdown-preview ()
  (interactive)
  (httpd-start)
  (impatient-mode 1)
  (setq imp-user-filter #'cj/markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients)
  ;;  (browse-url-generic-function 'browse-url-xdg-open)
  (browse-url-generic "https://localhost:8080/imp" 1))

(defun cj/markdown-html (buffer)
  (princ (with-current-buffer buffer
		   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>"
				   (buffer-substring-no-properties (point-min) (point-max))))
		 (current-buffer)))

(provide 'markdown-config)
;;; markdown-config.el ends here
