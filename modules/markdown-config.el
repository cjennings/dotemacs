;;; markdown-config --- Settings for Editing Markdown -*- lexical-binding: t; coding: utf-8; -*-
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

;; Register markdown as a known org-src-block language so `org-lint'
;; stops warning on `#+begin_src markdown ... #+end_src' and `C-c ''
;; inside such a block opens it in `markdown-mode' instead of falling
;; back to fundamental-mode.
(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("markdown" . markdown)))

;;;; ------------------------- Impatient-Mode ------------------------

;; allows for live previews of your html
;; see: https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :defer t
  :config
  (setq imp-set-user-filter 'markdown-html))

;;;; --------------------- WIP: Markdown-Preview ---------------------

(defun cj/markdown-preview-server-start ()
  "Start the simple-httpd listener that serves the live markdown preview.
Idempotent: re-running while the server is already up is a no-op."
  (interactive)
  (require 'simple-httpd)
  (httpd-start)
  (message "markdown preview server running on http://localhost:8080/imp"))

;; the filter to apply to markdown before impatient-mode pushes it to the server
(defun markdown-preview ()
  "Open the current buffer as a live HTML preview at http://localhost:8080/imp.
The simple-httpd listener must already be running -- see
`cj/markdown-preview-server-start'.  Starting a network listener as a
side effect of opening a preview is surprising, so the server start
lives in a separate command."
  (interactive)
  (unless (and (boundp 'httpd-process) httpd-process)
    (user-error "markdown preview server not running; run `M-x cj/markdown-preview-server-start' first"))
  (impatient-mode 1)
  (setq imp-user-filter #'cj/markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients)
  ;; Use plain `browse-url' (not `browse-url-generic') so the user's
  ;; default protocol handler picks the browser, and use `http://' --
  ;; simple-httpd serves plaintext; the previous `https://' URL caused
  ;; a TLS handshake against a non-TLS listener and the preview never
  ;; rendered.
  (browse-url "http://localhost:8080/imp"))

(defun cj/markdown-html (buffer)
  (princ (with-current-buffer buffer
		   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>"
				   (buffer-substring-no-properties (point-min) (point-max))))
		 (current-buffer)))

(provide 'markdown-config)
;;; markdown-config.el ends here
