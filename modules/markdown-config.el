;;; markdown-config.el --- Settings for Editing Markdown -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; markdown editing, a mode-loaded deferral candidate.
;; Top-level side effects: package configuration via use-package; org-src lang
;;   mapping registered after-load.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;;; Code:

;;;; ------------------------- Markdown-Mode -------------------------

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Register markdown as a known org-src-block language so `org-lint'
;; stops warning on `#+begin_src markdown ... #+end_src' and `C-c ''
;; inside such a block opens it in `markdown-mode' instead of falling
;; back to fundamental-mode.
(defvar org-src-lang-modes)
(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("markdown" . markdown)))

;;;; ------------------------- Impatient-Mode ------------------------

;; allows for live previews of your html
;; see: https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :defer t)

;;;; --------------------- WIP: Markdown-Preview ---------------------

(declare-function imp--notify-clients "impatient-mode")
(declare-function httpd-running-p "simple-httpd")
(declare-function httpd-start "simple-httpd")

(defun cj/markdown-preview-server-start ()
  "Start the simple-httpd listener that serves the live markdown preview.
Idempotent: re-running while the server is already up is a no-op."
  (interactive)
  (require 'simple-httpd)
  (httpd-start)
  (message "markdown preview server running on http://localhost:8080/imp"))

(defun cj/--markdown-preview-ensure-server ()
  "Start the markdown preview server unless it's already running."
  (require 'simple-httpd)
  (unless (httpd-running-p)
    (cj/markdown-preview-server-start)))

;; the filter to apply to markdown before impatient-mode pushes it to the server
(defun cj/markdown-preview ()
  "Open the current buffer as a live HTML preview at http://localhost:8080/imp.
Starts the simple-httpd listener itself when it isn't already running
\(per the 2026-07-01 decision; the earlier separate-start design
signaled a `user-error' instead)."
  (interactive)
  (cj/--markdown-preview-ensure-server)
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

;; strapdown.js renders the markdown client-side.  It is vendored under
;; assets/ and embedded inline rather than loaded from
;; http://ndossougbe.github.io/strapdown/dist/strapdown.js: the CDN was plain
;; HTTP (mixed content), an unmaintained third-party github.io page (a
;; supply-chain and MITM surface), and made the preview fail with no network.
;; The bundle (jQuery + marked + bootstrap themes) is self-contained, so inlining
;; it serves the whole preview from localhost.  Refresh by re-downloading the
;; dist build into assets/strapdown.js.
(defconst cj/markdown--strapdown-file
  (expand-file-name "assets/strapdown.js" user-emacs-directory)
  "Path to the vendored strapdown.js bundle served with the markdown preview.")

(defvar cj/markdown--strapdown-cache nil
  "Cached contents of `cj/markdown--strapdown-file', read once on first preview.")

(defun cj/markdown--strapdown-js ()
  "Return the vendored strapdown.js source, reading and caching it once."
  (or cj/markdown--strapdown-cache
      (setq cj/markdown--strapdown-cache
            (with-temp-buffer
              (insert-file-contents-literally cj/markdown--strapdown-file)
              (buffer-string)))))

(defun cj/markdown-html (buffer)
  (princ (with-current-buffer buffer
		   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script>%s</script></html>"
				   (buffer-substring-no-properties (point-min) (point-max))
				   (cj/markdown--strapdown-js)))
		 (current-buffer)))

;; Bind the preview key after the defun so use-package's `:bind' autoload
;; stub doesn't collide with this file's own definition of the command
;; (that collision is the "defined multiple times" byte-compile warning).
;; Same key as compile, for consistency.
(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "<f2>" #'cj/markdown-preview))

(provide 'markdown-config)
;;; markdown-config.el ends here
