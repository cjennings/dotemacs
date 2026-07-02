;;; test-markdown-config.el --- Tests for markdown-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the org-side wiring in markdown-config.el.  The use-package
;; block for markdown-mode itself is upstream config; the assertion
;; here is on the local glue that points org at markdown-mode for
;; `#+begin_src markdown' blocks.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Require org BEFORE markdown-config so the `with-eval-after-load
;; 'org' body fires synchronously instead of being deferred.
(require 'org)
(require 'markdown-config)

;; simple-httpd (elpa) is absent under `make test'; stand in for it so
;; the preview path's (require 'simple-httpd) stays a no-op.  Tests mock
;; httpd-running-p explicitly, so the stub bodies never matter.
(unless (featurep 'simple-httpd)
  (defun httpd-running-p () nil)
  (defun httpd-start () nil)
  (provide 'simple-httpd))

(ert-deftest test-markdown-config-registers-markdown-org-src-lang ()
  "Normal: `markdown' shows up in `org-src-lang-modes' mapped to
`markdown' so org-lint stops warning on `#+begin_src markdown' and
`C-c '' opens those blocks in `markdown-mode'."
  (should (equal (cdr (assoc "markdown" org-src-lang-modes)) 'markdown)))

;;; cj/markdown-html (impatient-mode filter: source buffer -> HTML)

(ert-deftest test-markdown-html-wraps-source-buffer-content ()
  "Normal: the source buffer's text is embedded in the strapdown HTML shell."
  (let ((src (generate-new-buffer " *md-src*")))
    (unwind-protect
        (progn
          (with-current-buffer src (insert "# Title\n\nsome **markdown**"))
          (with-temp-buffer
            (cj/markdown-html src)
            (let ((html (buffer-string)))
              (should (string-match-p "<!DOCTYPE html>" html))
              (should (string-match-p "<xmp" html))
              (should (string-match-p "some \\*\\*markdown\\*\\*" html)))))
      (kill-buffer src))))

(ert-deftest test-markdown-html-vendors-strapdown-no-external-cdn ()
  "Normal: the preview embeds the vendored strapdown inline and references no
external CDN, so the preview works offline and doesn't load third-party JS over
plain HTTP."
  (let ((src (generate-new-buffer " *md-cdn*")))
    (unwind-protect
        (progn
          (with-current-buffer src (insert "# Hello"))
          (with-temp-buffer
            (cj/markdown-html src)
            (let ((html (buffer-string)))
              ;; No external CDN of any kind.
              (should-not (string-match-p "ndossougbe" html))
              (should-not (string-match-p "src=\"https?://" html))
              ;; Vendored strapdown is embedded inline (a bare <script> with the
              ;; ~121KB bundle, not a <script src=...>).
              (should (string-match-p "<script>" html))
              (should (> (length html) 100000)))))
      (kill-buffer src))))

(ert-deftest test-markdown-html-empty-source-buffer ()
  "Boundary: an empty source buffer still yields the HTML shell."
  (let ((src (generate-new-buffer " *md-empty*")))
    (unwind-protect
        (with-temp-buffer
          (cj/markdown-html src)
          (should (string-match-p "<!DOCTYPE html>" (buffer-string)))
          (should (string-match-p "<xmp" (buffer-string))))
      (kill-buffer src))))

;;; cj/markdown-preview (autostart: bring the httpd listener up when down)

(defvar imp-user-filter)
(defvar imp-last-state)

(ert-deftest test-markdown-preview-starts-server-when-down ()
  "Normal: `cj/markdown-preview' starts the httpd listener when it's down
\(the 2026-07-01 autostart decision replaced the old user-error guard).
Also pins the rename off the bare `markdown-preview' that markdown-mode
shadows.  Everything downstream is mocked so no listener or browser runs."
  (let ((started nil)
        (imp-user-filter nil)
        (imp-last-state 0))
    (cl-letf (((symbol-function 'httpd-running-p) (lambda () nil))
              ((symbol-function 'cj/markdown-preview-server-start)
               (lambda () (setq started t)))
              ((symbol-function 'impatient-mode) (lambda (&rest _) nil))
              ((symbol-function 'imp--notify-clients) (lambda (&rest _) nil))
              ((symbol-function 'browse-url) (lambda (&rest _) nil)))
      (with-temp-buffer
        (cj/markdown-preview))
      (should started))))

(provide 'test-markdown-config)
;;; test-markdown-config.el ends here
