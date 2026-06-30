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

;;; cj/markdown-preview (guard: refuse when the httpd listener is down)

(ert-deftest test-markdown-preview-errors-when-server-down ()
  "Error: `cj/markdown-preview' signals a user-error when the simple-httpd
listener is not running, rather than opening a preview against a dead server.
Also pins the rename off the bare `markdown-preview' that markdown-mode shadows."
  (cl-letf (((symbol-function 'httpd-running-p) (lambda () nil)))
    (should-error (cj/markdown-preview) :type 'user-error)))

(provide 'test-markdown-config)
;;; test-markdown-config.el ends here
