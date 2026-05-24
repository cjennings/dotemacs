;;; test-markdown-config.el --- Tests for markdown-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the org-side wiring in markdown-config.el.  The use-package
;; block for markdown-mode itself is upstream config; the assertion
;; here is on the local glue that points org at markdown-mode for
;; `#+begin_src markdown' blocks.

;;; Code:

(require 'ert)

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
              (should (string-match-p "strapdown\\.js" html))
              (should (string-match-p "some \\*\\*markdown\\*\\*" html)))))
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

(provide 'test-markdown-config)
;;; test-markdown-config.el ends here
