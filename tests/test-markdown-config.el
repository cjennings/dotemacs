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

(provide 'test-markdown-config)
;;; test-markdown-config.el ends here
