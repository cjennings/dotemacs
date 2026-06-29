;;; test-meta-package-headers.el --- Enforce Elisp package-header conventions -*- lexical-binding: t; -*-

;;; Commentary:
;; Checks that every owned active config module follows the standard Emacs
;; Library Header conventions -- the part test-init-module-headers.el does not
;; cover (it enforces the load-graph metadata block inside the Commentary):
;;
;;   1. First line is  ;;; NAME.el --- SUMMARY -*- ... -*-  (name carries the
;;      .el, summary present, file-local-variable cookie present).
;;   2. ;;; Commentary: appears before ;;; Code:.
;;   3. A (provide 'NAME) footer, so the file is require-able.
;;   4. No UTF-8 BOM before the header.
;;
;; Scope is modules/*.el, the owned active module set.  Vendored (custom/),
;; generated (themes/, browser-choice.el), archived (archive/), and private
;; (*.local.el) files are out of scope by design -- classifying those is the
;; file-class policy task, not this test.  The checker reads files on disk
;; without loading them, so it adds no startup or package dependency.

;;; Code:

(require 'ert)

(defconst test-pkg-header--exempt '()
  "Basenames under modules/ exempt from the package-header checks.
Empty today.  Add a basename with a comment when a module is intentionally
shaped differently, so the exemption is explicit rather than silent.")

(defun test-pkg-header--check (name text)
  "Return the list of violation symbols for module NAME given file TEXT.
NAME is the basename (e.g. \"font-config.el\").  An empty list means the
file is conformant.  Possible symbols: `bom', `header', `markers',
`order', `provide'."
  (let ((violations '()))
    (when (string-prefix-p "﻿" text)
      (push 'bom violations))
    (let ((first-line (car (split-string text "\n"))))
      (unless (string-match-p
               (concat "\\`;;; " (regexp-quote name) " --- .+-\\*-.*-\\*-")
               first-line)
        (push 'header violations)))
    (let ((commentary (string-match "^;;; Commentary:" text))
          (code (string-match "^;;; Code:" text)))
      (cond ((or (null commentary) (null code)) (push 'markers violations))
            ((>= commentary code) (push 'order violations))))
    (let ((stem (file-name-sans-extension name)))
      (unless (string-match-p (concat "^(provide '" (regexp-quote stem) ")") text)
        (push 'provide violations)))
    (nreverse violations)))

(ert-deftest test-pkg-header-checker-flags-malformed ()
  "Error: the checker catches each malformed shape."
  (should (memq 'bom
                (test-pkg-header--check
                 "foo.el"
                 "﻿;;; foo.el --- x -*- lexical-binding: t; -*-\n;;; Commentary:\n;;; Code:\n(provide 'foo)")))
  (should (memq 'header
                (test-pkg-header--check
                 "foo.el"
                 ";;; foo --- x -*- lexical-binding: t; -*-\n;;; Commentary:\n;;; Code:\n(provide 'foo)")))
  (should (memq 'order
                (test-pkg-header--check
                 "foo.el"
                 ";;; foo.el --- x -*- lexical-binding: t; -*-\n;;; Code:\n;;; Commentary:\n(provide 'foo)")))
  (should (memq 'provide
                (test-pkg-header--check
                 "foo.el"
                 ";;; foo.el --- x -*- lexical-binding: t; -*-\n;;; Commentary:\n;;; Code:\n"))))

(ert-deftest test-pkg-header-checker-passes-conformant ()
  "Normal: a well-formed module yields no violations."
  (should-not (test-pkg-header--check
               "foo.el"
               ";;; foo.el --- A thing -*- lexical-binding: t; -*-\n;;; Commentary:\n;; doc\n;;; Code:\n(provide 'foo)\n")))

(ert-deftest test-pkg-header-checker-boundary-empty ()
  "Boundary: empty file text reports every applicable violation, no crash."
  (let ((v (test-pkg-header--check "foo.el" "")))
    (should (memq 'header v))
    (should (memq 'markers v))
    (should (memq 'provide v))))

(ert-deftest test-pkg-header-all-modules-conform ()
  "Normal: every modules/*.el passes the package-header checks."
  (let ((dir (expand-file-name "modules" user-emacs-directory))
        (bad '()))
    (dolist (file (directory-files dir t "\\.el\\'"))
      (let ((name (file-name-nondirectory file)))
        (unless (member name test-pkg-header--exempt)
          (let* ((text (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
                 (violations (test-pkg-header--check name text)))
            (when violations (push (cons name violations) bad))))))
    (should-not bad)))

(provide 'test-meta-package-headers)
;;; test-meta-package-headers.el ends here
