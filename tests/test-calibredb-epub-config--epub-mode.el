;;; test-calibredb-epub-config--epub-mode.el --- Tests for epub mode resolution -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that .epub files reach nov-mode through `auto-mode-alist' alone, with
;; no advice on `set-auto-mode'.
;;
;; Background: the module used to carry an :around advice on `set-auto-mode'
;; forcing nov-mode for .epub, added to keep `magic-fallback-mode-alist' from
;; opening the zip container in archive-mode.  It was never needed.
;; `set-auto-mode' consults `auto-mode-alist' before `magic-fallback-mode-alist',
;; and nov's use-package :mode registers "\\.epub\\'" there, so the alist
;; already won.  Verified live on the daemon: a real zip-format .epub opened in
;; nov-mode both with the advice and with it removed.
;;
;; The advice was not free.  `set-auto-mode' runs on every file visit, so the
;; advice put a redundant frame and an extra failure surface on the path for
;; every file of every type.
;;
;; The second test is a regression guard: it fails if the advice is ever
;; reinstated, which is the mistake this cleanup exists to prevent.
;;
;; Test organization:
;; - Normal Cases: .epub resolves to nov-mode; no advice on set-auto-mode
;; - Boundary Cases: a path merely containing "epub", and a bare "epub" name
;; - Error Cases: an unrelated extension does not resolve to nov-mode
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)

(defun test-epub-mode--resolve (filename)
  "Return the major mode `auto-mode-alist' assigns to FILENAME."
  (assoc-default filename auto-mode-alist 'string-match))

;;; Normal Cases

(ert-deftest test-calibredb-epub-config-epub-resolves-to-nov-mode ()
  "Normal: auto-mode-alist maps a .epub file to nov-mode on its own."
  (should (eq 'nov-mode (test-epub-mode--resolve "book.epub"))))

(ert-deftest test-calibredb-epub-config-no-set-auto-mode-advice ()
  "Normal: nothing advises set-auto-mode to force nov-mode.
Regression guard.  auto-mode-alist already wins over
magic-fallback-mode-alist, so an advice here would be redundant work on
every file visit."
  (should-not (advice-member-p 'cj/force-nov-mode-for-epub 'set-auto-mode))
  (should-not (fboundp 'cj/force-nov-mode-for-epub)))

;;; Boundary Cases

(ert-deftest test-calibredb-epub-config-epub-in-directory-name ()
  "Boundary: the extension anchors at the end, so a directory named epub
does not by itself select nov-mode."
  (should-not (eq 'nov-mode (test-epub-mode--resolve "/home/user/epub/notes.txt"))))

(ert-deftest test-calibredb-epub-config-epub-with-path ()
  "Boundary: a full path with directories still resolves on the extension."
  (should (eq 'nov-mode (test-epub-mode--resolve "/home/user/books/a b.epub"))))

;;; Error Cases

(ert-deftest test-calibredb-epub-config-other-extension-not-nov ()
  "Error: an unrelated extension must not resolve to nov-mode."
  (should-not (eq 'nov-mode (test-epub-mode--resolve "archive.zip")))
  (should-not (eq 'nov-mode (test-epub-mode--resolve "notes.org"))))

(provide 'test-calibredb-epub-config--epub-mode)
;;; test-calibredb-epub-config--epub-mode.el ends here
