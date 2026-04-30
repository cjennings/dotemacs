;;; test-config-utilities--summary-string.el --- Tests for cj/emacs-build--summary-string -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/emacs-build--summary-string'. The function returns a
;; multi-line string covering Emacs version, system, build metadata,
;; and capability flags. Tests assert that key sections are present
;; without locking to exact wording.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(ert-deftest test-config-utilities-summary-string-includes-version-line ()
  "Normal: summary string includes a Version line with `emacs-version'."
  (let ((text (cj/emacs-build--summary-string)))
    (should (string-match-p (regexp-quote (format "Version: %s" emacs-version))
                            text))))

(ert-deftest test-config-utilities-summary-string-includes-system-line ()
  "Normal: summary string includes the System: line."
  (should (string-match-p "^System: " (cj/emacs-build--summary-string))))

(ert-deftest test-config-utilities-summary-string-includes-build-date-line ()
  "Normal: summary string includes Build date."
  (should (string-match-p "^Build date: " (cj/emacs-build--summary-string))))

(ert-deftest test-config-utilities-summary-string-includes-capabilities-section ()
  "Normal: summary string includes the Capabilities section with the
known capability lines (native compilation, dynamic modules, GnuTLS,
libxml2, ImageMagick, SQLite)."
  (let ((text (cj/emacs-build--summary-string)))
    (should (string-match-p "Capabilities:" text))
    (dolist (cap '("Native compilation"
                   "Dynamic modules"
                   "GnuTLS"
                   "libxml2"
                   "ImageMagick"
                   "SQLite"))
      (should (string-match-p (regexp-quote cap) text)))))

(ert-deftest test-config-utilities-summary-string-renders-yes-or-no-flags ()
  "Boundary: each capability line ends in either \"yes\" or \"no\"."
  (let ((text (cj/emacs-build--summary-string)))
    (dolist (cap '("Native compilation" "Dynamic modules"
                   "GnuTLS" "libxml2"
                   "ImageMagick" "SQLite"))
      (let ((re (concat "^- " (regexp-quote cap) ": \\(yes\\|no\\)")))
        (should (string-match-p re text))))))

(provide 'test-config-utilities--summary-string)
;;; test-config-utilities--summary-string.el ends here
