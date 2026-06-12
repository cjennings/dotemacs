;;; test-org-roam-config-dailies-head.el --- Tests for the dailies template head -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--org-roam-dailies-head' is the head inserted into a new org-roam
;; daily file.  #+FILETAGS and #+TITLE must sit on separate lines, or Org
;; never parses the #+TITLE keyword and the FILETAGS value swallows the
;; rest of the line.

;;; Code:

(require 'ert)
(require 'testutil-general)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-roam-config)

(ert-deftest test-org-roam-config-dailies-head-separates-filetags-and-title ()
  "Boundary: #+FILETAGS and #+TITLE sit on separate lines."
  (should (string-match-p "#\\+FILETAGS: Journal\n#\\+TITLE:"
                          cj/--org-roam-dailies-head))
  ;; And never run together on one line.
  (should-not (string-match-p "Journal #\\+TITLE:" cj/--org-roam-dailies-head)))

(ert-deftest test-org-roam-config-dailies-head-ends-with-newline ()
  "Boundary: the head ends with a newline so the capture body starts clean."
  (should (string-suffix-p "\n" cj/--org-roam-dailies-head)))

(provide 'test-org-roam-config-dailies-head)
;;; test-org-roam-config-dailies-head.el ends here
