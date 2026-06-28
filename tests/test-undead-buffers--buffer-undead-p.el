;;; test-undead-buffers--buffer-undead-p.el --- undead predicate (name + regexp) -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--buffer-undead-p' decides whether a buffer name is buried instead of
;; killed.  A name is undead when it is in `cj/undead-buffer-list' (exact) or
;; matches any regexp in `cj/undead-buffer-regexps' (dynamic families like the
;; ai-term agent buffers, named "agent [<project>]").  `cj/make-buffer-pattern-undead'
;; registers a regexp.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'undead-buffers)

(ert-deftest test-undead-buffer-undead-p-name-list ()
  "Normal: a name in the exact list is undead; others are not."
  (let ((cj/undead-buffer-list '("*scratch*" "*Messages*"))
        (cj/undead-buffer-regexps nil))
    (should (cj/--buffer-undead-p "*scratch*"))
    (should (cj/--buffer-undead-p "*Messages*"))
    (should-not (cj/--buffer-undead-p "other"))))

(ert-deftest test-undead-buffer-undead-p-regexp ()
  "Normal: a name matching a regexp is undead; the agent pattern is anchored."
  (let ((cj/undead-buffer-list nil)
        (cj/undead-buffer-regexps '("\\`agent \\[")))
    (should (cj/--buffer-undead-p "agent [rulesets]"))
    (should (cj/--buffer-undead-p "agent [.emacs.d]"))
    (should-not (cj/--buffer-undead-p "not an agent"))
    (should-not (cj/--buffer-undead-p "my agent [x]"))))   ; anchored: must start with "agent ["

(ert-deftest test-undead-buffer-undead-p-neither ()
  "Boundary/Error: a name in neither, an empty string, and a non-string are not undead."
  (let ((cj/undead-buffer-list '("*scratch*"))
        (cj/undead-buffer-regexps '("\\`agent \\[")))
    (should-not (cj/--buffer-undead-p "random"))
    (should-not (cj/--buffer-undead-p ""))
    (should-not (cj/--buffer-undead-p nil))))

(ert-deftest test-undead-make-buffer-pattern-undead-adds-and-rejects ()
  "Normal/Error: registering a regexp makes matching names undead; a blank or
non-string regexp signals."
  (let ((cj/undead-buffer-regexps nil))
    (cj/make-buffer-pattern-undead "\\`agent \\[")
    (should (member "\\`agent \\[" cj/undead-buffer-regexps))
    (should (cj/--buffer-undead-p "agent [x]"))
    (should-error (cj/make-buffer-pattern-undead ""))
    (should-error (cj/make-buffer-pattern-undead 42))))

(provide 'test-undead-buffers--buffer-undead-p)
;;; test-undead-buffers--buffer-undead-p.el ends here
