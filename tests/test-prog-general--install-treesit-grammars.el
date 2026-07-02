;;; test-prog-general--install-treesit-grammars.el --- Grammar bootstrap command -*- lexical-binding: t; -*-

;;; Commentary:
;; `treesit-auto-install' is set to `prompt' so opening a file never silently
;; downloads and builds a grammar.  `cj/install-treesit-grammars' is the
;; explicit fresh-machine bootstrap: it loads treesit-auto and installs every
;; grammar in one deliberate command.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

;; Satisfy the command's (require 'treesit-auto) without the real package,
;; which isn't available in batch runs.
(provide 'treesit-auto)

(ert-deftest test-prog-general-install-treesit-grammars-is-a-command ()
  "Normal: the bootstrap entry point exists and is interactive."
  (should (commandp 'cj/install-treesit-grammars)))

(ert-deftest test-prog-general-install-treesit-grammars-installs-all ()
  "Normal: the command delegates to treesit-auto-install-all."
  (let ((called 0))
    (cl-letf (((symbol-function 'treesit-auto-install-all)
               (lambda (&rest _) (setq called (1+ called)))))
      (cj/install-treesit-grammars)
      (should (= called 1)))))

(provide 'test-prog-general--install-treesit-grammars)
;;; test-prog-general--install-treesit-grammars.el ends here
