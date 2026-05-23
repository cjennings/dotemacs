;;; test-org-babel-config-structure-templates.el --- Org Tempo template tests -*- lexical-binding: t; -*-

;;; Commentary:
;; org-babel-config.el registers Org Tempo structure templates (e.g. `<el'
;; expands to an emacs-lisp src block) by adding entries to
;; `org-structure-template-alist' in org-tempo's :config.  A typo in the
;; value expands to a bogus language name (a `<java' that produced
;; "#+begin_src javas").  Requiring the module then org-tempo fires that
;; config in batch, so this test pins the alias-to-language mapping for the
;; common aliases.

;;; Code:

(require 'ert)
(require 'org)
(require 'ob-core)
(require 'org-babel-config)
(require 'org-tempo)

(ert-deftest test-org-babel-structure-templates-map-to-languages ()
  "Normal: each common alias maps to its intended src language name."
  (dolist (pair '(("bash" . "src bash")
                  ("zsh"  . "src zsh")
                  ("el"   . "src emacs-lisp")
                  ("py"   . "src python")
                  ("json" . "src json")
                  ("yaml" . "src yaml")
                  ("java" . "src java")))
    (should (equal (assoc (car pair) org-structure-template-alist) pair))))

(provide 'test-org-babel-config-structure-templates)
;;; test-org-babel-config-structure-templates.el ends here
