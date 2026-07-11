;;; test-local-repository.el --- Tests for the local-repository update command -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/update-localrepo-repository' refreshes the checked-in local package
;; archive at `localrepo-location' (owned by early-init.el) via elpa-mirror.
;; The elpa-mirror call is mocked at the boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'local-repository)

;; localrepo-location is a defconst in early-init.el, which `make test' never
;; loads.  Declare it special here so the `let' below binds it dynamically and
;; the module reads that value.
(defvar localrepo-location nil)

(ert-deftest test-local-repository-update-targets-early-init-location ()
  "Normal: the mirror update targets `localrepo-location', the single archive
path early-init.el owns, not a divergent module-local copy."
  (let ((localrepo-location "/tmp/test-localrepo/")
        (captured nil))
    (cl-letf (((symbol-function 'elpamr-create-mirror-for-installed)
               (lambda (dir &rest _) (setq captured dir))))
      (cj/update-localrepo-repository)
      (should (equal captured "/tmp/test-localrepo/")))))

(provide 'test-local-repository)
;;; test-local-repository.el ends here
