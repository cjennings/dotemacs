;;; test-org-drill-config-source.el --- Tests for org-drill source selection -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--org-drill-source-keywords decides how use-package obtains org-drill:
;; a local dev checkout via :load-path when it exists, otherwise the upstream
;; repo via :vc.  Without this, a machine lacking the checkout hits :demand t
;; against a nonexistent :load-path and drill fails to load entirely.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-drill-config)

(ert-deftest test-org-drill-source-keywords-uses-load-path-when-checkout-exists ()
  "Normal: an existing checkout directory selects :load-path."
  (let ((dir (make-temp-file "org-drill-checkout-" t)))
    (unwind-protect
        (should (equal (cj/--org-drill-source-keywords dir)
                       (list :load-path dir)))
      (delete-directory dir t))))

(ert-deftest test-org-drill-source-keywords-falls-back-to-vc-when-absent ()
  "Error: a missing checkout falls back to a :vc install spec."
  (let ((kws (cj/--org-drill-source-keywords "/nonexistent/org-drill-xyz")))
    (should (eq (car kws) :vc))
    (should (plist-get (nth 1 kws) :url))))

(provide 'test-org-drill-config-source)
;;; test-org-drill-config-source.el ends here
