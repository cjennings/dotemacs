;;; test-org-capture-config-drill-template.el --- Tests for drill capture templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the drill org-capture template remains well-formed.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

(ert-deftest test-org-capture-config-drill-template-source-link-is-closed ()
  "The regular drill capture template should emit a valid source link."
  (let ((template (nth 4 (assoc "d" org-capture-templates))))
    (should (stringp template))
    (should (string-match-p
             "Source: \\[\\[%:link\\]\\[%:description\\]\\]"
             template))
    (should (string-match-p "\nCaptured On: %U" template))
    (should-not (string-match-p "\\[%:description\\]\n" template))
    (should-not (string-match-p "\nnCaptured On: %U" template))))

(provide 'test-org-capture-config-drill-template)
;;; test-org-capture-config-drill-template.el ends here
