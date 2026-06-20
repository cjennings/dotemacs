;;; test-org-agenda-config--base-files.el --- Tests for the agenda base-file helper -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--org-agenda-base-files is the single source of the fixed agenda base list
;; (inbox, schedule, and the three calendars) that was previously spelled out as
;; a literal in three places.  The path vars are special (defvar'd in
;; user-constants), so they can be dynamically bound here.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

(ert-deftest test-org-agenda-base-files-returns-fixed-list-in-order ()
  "Normal: returns inbox, schedule, gcal, pcal, dcal in that order."
  (let ((inbox-file "/i")
        (schedule-file "/s")
        (gcal-file "/g")
        (pcal-file "/p")
        (dcal-file "/d"))
    (should (equal (cj/--org-agenda-base-files)
                   '("/i" "/s" "/g" "/p" "/d")))))

(ert-deftest test-org-agenda-base-files-reflects-current-values ()
  "Boundary: the helper reads the vars at call time (not a captured snapshot)."
  (let ((inbox-file "first")
        (schedule-file "x") (gcal-file "x") (pcal-file "x") (dcal-file "x"))
    (should (equal (car (cj/--org-agenda-base-files)) "first"))
    (setq inbox-file "second")
    (should (equal (car (cj/--org-agenda-base-files)) "second"))
    (should (= (length (cj/--org-agenda-base-files)) 5))))

(provide 'test-org-agenda-config--base-files)
;;; test-org-agenda-config--base-files.el ends here
