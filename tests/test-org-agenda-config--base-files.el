;;; test-org-agenda-config--base-files.el --- Tests for the agenda base-file helper -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--org-agenda-base-files is the single source of the fixed agenda base list
;; (inbox, schedule, and the three calendars) that was previously spelled out as
;; a literal in three places.  It now drops files that do not exist so org-agenda
;; never prompts to create a missing path (the hang class).  The path vars are
;; special (defvar'd in user-constants), so they can be dynamically bound; tests
;; use real temp files for "exists" rather than mocking the `file-exists-p'
;; primitive.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

(defun test-oa-base--tmp ()
  "Return a fresh existing temp file path."
  (make-temp-file "oa-base-"))

(ert-deftest test-org-agenda-base-files-returns-existing-in-order ()
  "Normal: returns inbox, schedule, gcal, pcal, dcal (all existing) in order."
  (let* ((i (test-oa-base--tmp)) (s (test-oa-base--tmp)) (g (test-oa-base--tmp))
         (p (test-oa-base--tmp)) (d (test-oa-base--tmp))
         (inbox-file i) (schedule-file s) (gcal-file g) (pcal-file p) (dcal-file d))
    (unwind-protect
        (should (equal (cj/--org-agenda-base-files) (list i s g p d)))
      (dolist (f (list i s g p d)) (ignore-errors (delete-file f))))))

(ert-deftest test-org-agenda-base-files-reflects-current-values ()
  "Boundary: the helper reads the vars at call time (not a captured snapshot)."
  (let* ((a (test-oa-base--tmp)) (b (test-oa-base--tmp))
         (inbox-file a) (schedule-file b) (gcal-file b) (pcal-file b) (dcal-file b))
    (unwind-protect
        (progn
          (should (equal (car (cj/--org-agenda-base-files)) a))
          (setq inbox-file b)
          (should (equal (car (cj/--org-agenda-base-files)) b))
          (should (= (length (cj/--org-agenda-base-files)) 5)))
      (ignore-errors (delete-file a))
      (ignore-errors (delete-file b)))))

(ert-deftest test-org-agenda-base-files-drops-missing-files ()
  "Boundary/Error: files that do not exist are dropped, so a fresh machine
without synced calendars never hands org-agenda a path it would prompt to create."
  (let* ((i (test-oa-base--tmp)) (s (test-oa-base--tmp))
         (inbox-file i) (schedule-file s)
         (gcal-file "/no/such/gcal.org")
         (pcal-file "/no/such/pcal.org")
         (dcal-file "/no/such/dcal.org"))
    (unwind-protect
        (should (equal (cj/--org-agenda-base-files) (list i s)))
      (ignore-errors (delete-file i))
      (ignore-errors (delete-file s)))))

(provide 'test-org-agenda-config--base-files)
;;; test-org-agenda-config--base-files.el ends here
