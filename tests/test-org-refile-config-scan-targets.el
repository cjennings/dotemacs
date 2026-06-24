;;; test-org-refile-config-scan-targets.el --- Tests for the refile scanner -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover `cj/org-refile-refresh-targets', `cj/org-refile',
;; and `cj/org-refile-in-file'.  This file covers
;; `cj/--org-refile-scan-targets', the disk-walking helper that builds
;; the targets list.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-refile-config)

;; The scanner reads several globals defined by `user-constants'.  Provide
;; top-level defvars so let-binds become dynamic under lexical scope.
(defvar inbox-file "/tmp/test-inbox.org")
(defvar reference-file "/tmp/test-reference.org")
(defvar schedule-file "/tmp/test-schedule.org")
(defvar code-dir nil)
(defvar projects-dir nil)

;;; cj/--org-refile-scan-targets

(ert-deftest test-org-refile-scan-targets-builds-base-list ()
  "Normal: the scanner returns inbox/reference/schedule entries with their
maxlevel rules when no roam tags and no code/projects todo files exist."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-refile-empty-" t)))
         (inbox-file "/tmp/test-inbox.org")
         (reference-file "/tmp/test-reference.org")
         (schedule-file "/tmp/test-schedule.org")
         (code-dir tmp)
         (projects-dir tmp))
    (unwind-protect
        (let ((result (cj/--org-refile-scan-targets)))
          (should (member (cons inbox-file '(:maxlevel . 1)) result))
          (should (member (cons reference-file '(:maxlevel . 2)) result))
          (should (member (cons schedule-file '(:maxlevel . 1)) result)))
      (delete-directory tmp t))))

(ert-deftest test-org-refile-scan-targets-picks-up-todo-files ()
  "Normal: nested `todo.org' files under code-dir / projects-dir are added."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-refile-todo-" t)))
         (sub (expand-file-name "alpha" tmp))
         (todo (expand-file-name "todo.org" sub))
         (inbox-file "/tmp/test-inbox.org")
         (reference-file "/tmp/test-reference.org")
         (schedule-file "/tmp/test-schedule.org")
         (code-dir tmp)
         (projects-dir tmp))
    (make-directory sub t)
    (with-temp-file todo (insert "* heading\n"))
    (unwind-protect
        (let ((result (cj/--org-refile-scan-targets)))
          (should (member (cons todo '(:maxlevel . 1)) result)))
      (delete-directory tmp t))))

(ert-deftest test-org-refile-scan-targets-skips-airootfs-trees ()
  "Boundary: subtrees named airootfs are pruned during the walk."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-refile-airoot-" t)))
         (skipped-dir (expand-file-name "airootfs" tmp))
         (skipped-todo (expand-file-name "todo.org" skipped-dir))
         (kept-dir (expand-file-name "regular" tmp))
         (kept-todo (expand-file-name "todo.org" kept-dir))
         (inbox-file "/tmp/test-inbox.org")
         (reference-file "/tmp/test-reference.org")
         (schedule-file "/tmp/test-schedule.org")
         (code-dir tmp)
         (projects-dir tmp))
    (make-directory skipped-dir t)
    (make-directory kept-dir t)
    (with-temp-file skipped-todo (insert "* skip me\n"))
    (with-temp-file kept-todo (insert "* keep me\n"))
    (unwind-protect
        (let* ((result (cj/--org-refile-scan-targets))
               (paths (mapcar #'car result)))
          (should (member kept-todo paths))
          (should-not (member skipped-todo paths)))
      (delete-directory tmp t))))

(ert-deftest test-org-refile-scan-targets-deduplicates-todo-paths ()
  "Boundary: the same `todo.org' reached via two scan dirs appears once."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-refile-dup-" t)))
         (sub (expand-file-name "shared" tmp))
         (todo (expand-file-name "todo.org" sub))
         (inbox-file "/tmp/test-inbox.org")
         (reference-file "/tmp/test-reference.org")
         (schedule-file "/tmp/test-schedule.org")
         ;; Point both directory variables at the same root so the walk runs
         ;; twice over the same todo file.
         (code-dir tmp)
         (projects-dir tmp))
    (make-directory sub t)
    (with-temp-file todo (insert "* heading\n"))
    (unwind-protect
        (let* ((result (cj/--org-refile-scan-targets))
               (hits (cl-count-if (lambda (entry) (equal (car entry) todo))
                                  result)))
          (should (= 1 hits)))
      (delete-directory tmp t))))

(ert-deftest test-org-refile-scan-targets-includes-roam-topic-not-project ()
  "Normal: roam Topic files become refile targets; Project files do NOT.
Project notes were dropped as refile targets (2026-06-24) -- roam Projects are
no longer scanned for refile."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-refile-roam-" t)))
         (inbox-file "/tmp/test-inbox.org")
         (reference-file "/tmp/test-reference.org")
         (schedule-file "/tmp/test-schedule.org")
         (code-dir tmp)
         (projects-dir tmp))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/org-roam-list-notes-by-tag)
                   (lambda (tag)
                     (cond
                      ((equal tag "Project") '("/notes/alpha.org"))
                      ((equal tag "Topic")   '("/notes/topic.org"))
                      (t nil))))
                  ((symbol-function 'org-roam-node-list)
                   (lambda () nil)))
          (let* ((result (cj/--org-refile-scan-targets))
                 (paths (mapcar #'car result)))
            (should (member "/notes/topic.org" paths))
            (should-not (member "/notes/alpha.org" paths))))
      (delete-directory tmp t))))

(ert-deftest test-org-refile-scan-targets-survives-permission-denied ()
  "Boundary: a permission-denied error during `directory-files-recursively'
is caught and the scan still returns the base list."
  (let* ((inbox-file "/tmp/test-inbox.org")
         (reference-file "/tmp/test-reference.org")
         (schedule-file "/tmp/test-schedule.org")
         (code-dir "/nope/code")
         (projects-dir "/nope/projects"))
    (cl-letf (((symbol-function 'directory-files-recursively)
               (lambda (&rest _) (signal 'permission-denied nil))))
      (let ((result (cj/--org-refile-scan-targets)))
        (should (member (cons inbox-file '(:maxlevel . 1)) result))))))

(provide 'test-org-refile-config-scan-targets)
;;; test-org-refile-config-scan-targets.el ends here
