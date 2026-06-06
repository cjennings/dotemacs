;;; test-org-capture-config-project-target.el --- Project-aware capture tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the project-aware capture target shared by C-c c t (Task) and
;; C-c c b (Bug): the pure project-name and target-decision helpers, the
;; find-or-create "Open Work" / "Inbox" heading helpers, the function-target
;; wiring, and the two template registrations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-capture)
(require 'user-constants)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

;;; cj/--org-capture-project-name

(ert-deftest test-org-capture-project-name-normal ()
  "Normal: basename, first letter upcased; trailing slash ignored."
  (should (equal (cj/--org-capture-project-name "/home/cj/code/duet/") "Duet"))
  (should (equal (cj/--org-capture-project-name "/home/cj/code/duet") "Duet")))

(ert-deftest test-org-capture-project-name-strips-leading-dot ()
  "Boundary: a single leading dot is stripped before upcasing."
  (should (equal (cj/--org-capture-project-name "/home/cj/.emacs.d/") "Emacs.d")))

(ert-deftest test-org-capture-project-name-nil-and-empty ()
  "Error: nil or empty root yields nil."
  (should-not (cj/--org-capture-project-name nil))
  (should-not (cj/--org-capture-project-name "")))

;;; cj/--org-capture-project-target

(ert-deftest test-org-capture-target-project-with-todo ()
  "Normal: a projectile root whose todo.org exists targets that file's Open Work."
  (let ((root (make-temp-file "captest-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "todo.org" root)
            (insert "* X Open Work\n"))
          (let ((plan (cj/--org-capture-project-target root "/tmp/inbox.org")))
            (should (string= (plist-get plan :file)
                             (expand-file-name "todo.org" root)))
            (should (plist-get plan :open-work))
            (should-not (plist-get plan :warn))))
      (delete-directory root t))))

(ert-deftest test-org-capture-target-project-without-todo ()
  "Boundary: a projectile root with no todo.org falls back to inbox and warns."
  (let ((root (make-temp-file "captest-" t)))
    (unwind-protect
        (let ((plan (cj/--org-capture-project-target root "/tmp/inbox.org")))
          (should (string= (plist-get plan :file) "/tmp/inbox.org"))
          (should-not (plist-get plan :open-work))
          (should (stringp (plist-get plan :warn)))
          (should (string-match-p (regexp-quote (cj/--org-capture-project-name root))
                                  (plist-get plan :warn))))
      (delete-directory root t))))

(ert-deftest test-org-capture-target-no-project ()
  "Boundary: nil root targets the inbox with no warning."
  (let ((plan (cj/--org-capture-project-target nil "/tmp/inbox.org")))
    (should (string= (plist-get plan :file) "/tmp/inbox.org"))
    (should-not (plist-get plan :open-work))
    (should-not (plist-get plan :warn))))

;;; cj/--org-capture-goto-open-work

(ert-deftest test-org-capture-goto-open-work-finds-existing ()
  "Normal: an existing top-level \"... Open Work\" heading is reused, not duplicated."
  (with-temp-buffer
    (org-mode)
    (insert "* Emacs Open Work\n** TODO a\n* Emacs Resolved\n")
    (cj/--org-capture-goto-open-work "Ignored")
    (should (string= (org-get-heading t t t t) "Emacs Open Work"))
    (should-not (string-match-p "Ignored" (buffer-string)))))

(ert-deftest test-org-capture-goto-open-work-matches-tagged-heading ()
  "Boundary: a tagged \"... Open Work\" heading still matches and is not duplicated."
  (with-temp-buffer
    (org-mode)
    (insert "* Foo Open Work :stuff:\n")
    (cj/--org-capture-goto-open-work "Bar")
    (should (string-match-p "Open Work" (org-get-heading t t t t)))
    (should-not (string-match-p "Bar Open Work" (buffer-string)))))

(ert-deftest test-org-capture-goto-open-work-creates-when-absent ()
  "Boundary: with no Open Work heading, create \"* NAME Open Work\" at end."
  (with-temp-buffer
    (org-mode)
    (insert "* Something Else\n")
    (cj/--org-capture-goto-open-work "Duet")
    (should (string-match-p "^\\* Duet Open Work$" (buffer-string)))
    (should (string= (org-get-heading t t t t) "Duet Open Work"))))

;;; cj/--org-capture-goto-exact-headline

(ert-deftest test-org-capture-goto-exact-headline-finds ()
  "Normal: an existing Inbox heading is found."
  (with-temp-buffer
    (org-mode)
    (insert "* Inbox\n** TODO x\n")
    (cj/--org-capture-goto-exact-headline "Inbox")
    (should (string= (org-get-heading t t t t) "Inbox"))))

(ert-deftest test-org-capture-goto-exact-headline-creates ()
  "Boundary: a missing Inbox heading is created at end of buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* Other\n")
    (cj/--org-capture-goto-exact-headline "Inbox")
    (should (string-match-p "^\\* Inbox$" (buffer-string)))))

;;; cj/--org-capture-project-location (function-target wiring)

(ert-deftest test-org-capture-location-files-into-project-open-work ()
  "Integration: in a project with a todo.org, the location function visits that
file and lands point on its Open Work heading."
  (let* ((root (make-temp-file "captest-" t))
         (todo (expand-file-name "todo.org" root))
         (org-capture-plist nil)
         visited)
    (unwind-protect
        (progn
          (with-temp-file todo (insert "* Captest Open Work\n** TODO old\n"))
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda (&optional _d) root)))
            (cj/--org-capture-project-location)
            (setq visited (current-buffer))
            (should (string= (buffer-file-name) todo))
            (should (string-match-p "Open Work" (org-get-heading t t t t)))))
      (when (buffer-live-p visited) (kill-buffer visited))
      (delete-directory root t))))

(ert-deftest test-org-capture-location-falls-back-to-inbox-without-project ()
  "Integration: with no project, the location function visits the inbox file
under its Inbox heading."
  (let* ((inbox (make-temp-file "captest-inbox-" nil ".org" "* Inbox\n"))
         (inbox-file inbox)
         (org-capture-plist nil)
         visited)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-project-root)
                   (lambda (&optional _d) nil)))
          (cj/--org-capture-project-location)
          (setq visited (current-buffer))
          (should (string= (buffer-file-name) inbox))
          (should (string= (org-get-heading t t t t) "Inbox")))
      (when (buffer-live-p visited) (kill-buffer visited))
      (delete-file inbox))))

;;; templates

(ert-deftest test-org-capture-task-template-is-project-aware ()
  "Normal: the Task template (t) targets the project-aware function."
  (let ((entry (assoc "t" org-capture-templates)))
    (should entry)
    (should (equal (nth 3 entry)
                   '(function cj/--org-capture-project-location)))))

(ert-deftest test-org-capture-bug-template-registered ()
  "Normal: the Bug template (b) exists, targets the project-aware function, and
defaults to the [#C] priority."
  (let ((entry (assoc "b" org-capture-templates)))
    (should entry)
    (should (equal (nth 3 entry)
                   '(function cj/--org-capture-project-location)))
    (should (string-match-p "\\[#C\\]" (nth 4 entry)))))

(provide 'test-org-capture-config-project-target)
;;; test-org-capture-config-project-target.el ends here
