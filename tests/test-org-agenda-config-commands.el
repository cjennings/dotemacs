;;; test-org-agenda-config-commands.el --- Tests for org-agenda command wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover skip-functions, category, display, add-files,
;; and build-list.  This file covers the remaining wrapper commands:
;;
;;   cj/--org-agenda-scan-files
;;   cj/org-agenda-refresh-files
;;   cj/todo-list-all-agenda-files
;;   cj/todo-list-single-project
;;   cj/todo-list-from-this-buffer
;;   cj/main-agenda-display
;;   cj/add-timestamp-to-org-entry

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

;; Top-level defvars so let-binds reach the dynamic variable under
;; lexical scope.
(defvar inbox-file "/tmp/test-inbox.org")
(defvar schedule-file "/tmp/test-schedule.org")
(defvar gcal-file "/tmp/test-gcal.org")
(defvar pcal-file "/tmp/test-pcal.org")
(defvar dcal-file "/tmp/test-dcal.org")
(defvar projects-dir "/tmp/test-projects/")
(defvar org-agenda-files nil)

;;; cj/--org-agenda-scan-files

(ert-deftest test-org-agenda-scan-files-includes-base-files ()
  "Normal: the scanner seeds the list with the inbox/schedule/cal globals."
  (cl-letf (((symbol-function 'cj/add-files-to-org-agenda-files-list) #'ignore))
    (let ((result (cj/--org-agenda-scan-files)))
      (should (member inbox-file result))
      (should (member schedule-file result))
      (should (member gcal-file result))
      (should (member pcal-file result))
      (should (member dcal-file result)))))

(ert-deftest test-org-agenda-scan-files-includes-project-additions ()
  "Normal: anything the projects walker pushes onto `org-agenda-files'
ends up in the returned list."
  (cl-letf (((symbol-function 'cj/add-files-to-org-agenda-files-list)
             (lambda (_dir)
               (push "/tmp/test-projects/alpha/todo.org" org-agenda-files))))
    (let ((result (cj/--org-agenda-scan-files)))
      (should (member "/tmp/test-projects/alpha/todo.org" result)))))

;;; cj/org-agenda-refresh-files

(ert-deftest test-org-agenda-refresh-files-forces-rebuild ()
  "Normal: refresh-files calls build with force-rebuild flag."
  (let (received-flag)
    (cl-letf (((symbol-function 'cj/build-org-agenda-list)
               (lambda (&optional flag) (setq received-flag flag))))
      (cj/org-agenda-refresh-files))
    (should (eq received-flag 'force-rebuild))))

;;; cj/todo-list-all-agenda-files

(ert-deftest test-org-agenda-todo-list-all-routes-through-a-t ()
  "Normal: todo-list-all-agenda-files builds the list then calls org-agenda \"a\" \"t\"."
  (let (build-called agenda-args)
    (cl-letf (((symbol-function 'cj/build-org-agenda-list)
               (lambda (&optional _) (setq build-called t)))
              ((symbol-function 'org-agenda)
               (lambda (&rest args) (setq agenda-args args))))
      (cj/todo-list-all-agenda-files))
    (should build-called)
    (should (equal agenda-args '("a" "t")))))

;;; cj/todo-list-single-project

(ert-deftest test-org-agenda-todo-list-single-project-scopes-to-chosen-project ()
  "Normal: single-project narrows `org-agenda-files' to the chosen
project's todo.org plus the calendar/inbox files, then routes \"a\" \"d\"."
  (let* ((tmp (file-name-as-directory (make-temp-file "cj-agenda-proj-" t)))
         (alpha (expand-file-name "alpha" tmp))
         (alpha-todo (expand-file-name "todo.org" alpha))
         (projects-dir tmp)
         seen-files agenda-args)
    (make-directory alpha t)
    (with-temp-file alpha-todo (insert "* heading\n"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "alpha"))
                    ((symbol-function 'org-agenda)
                     (lambda (&rest args)
                       (setq seen-files org-agenda-files
                             agenda-args args))))
            (cj/todo-list-single-project))
          (should (equal agenda-args '("a" "d")))
          (should (member alpha-todo seen-files))
          (should (member inbox-file seen-files)))
      (delete-directory tmp t))))

;;; cj/todo-list-from-this-buffer

(ert-deftest test-org-agenda-todo-list-from-org-buffer-uses-buffer-file ()
  "Normal: in an org-mode buffer, the agenda is scoped to its file."
  (let (seen-files agenda-args)
    (cl-letf (((symbol-function 'org-agenda)
               (lambda (&rest args)
                 (setq seen-files org-agenda-files
                       agenda-args args))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/notes.org")
        (org-mode)
        (cj/todo-list-from-this-buffer)))
    (should (equal agenda-args '("a" "t")))
    (should (equal seen-files (list "/tmp/notes.org")))))

(ert-deftest test-org-agenda-todo-list-from-non-org-buffer-messages ()
  "Boundary: a non-org buffer routes through `message' instead of
calling `org-agenda'."
  (let (msg called-agenda)
    (cl-letf (((symbol-function 'org-agenda)
               (lambda (&rest _) (setq called-agenda t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (with-temp-buffer
        (fundamental-mode)
        (cj/todo-list-from-this-buffer)))
    (should-not called-agenda)
    (should (string-match-p "not an org buffer" msg))))

;;; cj/main-agenda-display

(ert-deftest test-org-agenda-main-display-builds-then-routes-a-d ()
  "Normal: main-agenda-display builds the agenda list then calls
`org-agenda' with the \"a\" \"d\" keys."
  (let (build-called agenda-args)
    (cl-letf (((symbol-function 'cj/build-org-agenda-list)
               (lambda (&optional _) (setq build-called t)))
              ((symbol-function 'org-agenda)
               (lambda (&rest args) (setq agenda-args args))))
      (cj/main-agenda-display))
    (should build-called)
    (should (equal agenda-args '("a" "d")))))

;;; cj/add-timestamp-to-org-entry

(ert-deftest test-org-agenda-add-timestamp-inserts-on-next-line ()
  "Normal: add-timestamp inserts a `<date time>' line below the cursor."
  (with-temp-buffer
    (insert "First line content")
    (goto-char (point-min))
    (cj/add-timestamp-to-org-entry "09:00")
    (let ((text (buffer-string)))
      ;; Inserted line carries the time we passed and angle brackets.
      (should (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} 09:00>"
                              text)))))

(provide 'test-org-agenda-config-commands)
;;; test-org-agenda-config-commands.el ends here
