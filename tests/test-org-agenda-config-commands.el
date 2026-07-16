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

;;; org-agenda-custom-commands "d" daily structure

(defun test-org-agenda--daily-blocks ()
  "Return the block list of the \"d\" daily agenda command."
  (nth 2 (assoc "d" org-agenda-custom-commands)))

(ert-deftest test-org-agenda-daily-schedule-block-is-first ()
  "Normal: the schedule (calendar) block leads the daily agenda."
  (should (eq (car (nth 0 (test-org-agenda--daily-blocks))) 'agenda)))

(ert-deftest test-org-agenda-daily-has-no-overdue-block ()
  "Normal: no overdue block.  It duplicated the past-due
scheduled/deadline items the schedule block already surfaces on
today's line (org-scheduled-past-days/org-deadline-past-days are
large), so the standalone OVERDUE section was redundant."
  (let ((flat (flatten-tree (test-org-agenda--daily-blocks))))
    (should-not (memq 'cj/org-agenda-skip-subtree-if-not-overdue flat))))

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

(ert-deftest test-org-agenda-add-timestamp-preserves-point-and-following-line ()
  "Normal: the stamp lands between the entry and the next line, point unmoved.
The docstring promises the event appears \"underneath the line-at-point\",
so neither the entry nor whatever follows it may be disturbed."
  (with-temp-buffer
    (insert "* First\n* Second")
    (goto-char (point-min))
    (let ((start (progn (org-end-of-line) (point))))
      (goto-char (point-min))
      (cj/add-timestamp-to-org-entry "09:00")
      ;; Point is left at the end of the entry it stamped.
      (should (= (point) start)))
    (let ((lines (split-string (buffer-string) "\n")))
      (should (equal (nth 0 lines) "* First"))
      (should (string-match-p "\\`<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} 09:00>\\'"
                              (nth 1 lines)))
      (should (equal (nth 2 lines) "* Second")))))

(ert-deftest test-org-agenda-add-timestamp-empty-time-string ()
  "Boundary: an empty time yields a bare date stamp with a trailing space.
Characterizes current behavior -- the separator space is unconditional, so
an empty S produces `<DATE >' rather than `<DATE>'.  Harmless in an agenda
\(org reads the date\), and pinned here so a future format change is a
deliberate one."
  (with-temp-buffer
    (insert "* Heading here")
    (goto-char (point-min))
    (cj/add-timestamp-to-org-entry "")
    (should (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} >"
                            (buffer-string)))))

(ert-deftest test-org-agenda-add-timestamp-unicode-time-string ()
  "Boundary: non-ASCII in S survives into the stamp uncorrupted."
  (with-temp-buffer
    (insert "* Heading here")
    (goto-char (point-min))
    (cj/add-timestamp-to-org-entry "09:00 café ☕")
    (should (string-match-p "09:00 café ☕>" (buffer-string)))))

(ert-deftest test-org-agenda-add-timestamp-empty-buffer ()
  "Boundary: an empty buffer still gets a stamp rather than signalling.
`org-end-of-line' and `open-line' both no-op safely at point-min."
  (with-temp-buffer
    (cj/add-timestamp-to-org-entry "09:00")
    (should (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} 09:00>"
                            (buffer-string)))))

(ert-deftest test-org-agenda-add-timestamp-read-only-buffer-signals ()
  "Error: a read-only buffer signals rather than silently dropping the stamp."
  (with-temp-buffer
    (insert "* Heading")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (should-error (cj/add-timestamp-to-org-entry "09:00") :type 'buffer-read-only)))

(defconst test-org-agenda--timeformat-special-at-load
  (special-variable-p 'cj/timeformat)
  "Whether `cj/timeformat' was special immediately after loading the module.
Captured here at load, before any test body runs, because the fact under
test is destroyed by observing it late: calling
`cj/add-timestamp-to-org-entry' once would execute a `defvar' nested in the
defun and make the symbol special retroactively.  ERT runs tests
alphabetically, so an in-test `special-variable-p' check passes on the
strength of whichever test ran first -- green in a full-file run, red in
isolation.  Snapshotting at load makes the guard order-independent.")

(ert-deftest test-org-agenda-timeformat-is-a-top-level-special-variable ()
  "Normal: `cj/timeformat' is special and bound at load, not first call.
It used to be `defvar'd inside `cj/add-timestamp-to-org-entry', so it was
unbound until the command ran once and a `let' around the call bound it
lexically instead of dynamically.  Pinning both halves of the fix: the
symbol is special at load time, and rebinding it actually reaches the
command."
  (should test-org-agenda--timeformat-special-at-load)
  (should (equal (default-value 'cj/timeformat) "%Y-%m-%d %a"))
  ;; The dynamic binding must reach the insertion.
  (with-temp-buffer
    (insert "* Heading")
    (goto-char (point-min))
    (let ((cj/timeformat "%Y"))
      (cj/add-timestamp-to-org-entry "09:00"))
    (should (string-match-p "\\`<[0-9]\\{4\\} 09:00>\\'"
                            (nth 1 (split-string (buffer-string) "\n"))))))

(provide 'test-org-agenda-config-commands)
;;; test-org-agenda-config-commands.el ends here
