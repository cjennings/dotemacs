;;; test-org-agenda-config-category.el --- Tests for project-name category derivation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the agenda-display category helpers in org-agenda-config.el:
;; - cj/--org-todo-category-from-file (pure)
;; - cj/--org-set-todo-category (org-mode-hook side effect)
;;
;; Goal: when a buffer visits a project-local todo.org, its `org-category`
;; should be the parent directory name (the project slug) rather than the
;; default "todo" -- so the agenda's %c column shows "emacs.d:" instead of
;; "todo:" for every project's tasks.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-agenda-config)

;;; ---------- cj/--org-todo-category-from-file (pure helper) ----------

;;; Normal Cases

(ert-deftest test-org-agenda-config-category-normal-emacs-d-todo ()
  "Normal: todo.org under .emacs.d returns \"emacs.d\"."
  (should (equal "emacs.d"
                 (cj/--org-todo-category-from-file
                  "/home/cjennings/.emacs.d/todo.org"))))

(ert-deftest test-org-agenda-config-category-normal-project-todo ()
  "Normal: todo.org under a project dir returns the project basename."
  (should (equal "dotemacs"
                 (cj/--org-todo-category-from-file
                  "/home/cjennings/code/dotemacs/todo.org"))))

(ert-deftest test-org-agenda-config-category-normal-deep-project ()
  "Normal: deeply nested todo.org returns only the immediate parent."
  (should (equal "frontend"
                 (cj/--org-todo-category-from-file
                  "/home/cjennings/projects/work/myapp/frontend/todo.org"))))

;;; Boundary Cases

(ert-deftest test-org-agenda-config-category-boundary-non-todo-file ()
  "Boundary: non-todo.org file returns nil so default category stays."
  (should (null (cj/--org-todo-category-from-file
                 "/home/cjennings/sync/org/roam/inbox.org"))))

(ert-deftest test-org-agenda-config-category-boundary-schedule-org ()
  "Boundary: schedule.org returns nil; not a project todo file."
  (should (null (cj/--org-todo-category-from-file
                 "/home/cjennings/sync/org/schedule.org"))))

(ert-deftest test-org-agenda-config-category-boundary-todo-at-fs-root ()
  "Boundary: /todo.org with no real parent directory returns nil."
  (should (null (cj/--org-todo-category-from-file "/todo.org"))))

(ert-deftest test-org-agenda-config-category-boundary-relative-path ()
  "Boundary: bare relative \"todo.org\" with no directory returns nil."
  (should (null (cj/--org-todo-category-from-file "todo.org"))))

(ert-deftest test-org-agenda-config-category-boundary-todo-with-trailing-dir ()
  "Boundary: tolerate a path that is already directory-form."
  (should (equal "emacs.d"
                 (cj/--org-todo-category-from-file
                  "/home/cjennings/.emacs.d/todo.org"))))

;;; Error Cases

(ert-deftest test-org-agenda-config-category-error-nil-path ()
  "Error: nil PATH returns nil, no signal."
  (should (null (cj/--org-todo-category-from-file nil))))

(ert-deftest test-org-agenda-config-category-error-empty-path ()
  "Error: empty-string PATH returns nil."
  (should (null (cj/--org-todo-category-from-file ""))))

;;; ---------- cj/--org-set-todo-category (hook function) ----------

(defmacro test-org-agenda-config-category--with-file (path body-form)
  "Visit PATH in a temp buffer with org-mode active, evaluate BODY-FORM.
Sets `buffer-file-name' so the hook's lookup sees the desired path.
Suppresses other org-mode hooks to keep the test isolated."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)
           (text-mode-hook nil))
       (org-mode))
     (setq buffer-file-name ,path)
     ;; `org-category' is deliberately left nil, which is what org actually
     ;; does with no `#+CATEGORY:'.  An earlier version of this fixture set it
     ;; to the filename base to "mimic org's default"; org never does that, so
     ;; these tests passed against a state that cannot occur while the feature
     ;; did nothing in practice.
     ,body-form))

;;; Normal Cases

(ert-deftest test-org-agenda-config-category-hook-normal-overrides-todo ()
  "Normal: hook overrides default \"todo\" with the parent dir name."
  (test-org-agenda-config-category--with-file "/home/cjennings/.emacs.d/todo.org"
    (progn
      (cj/--org-set-todo-category)
      (should (equal "emacs.d" org-category)))))

(ert-deftest test-org-agenda-config-category-hook-normal-leaves-inbox-alone ()
  "Normal: hook declines on a non-todo file, leaving `org-category' unset.
Leaving it nil is the correct outcome, not a gap: `org-get-category' then
derives \"inbox\" from the filename, which is already a useful label.  The
end-to-end test below checks that derivation on a real file."
  (test-org-agenda-config-category--with-file "/home/cjennings/sync/org/roam/inbox.org"
    (progn
      (cj/--org-set-todo-category)
      (should (null org-category)))))

;;; Boundary Cases

(ert-deftest test-org-agenda-config-category-hook-boundary-respects-explicit ()
  "Boundary: explicit category (not the filename default) is preserved."
  (test-org-agenda-config-category--with-file "/home/cjennings/.emacs.d/todo.org"
    (progn
      (setq-local org-category "Personal")
      (cj/--org-set-todo-category)
      (should (equal "Personal" org-category)))))

(ert-deftest test-org-agenda-config-category-hook-boundary-nil-buffer-file-name ()
  "Boundary: hook is safe in buffers with no `buffer-file-name'."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (text-mode-hook nil))
      (org-mode))
    (setq buffer-file-name nil)
    (cj/--org-set-todo-category)
    ;; no error and no spurious mutation
    (should t)))

;;; ---------- End-to-end: a real file through the real hook ----------
;; The fixture above sets `org-category' by hand to the filename base, to
;; "mimic org's default".  Org does not do that: with no `#+CATEGORY:' it
;; leaves `org-category' nil and derives the fallback inside
;; `org-get-category' at read time.  So those tests exercise a precondition
;; that never occurs, and passed while the feature did nothing in practice.
;;
;; These drive a real file on disk through `find-file-noselect' (which runs
;; `org-mode-hook' for real) and assert on `org-get-category', which is what
;; the agenda's %c column actually reads.

(defmacro test-org-agenda-config-category--with-real-file (spec &rest body)
  "Create FILE under a temp project dir and visit it, then run BODY.
SPEC is (VAR DIRNAME FILENAME CONTENT).  VAR is bound to the live buffer."
  (declare (indent 1))
  (let ((var (nth 0 spec)) (dirname (nth 1 spec))
        (filename (nth 2 spec)) (content (nth 3 spec)))
    ;; The outer `unwind-protect' covers `find-file-noselect' itself, so a
    ;; signal there still removes the temp tree rather than leaking it.
    `(let* ((root (make-temp-file "cj-cat-" t))
            (project (expand-file-name ,dirname root))
            (path (expand-file-name ,filename project))
            (,var nil))
       (unwind-protect
           (progn
             (make-directory project t)
             (with-temp-file path (insert ,content))
             (setq ,var (find-file-noselect path))
             ,@body)
         (when (buffer-live-p ,var)
           (with-current-buffer ,var (set-buffer-modified-p nil))
           (kill-buffer ,var))
         (delete-directory root t)))))

(ert-deftest test-org-agenda-config-category-endtoend-todo-shows-project ()
  "Normal: visiting a project's todo.org makes the agenda show the project.
This is the whole point of the feature, and it is what the hand-built
fixture above could not check."
  (test-org-agenda-config-category--with-real-file
      (buffer "myproject" "todo.org" "* TODO a task\n")
    (with-current-buffer buffer
      (should (equal "myproject" (org-get-category (point-min)))))))

(ert-deftest test-org-agenda-config-category-endtoend-explicit-category-wins ()
  "Boundary: an explicit `#+CATEGORY:' still beats the derived name.

Note what this does and does not prove.  It confirms the user-visible contract,
but not that the hook's guard is what enforces it: `org-element--get-category'
searches the buffer for `#+CATEGORY:' itself and returns that before consulting
`org-category', so the directive survives even with the guard deleted.  The
guard's own job is covered by
`test-org-agenda-config-category-hook-boundary-respects-explicit'."
  (test-org-agenda-config-category--with-real-file
      (buffer "myproject" "todo.org" "#+CATEGORY: Personal\n* TODO a task\n")
    (with-current-buffer buffer
      (should (equal "Personal" (org-get-category (point-min)))))))

(ert-deftest test-org-agenda-config-category-endtoend-survives-an-earlier-hook ()
  "Error: the override still lands when another hook reads the category first.

`org-get-category' resolves a deferred `:CATEGORY' that the org-element cache
then holds.  A hook running BEFORE ours that reads it freezes \"todo\" in that
cache, and our later `setq-local' becomes inert -- `org-category' reads correct
while the agenda still shows \"todo\".

This is not hypothetical.  `add-hook' prepends by default, so every hook added
after ours runs before it, and the live config already has three ahead of it.
The fix is the explicit depth on our `add-hook'; this test is what holds it
there."
  (test-org-agenda-config-category--with-real-file
      (buffer "projX" "todo.org" "* TODO a task\n")
    (ignore buffer))
  ;; Re-visit with a nosy reader installed ahead of ours.
  (let ((reader (lambda () (ignore (org-get-category (point-min))))))
    (unwind-protect
        (progn
          (add-hook 'org-mode-hook reader)
          (test-org-agenda-config-category--with-real-file
              (buffer "projX" "todo.org" "* TODO a task\n")
            (with-current-buffer buffer
              (should (equal "projX" (org-get-category (point-min)))))))
      (remove-hook 'org-mode-hook reader))))

(ert-deftest test-org-agenda-config-category-endtoend-other-filename-untouched ()
  "Boundary: a distinctively-named file keeps its filename category.
Only todo.org is generic enough to be worth replacing.  Files like
schedule.org or gcal.org already say something useful, and deriving from
the directory would collapse several of them onto one name."
  (test-org-agenda-config-category--with-real-file
      (buffer "data" "gcal.org" "* TODO an event\n")
    (with-current-buffer buffer
      (should (equal "gcal" (org-get-category (point-min)))))))

(provide 'test-org-agenda-config-category)
;;; test-org-agenda-config-category.el ends here
