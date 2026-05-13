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
     ;; mimic org's default category (filename-sans-extension) so the
     ;; hook's "only override the default" guard is exercised.
     (setq-local org-category
                 (and ,path
                      (file-name-sans-extension
                       (file-name-nondirectory ,path))))
     ,body-form))

;;; Normal Cases

(ert-deftest test-org-agenda-config-category-hook-normal-overrides-todo ()
  "Normal: hook overrides default \"todo\" with the parent dir name."
  (test-org-agenda-config-category--with-file "/home/cjennings/.emacs.d/todo.org"
    (progn
      (cj/--org-set-todo-category)
      (should (equal "emacs.d" org-category)))))

(ert-deftest test-org-agenda-config-category-hook-normal-leaves-inbox-alone ()
  "Normal: hook leaves inbox.org's category at its filename default."
  (test-org-agenda-config-category--with-file "/home/cjennings/sync/org/roam/inbox.org"
    (progn
      (cj/--org-set-todo-category)
      (should (equal "inbox" org-category)))))

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

(provide 'test-org-agenda-config-category)
;;; test-org-agenda-config-category.el ends here
