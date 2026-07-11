;;; test-org-roam-config-tag-and-find.el --- Tests for org-roam tag predicate + find wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the slug helper, the link-description helper,
;; the demote-subtree pure function, the format-roam-node helper, and
;; the copy-todo-to-today hook.  This file fills in the tag-filter
;; predicate, the list-notes-by-tag wrapper, the find-node command +
;; its convenience entries, the insert-immediate wrapper, and the
;; capture-finalize hook.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-roam-config)

;; Top-level defvars so `let' bindings reach the dynamic variable under
;; lexical scope (the hooks reference these globals).
(defvar org-note-abort nil
  "Dynamic stand-in for `org-note-abort' (org-capture isn't loaded here).")
(defvar org-roam-capture-templates nil
  "Dynamic stand-in for `org-roam-capture-templates'.")
(defvar org-agenda-files nil
  "Dynamic stand-in for `org-agenda-files' (org-agenda not loaded here).")
(defvar org-capture-after-finalize-hook nil
  "Dynamic stand-in for `org-capture-after-finalize-hook'.")

;;; cj/org-roam-filter-by-tag

(ert-deftest test-org-roam-filter-by-tag-returns-predicate ()
  "Normal: filter returns a function."
  (should (functionp (cj/org-roam-filter-by-tag "Topic"))))

(ert-deftest test-org-roam-filter-by-tag-keeps-nodes-with-matching-tag ()
  "Normal: predicate returns non-nil for a node whose tags contain TAG-NAME."
  (cl-letf (((symbol-function 'org-roam-node-tags)
             (lambda (n) (plist-get n :tags))))
    (let ((pred (cj/org-roam-filter-by-tag "Topic")))
      (should (funcall pred (list :tags '("Topic" "Tech")))))))

(ert-deftest test-org-roam-filter-by-tag-rejects-nodes-without-tag ()
  "Boundary: predicate returns nil when the tag isn't in the node's tag list."
  (cl-letf (((symbol-function 'org-roam-node-tags)
             (lambda (n) (plist-get n :tags))))
    (let ((pred (cj/org-roam-filter-by-tag "Topic")))
      (should-not (funcall pred (list :tags '("Recipe")))))))

;;; cj/org-roam-list-notes-by-tag

(ert-deftest test-org-roam-list-notes-by-tag-returns-file-paths-for-matches ()
  "Normal: filters the node list by tag and returns each node's file path."
  (cl-letf (((symbol-function 'org-roam-node-list)
             (lambda () (list (list :tags '("Topic") :file "/p/a.org")
                              (list :tags '("Recipe") :file "/p/b.org")
                              (list :tags '("Topic" "Tech") :file "/p/c.org"))))
            ((symbol-function 'org-roam-node-tags)
             (lambda (n) (plist-get n :tags)))
            ((symbol-function 'org-roam-node-file)
             (lambda (n) (plist-get n :file))))
    (should (equal (cj/org-roam-list-notes-by-tag "Topic")
                   '("/p/a.org" "/p/c.org")))))

(ert-deftest test-org-roam-list-notes-by-tag-empty-when-no-matches ()
  "Boundary: a tag with no matches returns an empty list."
  (cl-letf (((symbol-function 'org-roam-node-list)
             (lambda () (list (list :tags '("Recipe") :file "/p/a.org"))))
            ((symbol-function 'org-roam-node-tags)
             (lambda (n) (plist-get n :tags)))
            ((symbol-function 'org-roam-node-file)
             (lambda (n) (plist-get n :file))))
    (should (null (cj/org-roam-list-notes-by-tag "Topic")))))

;;; cj/org-roam-add-node-to-agenda-files-finalize-hook

(ert-deftest test-org-roam-finalize-hook-removes-itself ()
  "Normal: the hook removes itself after running, regardless of abort."
  (let ((org-note-abort nil)
        (org-agenda-files nil)
        (org-capture-after-finalize-hook
         (list #'cj/org-roam-add-node-to-agenda-files-finalize-hook)))
    (cl-letf (((symbol-function 'org-capture-get)
               (lambda (_) (current-buffer))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/captured.org")
        (cj/org-roam-add-node-to-agenda-files-finalize-hook)
        (setq buffer-file-name nil)))
    (should-not (memq #'cj/org-roam-add-node-to-agenda-files-finalize-hook
                      org-capture-after-finalize-hook))))

(ert-deftest test-org-roam-finalize-hook-adds-file-when-not-aborted ()
  "Normal: when the capture wasn't aborted, the new file lands in
`org-agenda-files'."
  (let ((org-note-abort nil)
        (org-agenda-files nil)
        (org-capture-after-finalize-hook
         (list #'cj/org-roam-add-node-to-agenda-files-finalize-hook)))
    (cl-letf (((symbol-function 'org-capture-get)
               (lambda (_) (current-buffer))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/captured-a.org")
        (cj/org-roam-add-node-to-agenda-files-finalize-hook)
        (setq buffer-file-name nil)))
    (should (member "/tmp/captured-a.org" org-agenda-files))))

(ert-deftest test-org-roam-finalize-hook-skips-on-abort ()
  "Boundary: when the capture was aborted, the file is NOT added."
  (let ((org-note-abort t)
        (org-agenda-files nil)
        (org-capture-after-finalize-hook
         (list #'cj/org-roam-add-node-to-agenda-files-finalize-hook)))
    (cj/org-roam-add-node-to-agenda-files-finalize-hook)
    (should-not org-agenda-files)))

;;; cj/org-roam-find-node + the two convenience wrappers

(ert-deftest test-org-roam-find-node-passes-templates-and-tag-filter ()
  "Normal: find-node calls org-roam-node-find with a filter + templates."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'org-roam-node-find)
               (lambda (&rest args) (setq called-with args))))
      (cj/org-roam-find-node "Topic" "t" "/tmp/template.org"))
    (should called-with)
    ;; Third positional arg is the filter predicate.
    (should (functionp (nth 2 called-with)))
    ;; Templates keyword arg present.
    (should (memq :templates called-with))))

(ert-deftest test-org-roam-find-node-topic-delegates-to-find-node ()
  "Normal: `find-node-topic' calls `find-node' with Topic tag + template-key 't'."
  (let ((args nil))
    (cl-letf (((symbol-function 'cj/org-roam-find-node)
               (lambda (&rest a) (setq args a))))
      (cj/org-roam-find-node-topic))
    (should (equal (car args) "Topic"))
    (should (equal (cadr args) "t"))))

(ert-deftest test-org-roam-find-node-recipe-delegates-to-find-node ()
  "Normal: `find-node-recipe' uses Recipe tag, 'r' key, recipes/ subdir."
  (let ((args nil))
    (cl-letf (((symbol-function 'cj/org-roam-find-node)
               (lambda (&rest a) (setq args a))))
      (cj/org-roam-find-node-recipe))
    (should (equal (car args) "Recipe"))
    (should (equal (cadr args) "r"))
    ;; The 4th arg is the subdir
    (should (equal (nth 3 args) "recipes/"))))

(ert-deftest test-org-roam-find-node-project-delegates-to-find-node ()
  "Normal: `find-node-project' uses Project tag, 'p' key, project.org template."
  (let ((args nil))
    (cl-letf (((symbol-function 'cj/org-roam-find-node)
               (lambda (&rest a) (setq args a))))
      (cj/org-roam-find-node-project))
    (should (equal (car args) "Project"))
    (should (equal (cadr args) "p"))
    ;; The 3rd arg is the template file, under the canonical roam-dir/templates/.
    (should (string-suffix-p "templates/project.org" (nth 2 args)))))

;;; cj/org-roam-node-insert-immediate

(ert-deftest test-org-roam-node-insert-immediate-rebinds-templates-and-calls-insert ()
  "Normal: the wrapper appends :immediate-finish t to the first capture
template and calls org-roam-node-insert."
  (let ((seen-templates nil)
        (called nil))
    (cl-letf (((symbol-function 'org-roam-node-insert)
               (lambda (&rest _)
                 (setq called t
                       seen-templates org-roam-capture-templates))))
      (let ((org-roam-capture-templates '(("d" "default" plain "%?"))))
        (cj/org-roam-node-insert-immediate nil)))
    (should called)
    (should (member :immediate-finish (car seen-templates)))))

(provide 'test-org-roam-config-tag-and-find)
;;; test-org-roam-config-tag-and-find.el ends here
