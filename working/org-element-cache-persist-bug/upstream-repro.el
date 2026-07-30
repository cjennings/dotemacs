;;; upstream-repro.el --- minimal reproduction for the org list -*- lexical-binding: t -*-

;; Run with:  emacs -Q --batch -l upstream-repro.el
;;
;; Writing the element cache to disk while the buffer is still live strips the
;; :buffer property from every cached element and never restores it.  The next
;; access to a cached headline signals (wrong-type-argument stringp nil).

(require 'org)
(require 'org-element)
(require 'org-persist)

(let* ((dir (make-temp-file "org-persist-repro-" t))
       (org-persist-directory (expand-file-name "persist" dir))
       (file (expand-file-name "notes.org" dir))
       (org-element-use-cache t)
       (org-element-cache-persistent t)
       (org-log-done nil))

  (with-temp-file file
    (dotimes (i 20)
      (insert (format "* TODO task %d\nbody %d\n" i i))))

  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      ;; Populate the cache with parsed headlines.
      (org-element-cache-reset)
      (org-element-cache-map #'ignore :granularity 'headline)
      (goto-char (point-min))
      (re-search-forward "^\\* TODO task 0")
      (beginning-of-line)
      (org-todo "DONE"))

    (message "cached :buffer values before write: %S"
             (with-current-buffer buffer
               (let (values)
                 (avl-tree-mapc (lambda (element)
                                  (push (org-element-property :buffer element) values))
                                org-element--cache)
                 (delete-dups values))))

    ;; Persist while the buffer is still alive.  In a real session this is
    ;; reached by any in-session write, e.g. `org-persist-write-all-buffer' from
    ;; `kill-buffer-hook' in a *second* buffer visiting the same file.
    (org-persist-write `((elisp org-element--cache)
                         (version ,org-element-cache-version))
                       buffer t)

    (message "cached :buffer values after write:  %S"
             (with-current-buffer buffer
               (let (values)
                 (avl-tree-mapc (lambda (element)
                                  (push (org-element-property :buffer element) values))
                                org-element--cache)
                 (delete-dups values))))

    (with-current-buffer buffer
      (message "reading a cached headline: %s"
               (condition-case err
                   (progn (org-element-cache-map
                           (lambda (element) (org-element-property :raw-value element))
                           :granularity 'headline)
                          "ok")
                 (error (format "%S" err)))))

    (set-buffer-modified-p nil)
    (kill-buffer buffer))
  (delete-directory dir t))

;;; upstream-repro.el ends here
