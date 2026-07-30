;; Does the preview buffer re-register after the let unwinds, on fontification?
(require 'org) (require 'org-element) (require 'org-persist)
(defvar rk-dir (make-temp-file "refute-" t))
(setq org-persist-directory (expand-file-name "persist" rk-dir))
(setq org-element-use-cache t org-element-cache-persistent t org-log-done nil)
(defvar rk-file (expand-file-name "notes.org" rk-dir))
(with-temp-file rk-file
  (dotimes (i 20) (insert (format "* TODO task %d\nbody with src_python{1+1} inline\n" i))))

(defun rk-bufs (buf)
  (with-current-buffer buf
    (let (s) (avl-tree-mapc (lambda (el) (push (org-element-property :buffer el) s))
                            org-element--cache)
         (delete-dups s))))

(let ((real (find-file-noselect rk-file)))
  (with-current-buffer real
    (org-element-cache-reset)
    (org-element-cache-map #'ignore :granularity 'headline)
    (goto-char (point-min)) (re-search-forward "^\\* TODO task 0") (beginning-of-line)
    (org-todo "DONE"))
  (message "before: %S" (rk-bufs real))
  ;; preview WITH the fix's environment, then let it LIVE and fontify (the step my tests skip)
  (let ((prev (get-buffer-create "*prev*")))
    (with-current-buffer prev
      (with-silent-modifications
        (insert-file-contents rk-file nil 0 1048576)
        (setq buffer-file-name rk-file))
      (eval `(let ((inhibit-message t) (non-essential t)
                   (enable-dir-local-variables nil) (enable-local-variables :safe)
                   (org-element-use-cache nil))
               (setq-local delay-mode-hooks t) (set-auto-mode) (font-lock-mode 1)))
      (message "after set-auto-mode: local-cache=%S global=%S persist-hook=%S"
               org-element-use-cache (default-value 'org-element-use-cache)
               (and (memq 'org-persist-write-all-buffer kill-buffer-hook) t))
      ;; THE MISSING STEP: the preview lives in a window and gets fontified
      (font-lock-ensure)
      (message "after fontify:      persist-hook=%S cache-active=%S"
               (and (memq 'org-persist-write-all-buffer kill-buffer-hook) t)
               (org-element--cache-active-p)))
    (let (kill-buffer-query-functions) (kill-buffer prev)))
  (message "after kill: %S" (rk-bufs real))
  (with-current-buffer real
    (message "parse => %s"
             (condition-case e (progn (org-element-cache-map
                                       (lambda (el) (org-element-property :raw-value el))
                                       :granularity 'headline) "OK")
               (error (format "ERROR %S" e))))))
(delete-directory rk-dir t)
