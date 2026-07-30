;;; test-dirvish-config--preview-org-cache-env.el --- preview org-cache opt-out -*- lexical-binding: t; -*-

;;; Commentary:
;; Dirvish's fallback preview builds a throwaway buffer, points `buffer-file-name'
;; at the real file (dirvish.el:667) and runs `set-auto-mode' (dirvish.el:671).
;; Previewing an .org file therefore makes a real org-mode buffer that registers
;; with org-persist and picks up a buffer-local `org-persist-write-all-buffer' on
;; `kill-buffer-hook'.  When dirvish kills that buffer, the write fires,
;; `org-element--cache-persist-before-write' resolves `get-file-buffer' to the
;; user's REAL buffer, and nils `:buffer' across its live element cache.  The next
;; access to a cached headline then runs
;; `org-element--headline-parse-title', whose first form is
;; `(with-current-buffer (org-element-property :buffer headline))' -- and
;; `set-buffer' on nil signals `wrong-type-argument stringp nil'.
;;
;; Dirvish let-binds `dirvish-preview-environment' around `set-auto-mode', so
;; adding `(org-element-use-cache . nil)' there stops the preview buffer ever
;; registering.  Nothing is written mid-session and the real buffer is untouched.
;;
;; Coverage note, stated plainly: the wiring tests below assert the alist entry,
;; and the behavioral test drives the corruption sequence directly rather than
;; through dirvish.  Neither runs dirvish's own preview machinery, so a change in
;; how dirvish applies the environment would not be caught here.  That gap is
;; covered by a live check in the running daemon.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)
(require 'org)
(require 'org-element)
(require 'org-persist)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

;;; ----------------------------- the wiring -----------------------------------

(ert-deftest test-dirvish-preview-env-disables-org-cache ()
  "Normal: `dirvish-preview-environment' turns the element cache off.
This is the entry dirvish let-binds around `set-auto-mode', so it is what keeps
an org preview buffer from registering with org-persist."
  (should (member '(org-element-use-cache . nil) dirvish-preview-environment)))

(ert-deftest test-dirvish-preview-env-keeps-upstream-entries ()
  "Boundary: the upstream entries survive alongside ours.
Replacing the alist rather than extending it would silently drop dirvish's own
bindings, and preview would start honouring dir-locals and printing messages."
  (dolist (pair '((inhibit-message . t)
                  (non-essential . t)
                  (enable-dir-local-variables . nil)
                  (enable-local-variables . :safe)))
    (should (member pair dirvish-preview-environment))))

(ert-deftest test-dirvish-preview-env-entry-is-not-duplicated ()
  "Boundary: the entry appears exactly once.
The module can be re-loaded into a running daemon, so the wiring has to be
idempotent rather than appending on every load."
  (should (= 1 (cl-count 'org-element-use-cache dirvish-preview-environment
                         :key #'car))))

;;; --------------------------- the behavior -----------------------------------

(defun cj--test-preview-buffer (file env)
  "Build a dirvish-style preview buffer for FILE with ENV bound, and return it.
Mirrors `dirvish--preview-file-maybe-truncate': real `buffer-file-name', then
`set-auto-mode' under the preview environment."
  (with-current-buffer (get-buffer-create "*cj-test-preview*")
    (with-silent-modifications
      (insert-file-contents file nil 0 1048576)
      (setq buffer-file-name file)
      (goto-char (point-min))
      (rename-buffer (format "PREVIEW :: 1 :: %s" (file-name-nondirectory file))))
    ;; Dynamic `eval', matching dirvish.el:672 -- NOT lexical.  Under lexical
    ;; binding a non-special symbol would bind lexically and never reach
    ;; `set-auto-mode', so the replay would silently no-op and the test would
    ;; pass for the wrong reason.  Same trap as the json-object-type gotcha.
    (eval `(let ,(mapcar (lambda (e) `(,(car e) ',(cdr e))) env)
             (setq-local delay-mode-hooks t)
             (set-auto-mode)
             (font-lock-mode 1)))
    (current-buffer)))

(defun cj--test-persisted-p (file)
  "Return non-nil when FILE still has a registered org-element cache collection."
  (and (org-persist--find-index
        (org-persist--normalize-associated (list :file file)))
       t))

(defun cj--test-cached-buffers (buf)
  "Return the distinct `:buffer' values cached in BUF's element cache."
  (with-current-buffer buf
    (let (seen)
      (avl-tree-mapc (lambda (el) (push (org-element-property :buffer el) seen))
                     org-element--cache)
      (delete-dups seen))))

(defmacro cj--with-org-fixture (var &rest body)
  "Bind VAR to a live buffer visiting a populated org fixture, then run BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "cj-preview-" t))
          (org-persist-directory (expand-file-name "persist" dir))
          (file (expand-file-name "notes.org" dir))
          (org-element-use-cache t)
          (org-element-cache-persistent t)
          (org-log-done nil))
     (with-temp-file file
       (dotimes (i 30) (insert (format "* TODO task %d :t%d:\nbody %d\n" i i i))))
     (let ((,var (find-file-noselect file)))
       (unwind-protect
           (progn
             (with-current-buffer ,var
               (org-element-cache-reset)
               (org-element-cache-map #'ignore :granularity 'headline)
               ;; Closing a task is what actually lands parsed headline elements
               ;; in the cache carrying a `:buffer'.  Without it the cache stays
               ;; empty and every assertion below passes vacuously against nil.
               (goto-char (point-min))
               (re-search-forward "^\\* TODO task 0 ")
               (beginning-of-line)
               (org-todo "DONE"))
             (should (cj--test-cached-buffers ,var))
             ,@body)
         (when (buffer-live-p ,var)
           (with-current-buffer ,var (set-buffer-modified-p nil))
           (kill-buffer ,var))
         (delete-directory dir t)))))

(ert-deftest test-dirvish-preview-env-protects-the-real-buffer ()
  "Normal: previewing an already-open org file leaves its cache intact.
The whole point of the entry -- with it, killing the preview must not strip
`:buffer' from the real buffer's cached elements."
  (cj--with-org-fixture real
    (should (member real (cj--test-cached-buffers real)))
    (let ((preview (cj--test-preview-buffer (buffer-file-name real)
                                            dirvish-preview-environment)))
      (let (kill-buffer-query-functions) (kill-buffer preview)))
    (should (member real (cj--test-cached-buffers real)))
    (should-not (member nil (cj--test-cached-buffers real)))))

(ert-deftest test-dirvish-preview-env-real-buffer-still-parses ()
  "Normal: the real buffer still parses after the preview is killed.
Asserting on `:buffer' alone would pass against a cache that is broken in some
other way, so this drives the access path that actually signalled."
  (cj--with-org-fixture real
    (let ((preview (cj--test-preview-buffer (buffer-file-name real)
                                            dirvish-preview-environment)))
      (let (kill-buffer-query-functions) (kill-buffer preview)))
    (with-current-buffer real
      (should (equal "OK"
                     (condition-case err
                         (progn (org-element-cache-map
                                 (lambda (el) (org-element-property :raw-value el))
                                 :granularity 'headline)
                                "OK")
                       (error (format "%S" err))))))))

(ert-deftest test-dirvish-preview-env-without-the-entry-corrupts ()
  "Error: the same sequence WITHOUT the entry does corrupt the real buffer.
This is the control.  Without it the two tests above could pass because the
corruption never happens in this harness at all, rather than because the entry
prevents it."
  (cj--with-org-fixture real
    (let* ((bare (assq-delete-all 'org-element-use-cache
                                  (copy-alist dirvish-preview-environment)))
           (preview (cj--test-preview-buffer (buffer-file-name real) bare)))
      (let (kill-buffer-query-functions) (kill-buffer preview)))
    (should (member nil (cj--test-cached-buffers real)))))

(ert-deftest test-dirvish-preview-env-spares-an-unopened-file ()
  "Boundary: previewing an org file that is NOT open harms nothing.
`org-element--cache-persist-before-write' only acts when `get-file-buffer'
finds a live buffer, so this path was never at risk and must stay cheap."
  (cj--with-org-fixture real
    (let* ((other (expand-file-name "other.org" (file-name-directory
                                                 (buffer-file-name real)))))
      (with-temp-file other (insert "* TODO unrelated\n"))
      (let ((preview (cj--test-preview-buffer other dirvish-preview-environment)))
        (let (kill-buffer-query-functions) (kill-buffer preview))))
    (should-not (member nil (cj--test-cached-buffers real)))))

(provide 'test-dirvish-config--preview-org-cache-env)
;;; test-dirvish-config--preview-org-cache-env.el ends here
