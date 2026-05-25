;;; test-hugo-config-commands.el --- Tests for hugo-config command wrappers + preview helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the pure helpers (`cj/hugo--post-file-path`,
;; `cj/hugo--post-template`, `cj/hugo--post-metadata`,
;; `cj/hugo--collect-drafts`, `cj/hugo-open-blog-dir-external`,
;; `cj/hugo-toggle-draft`).  This file covers the remaining
;; commands + preview helpers:
;;
;;   cj/hugo-new-post
;;   cj/hugo-export-post
;;   cj/hugo-open-blog-dir
;;   cj/hugo-open-draft
;;   cj/hugo--preview-filter
;;   cj/hugo--preview-sentinel
;;   cj/hugo-preview
;;   cj/hugo-publish
;;
;; ox-hugo / magit / hugo / browse-url / dired are all stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'hugo-config)

;; ox-hugo isn't installed in the batch environment.  Provide a tiny
;; slug stub so the post-creation tests can build a filename.
(unless (fboundp 'org-hugo-slug)
  (defun org-hugo-slug (title)
    "Stub slugifier for batch tests."
    (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))))

;;; cj/hugo-new-post

(ert-deftest test-hugo-new-post-creates-file-with-template ()
  "Normal: a new post writes the template to a slugged file."
  (let* ((tmp-dir (make-temp-file "test-hugo-" t))
         (cj/hugo-content-org-dir tmp-dir))
    (unwind-protect
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) "My New Post"))
                  ((symbol-function 'message) #'ignore))
          (cj/hugo-new-post)
          (let ((expected (cj/hugo--post-file-path "My New Post")))
            (should (file-exists-p expected))
            (with-temp-buffer
              (insert-file-contents expected)
              (should (string-match-p "#\\+title: My New Post" (buffer-string)))
              (should (string-match-p "#\\+hugo_draft: true" (buffer-string))))
            ;; The find-file opened a buffer; kill it so the temp dir can go.
            (when-let ((buf (get-file-buffer expected)))
              (kill-buffer buf))))
      (delete-directory tmp-dir t))))

(ert-deftest test-hugo-new-post-errors-when-file-exists ()
  "Error: re-creating an existing post signals user-error."
  (let* ((tmp-dir (make-temp-file "test-hugo-" t))
         (cj/hugo-content-org-dir tmp-dir))
    (unwind-protect
        (let* ((path (cj/hugo--post-file-path "Duplicate Post")))
          (with-temp-file path (insert "already here"))
          (cl-letf (((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _) "Duplicate Post"))
                    ((symbol-function 'message) #'ignore))
            (should-error (cj/hugo-new-post) :type 'user-error)))
      (delete-directory tmp-dir t))))

;;; cj/hugo-export-post

(ert-deftest test-hugo-export-post-error-not-in-org-mode ()
  "Error: outside org-mode the export errors with user-error.

ox-hugo isn't installed in the batch environment, so `require' is
stubbed before the org-mode-derived guard runs."
  (with-temp-buffer
    (cl-letf (((symbol-function 'require)
               (lambda (feat &rest _) (if (eq feat 'ox-hugo) t (error "unstubbed require: %s" feat))))
              ((symbol-function 'org-hugo-export-to-md)
               (lambda () (error "shouldn't run"))))
      (should-error (cj/hugo-export-post) :type 'user-error))))

(ert-deftest test-hugo-export-post-normal-delegates-to-ox-hugo ()
  "Normal: in an org-mode buffer the export delegates to `org-hugo-export-to-md'."
  (let ((called nil)
        (msg nil))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (cl-letf (((symbol-function 'require)
                 (lambda (feat &rest _) (if (eq feat 'ox-hugo) t (error "unstubbed require: %s" feat))))
                ((symbol-function 'org-hugo-export-to-md)
                 (lambda () (setq called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (cj/hugo-export-post)))
    (should called)
    (should (string-match-p "Exported" msg))))

;;; cj/hugo-open-blog-dir

(ert-deftest test-hugo-open-blog-dir-creates-and-opens-dired ()
  "Normal: missing dir gets created, then dired opens it."
  (let* ((tmp (concat (make-temp-file "test-hugo-blog-" t) "/sub"))
         (cj/hugo-content-org-dir tmp)
         (dired-arg nil))
    (unwind-protect
        (cl-letf (((symbol-function 'dired)
                   (lambda (d) (setq dired-arg d))))
          (cj/hugo-open-blog-dir))
      (when (file-directory-p tmp) (delete-directory tmp t)))
    (should (equal dired-arg tmp))))

;;; cj/hugo-open-draft

(ert-deftest test-hugo-open-draft-empty-list-messages ()
  "Boundary: no drafts -> messages \"No drafts\"."
  (let ((msg nil))
    (cl-letf (((symbol-function 'cj/hugo--collect-drafts)
               (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/hugo-open-draft))
    (should (string-match-p "No drafts" msg))))

(ert-deftest test-hugo-open-draft-picks-via-completing-read ()
  "Normal: with drafts, completing-read picks one and find-file opens it."
  (let ((opened nil))
    (cl-letf (((symbol-function 'cj/hugo--collect-drafts)
               (lambda (_) '(("Foo Post" . "/tmp/foo.org")
                             ("Bar Post" . "/tmp/bar.org"))))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Foo Post"))
              ((symbol-function 'find-file)
               (lambda (f) (setq opened f))))
      (cj/hugo-open-draft))
    (should (equal opened "/tmp/foo.org"))))

;;; cj/hugo--preview-filter

(ert-deftest test-hugo-preview-filter-opens-browser-on-ready-line ()
  "Normal: a 'Web Server is available' line in OUTPUT triggers `browse-url'."
  (let ((url nil))
    (cl-letf (((symbol-function 'process-buffer)
               (lambda (_) nil))
              ((symbol-function 'process-live-p)
               (lambda (_) t))
              ((symbol-function 'browse-url)
               (lambda (u) (setq url u)))
              ((symbol-function 'set-process-filter) #'ignore))
      (cj/hugo--preview-filter 'fake-proc
                               "Web Server is available at http://localhost:1313/\n"))
    (should (equal url "http://localhost:1313/"))))

(ert-deftest test-hugo-preview-filter-does-not-open-on-other-output ()
  "Boundary: arbitrary output doesn't trigger the browser."
  (let ((url nil))
    (cl-letf (((symbol-function 'process-buffer) (lambda (_) nil))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'browse-url)
               (lambda (u) (setq url u)))
              ((symbol-function 'set-process-filter) #'ignore))
      (cj/hugo--preview-filter 'fake-proc "Hugo built site in 5ms\n"))
    (should-not url)))

;;; cj/hugo--preview-sentinel

(ert-deftest test-hugo-preview-sentinel-clears-process-var-on-clean-exit ()
  "Normal: a clean exit clears `cj/hugo--preview-process'."
  (let ((cj/hugo--preview-process 'fake-proc))
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 0)))
      (cj/hugo--preview-sentinel 'fake-proc "finished\n"))
    (should-not cj/hugo--preview-process)))

(ert-deftest test-hugo-preview-sentinel-messages-on-crash ()
  "Boundary: a non-zero exit logs the crash message."
  (let ((cj/hugo--preview-process 'fake-proc)
        (msg nil))
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 1))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/hugo--preview-sentinel 'fake-proc "exited abnormally\n"))
    (should (string-match-p "hugo server crashed" msg))))

;;; cj/hugo-preview

(ert-deftest test-hugo-preview-stops-running-server ()
  "Normal: with a live process, preview kills it and clears the var."
  (let ((cj/hugo--preview-process 'fake-proc)
        (killed nil)
        (msg nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'kill-process)
               (lambda (p) (setq killed p)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/hugo-preview))
    (should (eq killed 'fake-proc))
    (should-not cj/hugo--preview-process)
    (should (string-match-p "stopped" msg))))

(ert-deftest test-hugo-preview-starts-server-when-stopped ()
  "Normal: with no live process, preview starts the hugo server."
  (let ((cj/hugo--preview-process nil)
        (start-args nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'executable-find) (lambda (_) "/usr/bin/hugo"))
              ((symbol-function 'start-process)
               (lambda (&rest args)
                 (setq start-args args)
                 'fake-proc))
              ((symbol-function 'set-process-filter) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore))
      (cj/hugo-preview))
    (should (eq cj/hugo--preview-process 'fake-proc))
    (should (member "server" start-args))))

(ert-deftest test-hugo-preview-errors-when-hugo-missing ()
  "Error: a missing hugo binary signals user-error before start-process."
  (let ((cj/hugo--preview-process nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'executable-find) (lambda (_) nil))
              ((symbol-function 'start-process)
               (lambda (&rest _) (error "start-process should not run")))
              ((symbol-function 'message) #'ignore))
      (should-error (cj/hugo-preview) :type 'user-error))))

;;; cj/hugo-publish

(ert-deftest test-hugo-publish-opens-magit-on-website-dir ()
  "Normal: publish hands off to `magit-status-setup-buffer' on the website dir."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'magit-status-setup-buffer)
               (lambda (d) (setq called-with d))))
      (cj/hugo-publish))
    (should (equal called-with website-dir))))

(provide 'test-hugo-config-commands)
;;; test-hugo-config-commands.el ends here
