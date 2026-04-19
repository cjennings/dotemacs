;;; test-reconcile--check-for-open-work.el --- Tests for cj/check-for-open-work -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the top-level entry point that iterates `projects-dir',
;; `code-dir', `org-dir', and `user-emacs-directory'.
;;
;; Regression guard: a prior version used `(boundp 'base-dir)' under
;; lexical-binding, which always returned nil, causing every repo under
;; `projects-dir' and `code-dir' to be silently skipped.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

;; Declare as special so `let' creates dynamic bindings the SUT can observe.
(defvar projects-dir)
(defvar code-dir)
(defvar org-dir)

(defvar test-reconcile-calls nil
  "Directories passed to `cj/reconcile-git-directory' during a test.")

(defmacro test-reconcile-with-mocked-reconcile (&rest body)
  "Run BODY with `cj/reconcile-git-directory' recording its argument.
Uses `setq' so recorded calls remain readable after BODY returns — a
`let' binding would be gone by the time the outer `should' runs."
  (declare (indent 0))
  `(progn
     (setq test-reconcile-calls nil)
     (cl-letf (((symbol-function 'cj/reconcile-git-directory)
                (lambda (dir) (push dir test-reconcile-calls)))
               ((symbol-function 'message) (lambda (_fmt &rest _args))))
       ,@body)))

(defun test-reconcile-called-with-p (path)
  "Return non-nil if `cj/reconcile-git-directory' was called with PATH."
  (cl-some (lambda (d)
             (string= (file-name-as-directory d)
                      (file-name-as-directory path)))
           test-reconcile-calls))

;;; Normal Cases

(ert-deftest test-reconcile-check-for-open-work-normal-reconciles-projects-dir-repo ()
  "A repo under `projects-dir' is passed to `cj/reconcile-git-directory'.
Regression: lexical-binding + `(boundp 'base-dir)' used to silently skip this."
  (reconcile-test-with-temp-dirs
   ("projects/repo-a/.git/")
   (let ((projects-dir (expand-file-name "projects" test-root))
         (code-dir nil)
         (org-dir nil))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "projects/repo-a" test-root))))))

(ert-deftest test-reconcile-check-for-open-work-normal-reconciles-code-dir-repo ()
  "A repo under `code-dir' is passed to `cj/reconcile-git-directory'.
Regression: lexical-binding + `(boundp 'base-dir)' used to silently skip this."
  (reconcile-test-with-temp-dirs
   ("code/archsetup/.git/")
   (let ((projects-dir nil)
         (code-dir (expand-file-name "code" test-root))
         (org-dir nil))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "code/archsetup" test-root))))))

(ert-deftest test-reconcile-check-for-open-work-normal-reconciles-both-dirs ()
  "Repos under both `projects-dir' and `code-dir' are reconciled in one run."
  (reconcile-test-with-temp-dirs
   ("projects/proj-a/.git/" "code/code-a/.git/")
   (let ((projects-dir (expand-file-name "projects" test-root))
         (code-dir (expand-file-name "code" test-root))
         (org-dir nil))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "projects/proj-a" test-root)))
     (should (test-reconcile-called-with-p
              (expand-file-name "code/code-a" test-root))))))

(ert-deftest test-reconcile-check-for-open-work-normal-reconciles-every-repo ()
  "Every repo under `projects-dir' is reconciled, not just the first."
  (reconcile-test-with-temp-dirs
   ("projects/a/.git/" "projects/b/.git/" "projects/c/.git/")
   (let ((projects-dir (expand-file-name "projects" test-root))
         (code-dir nil)
         (org-dir nil))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (dolist (repo '("projects/a" "projects/b" "projects/c"))
       (should (test-reconcile-called-with-p
                (expand-file-name repo test-root)))))))

(ert-deftest test-reconcile-check-for-open-work-normal-reconciles-org-dir ()
  "`org-dir' is reconciled individually (the dir itself, not its children)."
  (reconcile-test-with-temp-dirs
   ("orgdir/.git/")
   (let ((projects-dir nil)
         (code-dir nil)
         (org-dir (expand-file-name "orgdir" test-root)))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "orgdir" test-root))))))

(ert-deftest test-reconcile-check-for-open-work-normal-reconciles-user-emacs-directory ()
  "`user-emacs-directory' is always reconciled individually."
  (reconcile-test-with-temp-dirs
   ("emacsdir/.git/")
   (let ((projects-dir nil)
         (code-dir nil)
         (org-dir nil)
         (user-emacs-directory (expand-file-name "emacsdir" test-root)))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "emacsdir" test-root))))))

;;; Boundary Cases

(ert-deftest test-reconcile-check-for-open-work-boundary-nil-projects-dir ()
  "Nil `projects-dir' doesn't crash; other dirs still process."
  (reconcile-test-with-temp-dirs
   ("code/repo/.git/")
   (let ((projects-dir nil)
         (code-dir (expand-file-name "code" test-root))
         (org-dir nil))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "code/repo" test-root))))))

(ert-deftest test-reconcile-check-for-open-work-boundary-nonexistent-projects-dir ()
  "Non-existent `projects-dir' is skipped without error; `code-dir' processes."
  (reconcile-test-with-temp-dirs
   ("code/repo/.git/")
   (let ((projects-dir (expand-file-name "does-not-exist" test-root))
         (code-dir (expand-file-name "code" test-root))
         (org-dir nil))
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (test-reconcile-called-with-p
              (expand-file-name "code/repo" test-root))))))

(ert-deftest test-reconcile-check-for-open-work-boundary-empty-dirs-produce-no-calls ()
  "Empty `projects-dir' and `code-dir' produce no repo-level reconcile calls."
  (reconcile-test-with-temp-dirs
   ("projects/" "code/")
   (let ((projects-dir (expand-file-name "projects" test-root))
         (code-dir (expand-file-name "code" test-root))
         (org-dir nil)
         (user-emacs-directory (expand-file-name "emacs-d" test-root))) ;; non-existent
     (test-reconcile-with-mocked-reconcile
       (cj/check-for-open-work))
     (should (null test-reconcile-calls)))))

;;; Error / Edge Cases

(ert-deftest test-reconcile-check-for-open-work-error-emits-complete-message ()
  "Emits the terminal `Complete.' message after iteration."
  (reconcile-test-with-temp-dirs
   ("projects/" "code/")
   (let ((projects-dir (expand-file-name "projects" test-root))
         (code-dir (expand-file-name "code" test-root))
         (org-dir nil)
         (messages nil))
     (cl-letf (((symbol-function 'cj/reconcile-git-directory) (lambda (_dir)))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       (cj/check-for-open-work))
     (should (cl-some (lambda (m) (string-match-p "Complete\\." m)) messages)))))

(provide 'test-reconcile--check-for-open-work)
;;; test-reconcile--check-for-open-work.el ends here
