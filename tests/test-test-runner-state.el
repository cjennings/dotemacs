;;; test-test-runner-state.el --- Tests for test-runner per-project state + small wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; The sibling test-test-runner.el covers the `cj/test--do-*' helpers
;; and the focus-add / focus-remove / extract-test-names families.
;; This file fills in the state machinery and the small wrappers:
;;
;;   cj/test--project-root
;;   cj/test--state-key
;;   cj/test--project-state
;;   cj/test--state-get / cj/test--state-put
;;   cj/test--current-focused-files / cj/test--set-current-focused-files
;;   cj/test--current-mode        / cj/test--set-current-mode
;;   cj/test--sync-legacy-state
;;   cj/test--remember-loaded-project-root
;;   cj/test--file-in-directory-p
;;   cj/test--get-test-directory
;;   cj/test--get-test-files
;;   cj/test--ensure-test-dir-in-load-path
;;   cj/test-focus-clear / cj/test-toggle-mode / cj/test-view-focused
;;   cj/test-run-all

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'test-runner)

(defmacro test-tr--with-clean-state (&rest body)
  "Run BODY with `cj/test-project-states' cleared (and restored on exit)."
  `(let ((cj/test-project-states (make-hash-table :test #'equal))
         (cj/test-focused-files nil)
         (cj/test-mode 'all)
         (cj/test-loaded-project-roots nil))
     ,@body))

;;; cj/test--project-root

(ert-deftest test-tr-project-root-nil-when-projectile-absent ()
  "Boundary: when `projectile-project-root' isn't fbound, returns nil."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (not (eq sym 'projectile-project-root)))))
    (should-not (cj/test--project-root))))

(ert-deftest test-tr-project-root-returns-expanded-trailing-slash ()
  "Normal: when projectile gives a root, it comes back as a directory file name."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (eq sym 'projectile-project-root)))
            ((symbol-function 'projectile-project-root)
             (lambda () "/tmp/proj")))
    (let ((root (cj/test--project-root)))
      (should (string-prefix-p "/tmp/proj" root))
      (should (string-suffix-p "/" root)))))

;;; cj/test--state-key

(ert-deftest test-tr-state-key-falls-back-to-default-directory ()
  "Normal: with no project root, `default-directory' becomes the key."
  (let ((default-directory "/tmp/nothing/"))
    (cl-letf (((symbol-function 'cj/test--project-root) (lambda () nil)))
      (should (equal (cj/test--state-key)
                     (file-name-as-directory
                      (expand-file-name default-directory)))))))

;;; cj/test--project-state

(ert-deftest test-tr-project-state-creates-entry-lazily ()
  "Normal: first lookup creates a default state plist; second lookup reuses it."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/")))
     (let ((s1 (cj/test--project-state))
           (s2 (cj/test--project-state)))
       (should (equal (plist-get s1 :focused-files) '()))
       (should (eq (plist-get s1 :mode) 'all))
       (should (eq s1 s2))))))

;;; cj/test--state-get / cj/test--state-put

(ert-deftest test-tr-state-put-and-get-roundtrip ()
  "Normal: a put followed by a get returns the stored value."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/")))
     (cj/test--state-put :mode 'focused)
     (should (eq (cj/test--state-get :mode 'all) 'focused)))))

(ert-deftest test-tr-state-get-default-when-unset ()
  "Boundary: an unset property returns the supplied DEFAULT."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/")))
     (should (equal (cj/test--state-get :missing :fallback) :fallback)))))

;;; current-focused-files / current-mode setters/getters

(ert-deftest test-tr-current-focused-files-roundtrip ()
  "Normal: set then get returns the stored list."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/")))
     (cj/test--set-current-focused-files '("a.el" "b.el"))
     (should (equal (cj/test--current-focused-files) '("a.el" "b.el"))))))

(ert-deftest test-tr-current-mode-roundtrip ()
  "Normal: set then get returns the stored mode."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/")))
     (cj/test--set-current-mode 'focused)
     (should (eq (cj/test--current-mode) 'focused)))))

;;; cj/test--sync-legacy-state

(ert-deftest test-tr-sync-legacy-state-mirrors-into-public-vars ()
  "Normal: the legacy vars `cj/test-focused-files' and `cj/test-mode' track
the per-project state after each set."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/")))
     (cj/test--set-current-focused-files '("foo.el"))
     (cj/test--set-current-mode 'focused)
     (should (equal cj/test-focused-files '("foo.el")))
     (should (eq cj/test-mode 'focused)))))

;;; cj/test--remember-loaded-project-root

(ert-deftest test-tr-remember-loaded-project-root-pushes-once ()
  "Normal: a remembered root appears once even after repeated calls."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--project-root)
              (lambda () "/tmp/proj/")))
     (cj/test--remember-loaded-project-root)
     (cj/test--remember-loaded-project-root)
     (should (equal cj/test-loaded-project-roots '("/tmp/proj/"))))))

(ert-deftest test-tr-remember-loaded-project-root-no-op-without-root ()
  "Boundary: with no project root, nothing is added."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--project-root) (lambda () nil)))
     (cj/test--remember-loaded-project-root)
     (should-not cj/test-loaded-project-roots))))

;;; cj/test--file-in-directory-p

(ert-deftest test-tr-file-in-directory-p-true-for-subpath ()
  "Normal: a file inside a directory returns non-nil."
  (let ((dir (make-temp-file "test-tr-dir-" t)))
    (unwind-protect
        (let ((file (expand-file-name "x.el" dir)))
          (with-temp-file file (insert ""))
          (should (cj/test--file-in-directory-p file dir)))
      (delete-directory dir t))))

(ert-deftest test-tr-file-in-directory-p-false-for-sibling ()
  "Boundary: a file outside a directory returns nil."
  (let ((d1 (make-temp-file "test-tr-d1-" t))
        (d2 (make-temp-file "test-tr-d2-" t)))
    (unwind-protect
        (let ((file (expand-file-name "x.el" d2)))
          (with-temp-file file (insert ""))
          (should-not (cj/test--file-in-directory-p file d1)))
      (delete-directory d1 t)
      (delete-directory d2 t))))

;;; cj/test--get-test-directory

(ert-deftest test-tr-get-test-directory-prefers-tests-subdir ()
  "Normal: when project root has a `tests/' subdir, it wins."
  (let ((proj (make-temp-file "test-tr-proj-" t)))
    (make-directory (expand-file-name "tests" proj))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/test--project-root)
                   (lambda () (file-name-as-directory proj))))
          (should (equal (cj/test--get-test-directory)
                         (expand-file-name "tests" proj))))
      (delete-directory proj t))))

(ert-deftest test-tr-get-test-directory-falls-back-to-global ()
  "Boundary: without a project root, the global default is returned."
  (cl-letf (((symbol-function 'cj/test--project-root) (lambda () nil)))
    (should (equal (cj/test--get-test-directory)
                   cj/test-global-directory))))

;;; cj/test--get-test-files

(ert-deftest test-tr-get-test-files-lists-test-files-only ()
  "Normal: only files matching `test-*.el' are returned, basenames only."
  (let ((dir (make-temp-file "test-tr-files-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "test-alpha.el" dir) (insert ""))
          (with-temp-file (expand-file-name "test-beta.el" dir) (insert ""))
          (with-temp-file (expand-file-name "helper.el" dir) (insert ""))
          (cl-letf (((symbol-function 'cj/test--get-test-directory)
                     (lambda () dir)))
            (let ((files (cj/test--get-test-files)))
              (should (member "test-alpha.el" files))
              (should (member "test-beta.el" files))
              (should-not (member "helper.el" files)))))
      (delete-directory dir t))))

;;; cj/test--ensure-test-dir-in-load-path

(ert-deftest test-tr-ensure-test-dir-in-load-path-adds-dir ()
  "Normal: the test directory gets added to `load-path' once."
  (let ((dir (make-temp-file "test-tr-lp-" t)))
    (unwind-protect
        (let ((load-path (copy-sequence load-path)))
          (cl-letf (((symbol-function 'cj/test--get-test-directory)
                     (lambda () dir)))
            (cj/test--ensure-test-dir-in-load-path))
          (should (member dir load-path)))
      (delete-directory dir t))))

;;; cj/test-focus-clear

(ert-deftest test-tr-focus-clear-empties-focused-files ()
  "Normal: clear resets focused-files to empty list."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/"))
             ((symbol-function 'message) #'ignore))
     (cj/test--set-current-focused-files '("foo.el" "bar.el"))
     (cj/test-focus-clear)
     (should (null (cj/test--current-focused-files))))))

;;; cj/test-toggle-mode

(ert-deftest test-tr-toggle-mode-cycles-between-all-and-focused ()
  "Normal: starting from `all', two toggles produce focused then back to all."
  (test-tr--with-clean-state
   (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/"))
             ((symbol-function 'message) #'ignore))
     (cj/test--set-current-mode 'all)
     (cj/test-toggle-mode)
     (should (eq (cj/test--current-mode) 'focused))
     (cj/test-toggle-mode)
     (should (eq (cj/test--current-mode) 'all)))))

;;; cj/test-view-focused

(ert-deftest test-tr-view-focused-empty-list-messages-empty-state ()
  "Boundary: with no focused files, view messages the empty state."
  (test-tr--with-clean-state
   (let ((msg nil))
     (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/"))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq msg (apply #'format fmt args)))))
       (cj/test-view-focused))
     (should (string-match-p "No focused" msg)))))

(ert-deftest test-tr-view-focused-lists-files-when-present ()
  "Normal: a non-empty list comes out in the message."
  (test-tr--with-clean-state
   (let ((msg nil))
     (cl-letf (((symbol-function 'cj/test--state-key) (lambda () "/p/"))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq msg (apply #'format fmt args)))))
       (cj/test--set-current-focused-files '("alpha.el" "beta.el"))
       (cj/test-view-focused))
     (should (string-match-p "alpha.el" msg))
     (should (string-match-p "beta.el" msg)))))

;;; cj/test-run-all

(ert-deftest test-tr-run-all-delegates ()
  "Normal: `cj/test-run-all' calls `cj/ert-run-current-project-tests'."
  (let ((called nil))
    (cl-letf (((symbol-function 'cj/ert-run-current-project-tests)
               (lambda () (setq called t))))
      (cj/test-run-all))
    (should called)))

(provide 'test-test-runner-state)
;;; test-test-runner-state.el ends here
