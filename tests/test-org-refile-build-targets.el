;;; test-org-refile-build-targets.el --- Tests for cj/build-org-refile-targets -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/build-org-refile-targets' and the underlying cache
;; shape.  The wrapper delegates to `cj-cache.el', so these tests
;; exercise the integration -- they don't reach into the cache plist
;; directly (the cache primitives have their own tests in
;; test-cj-cache.el).  Stubs `cj/--org-refile-scan-targets' to avoid
;; touching the filesystem.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar inbox-file "/tmp/test-inbox.org")
(defvar reference-file "/tmp/test-reference.org")
(defvar schedule-file "/tmp/test-schedule.org")
(defvar code-dir "/tmp/test-code/")
(defvar projects-dir "/tmp/test-projects/")

(require 'org-refile-config)

(defun test-org-refile--reset ()
  "Reset cache and `org-refile-targets' between tests."
  (cj/cache-invalidate cj/--org-refile-targets-cache)
  (setq org-refile-targets nil))

;;; Normal Cases

(ert-deftest test-org-refile-build-targets-first-call-builds-and-populates ()
  "Normal: first call calls the scan helper and assigns to `org-refile-targets'."
  (test-org-refile--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                   (lambda ()
                     (cl-incf scan-calls)
                     '(("/a.org" :maxlevel . 1) ("/b.org" :maxlevel . 1)))))
          (cj/build-org-refile-targets)
          (should (= 1 scan-calls))
          (should (equal '(("/a.org" :maxlevel . 1) ("/b.org" :maxlevel . 1))
                         org-refile-targets))))
    (test-org-refile--reset)))

(ert-deftest test-org-refile-build-targets-second-call-uses-cache ()
  "Normal: a valid cache means the second call doesn't call the scan helper."
  (test-org-refile--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                   (lambda ()
                     (cl-incf scan-calls)
                     '(("/a.org" :maxlevel . 1)))))
          (cj/build-org-refile-targets)
          (cj/build-org-refile-targets)
          (should (= 1 scan-calls))))
    (test-org-refile--reset)))

(ert-deftest test-org-refile-build-targets-force-rebuild-bypasses-cache ()
  "Normal: FORCE-REBUILD calls the scan helper even when the cache is valid."
  (test-org-refile--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                   (lambda ()
                     (cl-incf scan-calls)
                     '(("/a.org" :maxlevel . 1)))))
          (cj/build-org-refile-targets)
          (cj/build-org-refile-targets 'force)
          (should (= 2 scan-calls))))
    (test-org-refile--reset)))

;;; Boundary Cases

(ert-deftest test-org-refile-build-targets-cache-expires-after-ttl ()
  "Boundary: an expired cache rebuilds (cache time backdated past TTL)."
  (test-org-refile--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                   (lambda ()
                     (cl-incf scan-calls)
                     '(("/a.org" :maxlevel . 1)))))
          (cj/build-org-refile-targets)
          (let ((ttl (plist-get cj/--org-refile-targets-cache :ttl)))
            (plist-put cj/--org-refile-targets-cache :time
                       (- (float-time) (1+ ttl))))
          (cj/build-org-refile-targets)
          (should (= 2 scan-calls))))
    (test-org-refile--reset)))

(ert-deftest test-org-refile-build-targets-empty-scan-still-assigns ()
  "Boundary: scan returning nil assigns nil and does not cache (per the
documented \"nil reads as invalid\" contract)."
  (test-org-refile--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                   (lambda () (cl-incf scan-calls) nil)))
          (cj/build-org-refile-targets)
          (should (= 1 scan-calls))
          (should (null org-refile-targets))
          (cj/build-org-refile-targets)
          (should (= 2 scan-calls))))
    (test-org-refile--reset)))

(ert-deftest test-org-refile-build-targets-building-flag-clears-on-success ()
  "Boundary: the cache's :building flag is cleared after a successful build."
  (test-org-refile--reset)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                 (lambda () '(("/a.org" :maxlevel . 1)))))
        (cj/build-org-refile-targets)
        (should-not (cj/cache-building-p cj/--org-refile-targets-cache)))
    (test-org-refile--reset)))

;;; Error Cases

(ert-deftest test-org-refile-build-targets-scan-error-propagates ()
  "Error: a failure inside the scan helper propagates to the caller."
  (test-org-refile--reset)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                 (lambda () (error "Permission denied"))))
        (should-error (cj/build-org-refile-targets)))
    (test-org-refile--reset)))

(ert-deftest test-org-refile-build-targets-building-flag-clears-on-error ()
  "Error: the building flag is cleared even when the scan helper signals."
  (test-org-refile--reset)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/--org-refile-scan-targets)
                 (lambda () (error "Simulated failure"))))
        (ignore-errors (cj/build-org-refile-targets))
        (should-not (cj/cache-building-p cj/--org-refile-targets-cache)))
    (test-org-refile--reset)))

(provide 'test-org-refile-build-targets)
;;; test-org-refile-build-targets.el ends here
