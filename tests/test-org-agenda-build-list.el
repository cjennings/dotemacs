;;; test-org-agenda-build-list.el --- Tests for cj/build-org-agenda-list -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/build-org-agenda-list' and the underlying cache shape.
;; The wrapper delegates to `cj-cache.el', so these tests exercise the
;; integration -- they don't reach into the cache plist directly (the
;; cache primitives have their own tests in test-cj-cache.el).  Stubs
;; `cj/--org-agenda-scan-files' to avoid touching the filesystem.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar inbox-file "/tmp/test-inbox.org")
(defvar schedule-file "/tmp/test-schedule.org")
(defvar gcal-file "/tmp/test-gcal.org")
(defvar pcal-file "/tmp/test-pcal.org")
(defvar dcal-file "/tmp/test-dcal.org")
(defvar projects-dir "/tmp/test-projects/")

(require 'org-agenda-config)

(defun test-org-agenda--reset ()
  "Reset cache and `org-agenda-files' between tests."
  (cj/cache-invalidate cj/--org-agenda-files-cache)
  (setq org-agenda-files nil))

;;; Normal Cases

(ert-deftest test-org-agenda-build-list-first-call-builds-and-populates ()
  "Normal: first call calls the scan helper and assigns to `org-agenda-files'."
  (test-org-agenda--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                   (lambda ()
                     (cl-incf scan-calls)
                     '("/a.org" "/b.org" "/c.org"))))
          (cj/build-org-agenda-list)
          (should (= 1 scan-calls))
          (should (equal '("/a.org" "/b.org" "/c.org") org-agenda-files))))
    (test-org-agenda--reset)))

(ert-deftest test-org-agenda-build-list-second-call-uses-cache ()
  "Normal: a valid cache means the second call doesn't call the scan helper."
  (test-org-agenda--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                   (lambda ()
                     (cl-incf scan-calls)
                     '("/a.org"))))
          (cj/build-org-agenda-list)
          (cj/build-org-agenda-list)
          (should (= 1 scan-calls))))
    (test-org-agenda--reset)))

(ert-deftest test-org-agenda-build-list-force-rebuild-bypasses-cache ()
  "Normal: FORCE-REBUILD calls the scan helper even when the cache is valid."
  (test-org-agenda--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                   (lambda ()
                     (cl-incf scan-calls)
                     '("/a.org"))))
          (cj/build-org-agenda-list)
          (cj/build-org-agenda-list 'force)
          (should (= 2 scan-calls))))
    (test-org-agenda--reset)))

;;; Boundary Cases

(ert-deftest test-org-agenda-build-list-cache-expires-after-ttl ()
  "Boundary: an expired cache rebuilds.  Simulated by backdating the
cache's :time field beyond the TTL."
  (test-org-agenda--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                   (lambda ()
                     (cl-incf scan-calls)
                     '("/a.org"))))
          (cj/build-org-agenda-list)
          ;; Expire the cache by moving its timestamp backwards beyond TTL.
          (let ((ttl (plist-get cj/--org-agenda-files-cache :ttl)))
            (plist-put cj/--org-agenda-files-cache :time
                       (- (float-time) (1+ ttl))))
          (cj/build-org-agenda-list)
          (should (= 2 scan-calls))))
    (test-org-agenda--reset)))

(ert-deftest test-org-agenda-build-list-empty-scan-still-assigns ()
  "Boundary: a scan helper that returns nil does NOT cache (per the
documented \"nil reads as invalid\" contract).  org-agenda-files is
still assigned correctly."
  (test-org-agenda--reset)
  (unwind-protect
      (let ((scan-calls 0))
        (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                   (lambda () (cl-incf scan-calls) nil)))
          (cj/build-org-agenda-list)
          (should (= 1 scan-calls))
          (should (null org-agenda-files))
          ;; Next call rebuilds because nil-value reads as invalid.
          (cj/build-org-agenda-list)
          (should (= 2 scan-calls))))
    (test-org-agenda--reset)))

(ert-deftest test-org-agenda-build-list-building-flag-clears-on-success ()
  "Boundary: the cache's :building flag is cleared after a successful build."
  (test-org-agenda--reset)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                 (lambda () '("/a.org"))))
        (cj/build-org-agenda-list)
        (should-not (cj/cache-building-p cj/--org-agenda-files-cache)))
    (test-org-agenda--reset)))

;;; Error Cases

(ert-deftest test-org-agenda-build-list-scan-error-propagates ()
  "Error: a failure inside the scan helper propagates to the caller."
  (test-org-agenda--reset)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                 (lambda () (error "Permission denied"))))
        (should-error (cj/build-org-agenda-list)))
    (test-org-agenda--reset)))

(ert-deftest test-org-agenda-build-list-building-flag-clears-on-error ()
  "Error: the building flag is cleared even when the scan helper signals."
  (test-org-agenda--reset)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/--org-agenda-scan-files)
                 (lambda () (error "Simulated failure"))))
        (ignore-errors (cj/build-org-agenda-list))
        (should-not (cj/cache-building-p cj/--org-agenda-files-cache)))
    (test-org-agenda--reset)))

(provide 'test-org-agenda-build-list)
;;; test-org-agenda-build-list.el ends here
