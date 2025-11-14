;;; test-org-agenda-build-list.el --- Tests for cj/build-org-agenda-list -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/build-org-agenda-list caching logic.
;; Tests cache behavior, TTL expiration, force rebuild, and async build flag.

;;; Code:

(require 'ert)

;; Add modules to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar inbox-file "/tmp/test-inbox.org")
(defvar schedule-file "/tmp/test-schedule.org")
(defvar gcal-file "/tmp/test-gcal.org")
(defvar projects-dir "/tmp/test-projects/")

;; Now load the actual production module
(require 'org-agenda-config)

;;; Setup and Teardown

(defun test-org-agenda-setup ()
  "Reset cache and state before each test."
  (setq cj/org-agenda-files-cache nil)
  (setq cj/org-agenda-files-cache-time nil)
  (setq cj/org-agenda-files-building nil)
  (setq org-agenda-files nil))

(defun test-org-agenda-teardown ()
  "Clean up after each test."
  (setq cj/org-agenda-files-cache nil)
  (setq cj/org-agenda-files-cache-time nil)
  (setq cj/org-agenda-files-building nil)
  (setq org-agenda-files nil))

;;; Normal Cases

(ert-deftest test-org-agenda-build-list-normal-first-call-builds-cache ()
  "Test that first call builds cache from scratch.

When cache is empty, function should:
1. Scan directory for todo.org files
2. Build agenda files list
3. Populate cache
4. Set cache timestamp"
  (test-org-agenda-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern &optional _include-dirs) '("/tmp/project/todo.org"))))

        ;; Before call: cache empty
        (should (null cj/org-agenda-files-cache))
        (should (null cj/org-agenda-files-cache-time))

        ;; Build agenda files
        (cj/build-org-agenda-list)

        ;; After call: cache populated
        (should cj/org-agenda-files-cache)
        (should cj/org-agenda-files-cache-time)
        (should org-agenda-files)

        ;; Cache matches org-agenda-files
        (should (equal cj/org-agenda-files-cache org-agenda-files))

        ;; Contains base files (inbox, schedule, gcal) plus project files
        (should (>= (length org-agenda-files) 3)))
    (test-org-agenda-teardown)))

(ert-deftest test-org-agenda-build-list-normal-second-call-uses-cache ()
  "Test that second call uses cache instead of rebuilding.

When cache is valid (not expired):
1. Should NOT scan directories again
2. Should restore files from cache
3. Should NOT update cache timestamp"
  (test-org-agenda-setup)
  (unwind-protect
      (let ((scan-count 0))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern &optional _include-dirs)
                     (setq scan-count (1+ scan-count))
                     '("/tmp/project/todo.org"))))

          ;; First call: builds cache
          (cj/build-org-agenda-list)
          (should (= scan-count 1))  ; 1 directory scanned

          (let ((cached-time cj/org-agenda-files-cache-time)
                (cached-files cj/org-agenda-files-cache))

            ;; Second call: uses cache
            (cj/build-org-agenda-list)

            ;; Scan count unchanged (cache hit)
            (should (= scan-count 1))

            ;; Cache unchanged
            (should (equal cj/org-agenda-files-cache-time cached-time))
            (should (equal cj/org-agenda-files-cache cached-files)))))
    (test-org-agenda-teardown)))

(ert-deftest test-org-agenda-build-list-normal-force-rebuild-bypasses-cache ()
  "Test that force-rebuild parameter bypasses cache.

When force-rebuild is non-nil:
1. Should ignore valid cache
2. Should rebuild from scratch
3. Should update cache with new data"
  (test-org-agenda-setup)
  (unwind-protect
      (let ((scan-count 0))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern &optional _include-dirs)
                     (setq scan-count (1+ scan-count))
                     (if (> scan-count 1)
                         '("/tmp/project/todo.org" "/tmp/project2/todo.org")  ; New file on rebuild
                       '("/tmp/project/todo.org")))))

          ;; First call: builds cache
          (cj/build-org-agenda-list)
          (let ((initial-count (length org-agenda-files)))

            ;; Force rebuild
            (cj/build-org-agenda-list 'force)

            ;; Scanned again
            (should (= scan-count 2))

            ;; New files include additional project
            (should (> (length org-agenda-files) initial-count)))))
    (test-org-agenda-teardown)))

;;; Boundary Cases

(ert-deftest test-org-agenda-build-list-boundary-cache-expires-after-ttl ()
  "Test that cache expires after TTL period.

When cache timestamp exceeds TTL:
1. Should rebuild files list
2. Should update cache timestamp
3. Should rescan directory"
  (test-org-agenda-setup)
  (unwind-protect
      (let ((scan-count 0))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern &optional _include-dirs)
                     (setq scan-count (1+ scan-count))
                     '("/tmp/project/todo.org"))))

          ;; First call: builds cache
          (cj/build-org-agenda-list)
          (should (= scan-count 1))

          ;; Simulate cache expiration (set time to 2 hours ago)
          (setq cj/org-agenda-files-cache-time
                (- (float-time) (* 2 3600)))

          ;; Second call: cache expired, rebuild
          (cj/build-org-agenda-list)

          ;; Scanned again (cache was expired)
          (should (= scan-count 2))

          ;; Cache timestamp updated to current time
          (should (< (- (float-time) cj/org-agenda-files-cache-time) 1))))
    (test-org-agenda-teardown)))

(ert-deftest test-org-agenda-build-list-boundary-empty-directory-creates-minimal-list ()
  "Test behavior when directory contains no todo.org files.

When directory scan returns empty:
1. Should still create base files (inbox, schedule)
2. Should not fail or error
3. Should cache the minimal result"
  (test-org-agenda-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern &optional _include-dirs) nil)))  ; No files found

        (cj/build-org-agenda-list)

        ;; Should have base files only (inbox, schedule, gcal)
        (should (= (length org-agenda-files) 3))

        ;; Cache should contain base files
        (should cj/org-agenda-files-cache)
        (should (= (length cj/org-agenda-files-cache) 3)))
    (test-org-agenda-teardown)))

(ert-deftest test-org-agenda-build-list-boundary-building-flag-set-during-build ()
  "Test that building flag is set during build and cleared after.

During build:
1. Flag should be set to prevent concurrent builds
2. Flag should clear even if build fails
3. Flag state should be consistent"
  (test-org-agenda-setup)
  (unwind-protect
      (let ((flag-during-build nil))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern &optional _include-dirs)
                     ;; Capture flag state during directory scan
                     (setq flag-during-build cj/org-agenda-files-building)
                     '("/tmp/project/todo.org"))))

          ;; Before build
          (should (null cj/org-agenda-files-building))

          ;; Build
          (cj/build-org-agenda-list)

          ;; Flag was set during build
          (should flag-during-build)

          ;; Flag cleared after build
          (should (null cj/org-agenda-files-building))))
    (test-org-agenda-teardown)))

(ert-deftest test-org-agenda-build-list-boundary-building-flag-clears-on-error ()
  "Test that building flag clears even if build errors.

When build encounters error:
1. Flag should still be cleared (unwind-protect)
2. Prevents permanently locked state
3. Next build can proceed"
  (test-org-agenda-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern &optional _include-dirs)
                   (error "Simulated scan failure"))))

        ;; Build will error
        (should-error (cj/build-org-agenda-list))

        ;; Flag cleared despite error (unwind-protect)
        (should (null cj/org-agenda-files-building)))
    (test-org-agenda-teardown)))

;;; Error Cases

(ert-deftest test-org-agenda-build-list-error-nil-cache-with-old-timestamp ()
  "Test handling of inconsistent state (nil cache but timestamp set).

When cache is nil but timestamp exists:
1. Should recognize cache as invalid
2. Should rebuild files list
3. Should set both cache and timestamp"
  (test-org-agenda-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern &optional _include-dirs) '("/tmp/project/todo.org"))))

        ;; Set inconsistent state
        (setq cj/org-agenda-files-cache nil)
        (setq cj/org-agenda-files-cache-time (float-time))

        ;; Build should recognize invalid state
        (cj/build-org-agenda-list)

        ;; Cache now populated
        (should cj/org-agenda-files-cache)
        (should cj/org-agenda-files-cache-time)
        (should org-agenda-files))
    (test-org-agenda-teardown)))

(ert-deftest test-org-agenda-build-list-error-directory-scan-failure-propagates ()
  "Test that directory scan failures propagate as errors.

When directory-files-recursively errors:
1. Error should propagate to caller
2. Cache should not be corrupted
3. Building flag should clear"
  (test-org-agenda-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern &optional _include-dirs)
                   (error "Permission denied"))))

        ;; Should propagate error
        (should-error (cj/build-org-agenda-list))

        ;; Cache not corrupted (still nil)
        (should (null cj/org-agenda-files-cache))

        ;; Building flag cleared
        (should (null cj/org-agenda-files-building)))
    (test-org-agenda-teardown)))

(provide 'test-org-agenda-build-list)
;;; test-org-agenda-build-list.el ends here
