;;; test-org-refile-build-targets.el --- Tests for cj/build-org-refile-targets -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/build-org-refile-targets caching logic.
;; Tests cache behavior, TTL expiration, force rebuild, and async build flag.

;;; Code:

(require 'ert)

;; Add modules to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar inbox-file "/tmp/test-inbox.org")
(defvar reference-file "/tmp/test-reference.org")
(defvar schedule-file "/tmp/test-schedule.org")
(defvar user-emacs-directory "/tmp/test-emacs.d/")
(defvar code-dir "/tmp/test-code/")
(defvar projects-dir "/tmp/test-projects/")

;; Now load the actual production module
(require 'org-refile-config)

;;; Setup and Teardown

(defun test-org-refile-setup ()
  "Reset cache and state before each test."
  (setq cj/org-refile-targets-cache nil)
  (setq cj/org-refile-targets-cache-time nil)
  (setq cj/org-refile-targets-building nil)
  (setq org-refile-targets nil))

(defun test-org-refile-teardown ()
  "Clean up after each test."
  (setq cj/org-refile-targets-cache nil)
  (setq cj/org-refile-targets-cache-time nil)
  (setq cj/org-refile-targets-building nil)
  (setq org-refile-targets nil))

;;; Normal Cases

(ert-deftest test-org-refile-build-targets-normal-first-call-builds-cache ()
  "Test that first call builds cache from scratch.

When cache is empty, function should:
1. Scan directories for todo.org files
2. Build refile targets list
3. Populate cache
4. Set cache timestamp"
  (test-org-refile-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern) '("/tmp/todo.org")))
                ((symbol-function 'fboundp) (lambda (_sym) nil)))

        ;; Before call: cache empty
        (should (null cj/org-refile-targets-cache))
        (should (null cj/org-refile-targets-cache-time))

        ;; Build targets
        (cj/build-org-refile-targets)

        ;; After call: cache populated
        (should cj/org-refile-targets-cache)
        (should cj/org-refile-targets-cache-time)
        (should org-refile-targets)

        ;; Cache matches org-refile-targets
        (should (equal cj/org-refile-targets-cache org-refile-targets))

        ;; Contains base files (inbox, reference, schedule)
        (should (>= (length org-refile-targets) 3)))
    (test-org-refile-teardown)))

(ert-deftest test-org-refile-build-targets-normal-second-call-uses-cache ()
  "Test that second call uses cache instead of rebuilding.

When cache is valid (not expired):
1. Should NOT scan directories again
2. Should restore targets from cache
3. Should NOT update cache timestamp"
  (test-org-refile-setup)
  (unwind-protect
      (let ((scan-count 0))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern)
                     (setq scan-count (1+ scan-count))
                     '("/tmp/todo.org")))
                  ((symbol-function 'fboundp) (lambda (_sym) nil)))

          ;; First call: builds cache
          (cj/build-org-refile-targets)
          (should (= scan-count 3))  ; 3 directories scanned

          (let ((cached-time cj/org-refile-targets-cache-time)
                (cached-targets cj/org-refile-targets-cache))

            ;; Second call: uses cache
            (cj/build-org-refile-targets)

            ;; Scan count unchanged (cache hit)
            (should (= scan-count 3))

            ;; Cache unchanged
            (should (equal cj/org-refile-targets-cache-time cached-time))
            (should (equal cj/org-refile-targets-cache cached-targets)))))
    (test-org-refile-teardown)))

(ert-deftest test-org-refile-build-targets-normal-force-rebuild-bypasses-cache ()
  "Test that force-rebuild parameter bypasses cache.

When force-rebuild is non-nil:
1. Should ignore valid cache
2. Should rebuild from scratch
3. Should update cache with new data"
  (test-org-refile-setup)
  (unwind-protect
      (let ((scan-count 0))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern)
                     (setq scan-count (1+ scan-count))
                     (if (> scan-count 3)
                         '("/tmp/todo.org" "/tmp/todo2.org")  ; New file on rebuild
                       '("/tmp/todo.org"))))
                  ((symbol-function 'fboundp) (lambda (_sym) nil)))

          ;; First call: builds cache
          (cj/build-org-refile-targets)
          (let ((initial-count (length org-refile-targets)))

            ;; Force rebuild
            (cj/build-org-refile-targets 'force)

            ;; Scanned again (3 more directories)
            (should (= scan-count 6))

            ;; New targets include additional file
            (should (> (length org-refile-targets) initial-count)))))
    (test-org-refile-teardown)))

;;; Boundary Cases

(ert-deftest test-org-refile-build-targets-boundary-cache-expires-after-ttl ()
  "Test that cache expires after TTL period.

When cache timestamp exceeds TTL:
1. Should rebuild targets
2. Should update cache timestamp
3. Should rescan directories"
  (test-org-refile-setup)
  (unwind-protect
      (let ((scan-count 0))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern)
                     (setq scan-count (1+ scan-count))
                     '("/tmp/todo.org")))
                  ((symbol-function 'fboundp) (lambda (_sym) nil)))

          ;; First call: builds cache
          (cj/build-org-refile-targets)
          (should (= scan-count 3))

          ;; Simulate cache expiration (set time to 2 hours ago)
          (setq cj/org-refile-targets-cache-time
                (- (float-time) (* 2 3600)))

          ;; Second call: cache expired, rebuild
          (cj/build-org-refile-targets)

          ;; Scanned again (cache was expired)
          (should (= scan-count 6))

          ;; Cache timestamp updated to current time
          (should (< (- (float-time) cj/org-refile-targets-cache-time) 1))))
    (test-org-refile-teardown)))

(ert-deftest test-org-refile-build-targets-boundary-empty-directories-creates-minimal-targets ()
  "Test behavior when directories contain no todo.org files.

When directory scans return empty:
1. Should still create base targets (inbox, reference, schedule)
2. Should not fail or error
3. Should cache the minimal result"
  (test-org-refile-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern) nil))  ; No files found
                ((symbol-function 'fboundp) (lambda (_sym) nil)))

        (cj/build-org-refile-targets)

        ;; Should have base files only
        (should (= (length org-refile-targets) 3))

        ;; Cache should contain base files
        (should cj/org-refile-targets-cache)
        (should (= (length cj/org-refile-targets-cache) 3)))
    (test-org-refile-teardown)))

(ert-deftest test-org-refile-build-targets-boundary-building-flag-set-during-build ()
  "Test that building flag is set during build and cleared after.

During build:
1. Flag should be set to prevent concurrent builds
2. Flag should clear even if build fails
3. Flag state should be consistent"
  (test-org-refile-setup)
  (unwind-protect
      (let ((flag-during-build nil))
        (cl-letf (((symbol-function 'directory-files-recursively)
                   (lambda (_dir _pattern)
                     ;; Capture flag state during directory scan
                     (setq flag-during-build cj/org-refile-targets-building)
                     '("/tmp/todo.org")))
                  ((symbol-function 'fboundp) (lambda (_sym) nil)))

          ;; Before build
          (should (null cj/org-refile-targets-building))

          ;; Build
          (cj/build-org-refile-targets)

          ;; Flag was set during build
          (should flag-during-build)

          ;; Flag cleared after build
          (should (null cj/org-refile-targets-building))))
    (test-org-refile-teardown)))

(ert-deftest test-org-refile-build-targets-boundary-building-flag-clears-on-error ()
  "Test that building flag clears even if build errors.

When build encounters error:
1. Flag should still be cleared (unwind-protect)
2. Prevents permanently locked state
3. Next build can proceed"
  (test-org-refile-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern)
                   (error "Simulated scan failure")))
                ((symbol-function 'fboundp) (lambda (_sym) nil)))

        ;; Build will error
        (should-error (cj/build-org-refile-targets))

        ;; Flag cleared despite error (unwind-protect)
        (should (null cj/org-refile-targets-building)))
    (test-org-refile-teardown)))

;;; Error Cases

(ert-deftest test-org-refile-build-targets-error-nil-cache-with-old-timestamp ()
  "Test handling of inconsistent state (nil cache but timestamp set).

When cache is nil but timestamp exists:
1. Should recognize cache as invalid
2. Should rebuild targets
3. Should set both cache and timestamp"
  (test-org-refile-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern) '("/tmp/todo.org")))
                ((symbol-function 'fboundp) (lambda (_sym) nil)))

        ;; Set inconsistent state
        (setq cj/org-refile-targets-cache nil)
        (setq cj/org-refile-targets-cache-time (float-time))

        ;; Build should recognize invalid state
        (cj/build-org-refile-targets)

        ;; Cache now populated
        (should cj/org-refile-targets-cache)
        (should cj/org-refile-targets-cache-time)
        (should org-refile-targets))
    (test-org-refile-teardown)))

(ert-deftest test-org-refile-build-targets-error-directory-scan-failure-propagates ()
  "Test that directory scan failures propagate as errors.

When directory-files-recursively errors:
1. Error should propagate to caller
2. Cache should not be corrupted
3. Building flag should clear"
  (test-org-refile-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (_dir _pattern)
                   (error "Permission denied")))
                ((symbol-function 'fboundp) (lambda (_sym) nil)))

        ;; Should propagate error
        (should-error (cj/build-org-refile-targets))

        ;; Cache not corrupted (still nil)
        (should (null cj/org-refile-targets-cache))

        ;; Building flag cleared
        (should (null cj/org-refile-targets-building)))
    (test-org-refile-teardown)))

(provide 'test-org-refile-build-targets)
;;; test-org-refile-build-targets.el ends here
