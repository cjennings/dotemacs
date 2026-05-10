;;; test-cj-cache.el --- Tests for cj-cache.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the TTL+building cache helper.  Covers cache-make /
;; cache-valid-p / cache-value-or-rebuild / cache-building-p /
;; cache-invalidate against the contract in
;; docs/design/cache-helper-design.org.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'cj-cache-lib)

;;; cj/cache-make

(ert-deftest test-cj-cache-make-default-ttl ()
  "Normal: a fresh cache has the default TTL when none specified."
  (let ((c (cj/cache-make)))
    (should (= 3600 (plist-get c :ttl)))
    (should-not (plist-get c :value))
    (should-not (plist-get c :time))
    (should-not (plist-get c :building))))

(ert-deftest test-cj-cache-make-custom-ttl ()
  "Normal: an explicit :ttl keyword sets the TTL field."
  (let ((c (cj/cache-make :ttl 60)))
    (should (= 60 (plist-get c :ttl)))))

;;; cj/cache-valid-p

(ert-deftest test-cj-cache-valid-fresh-cache-invalid ()
  "Boundary: a fresh cache with no value is not valid."
  (let ((c (cj/cache-make)))
    (should-not (cj/cache-valid-p c))))

(ert-deftest test-cj-cache-valid-recent-build-valid ()
  "Normal: a cache built one second ago is valid."
  (let ((c (cj/cache-make :ttl 60)))
    (plist-put c :value '(file1 file2))
    (plist-put c :time (- (float-time) 1))
    (should (cj/cache-valid-p c))))

(ert-deftest test-cj-cache-valid-expired-cache-invalid ()
  "Boundary: a cache older than TTL is invalid."
  (let ((c (cj/cache-make :ttl 60)))
    (plist-put c :value '(file1))
    (plist-put c :time (- (float-time) 120))
    (should-not (cj/cache-valid-p c))))

(ert-deftest test-cj-cache-valid-nil-value-treated-invalid ()
  "Boundary: a nil cached value reads as invalid -- a build that
returned nil legitimately will rebuild on the next request, matching
the prior agenda/refile contract."
  (let ((c (cj/cache-make :ttl 60)))
    (plist-put c :value nil)
    (plist-put c :time (float-time))
    (should-not (cj/cache-valid-p c))))

;;; cj/cache-value-or-rebuild

(ert-deftest test-cj-cache-value-or-rebuild-miss-calls-build ()
  "Normal: a fresh cache calls BUILD-FN and stores its result."
  (let* ((c (cj/cache-make))
         (build-calls 0)
         (result (cj/cache-value-or-rebuild
                  c
                  (lambda () (cl-incf build-calls) '(a b c)))))
    (should (= 1 build-calls))
    (should (equal '(a b c) result))
    (should (equal '(a b c) (plist-get c :value)))
    (should (numberp (plist-get c :time)))))

(ert-deftest test-cj-cache-value-or-rebuild-hit-skips-build ()
  "Normal: a valid cache returns the stored value without calling BUILD-FN."
  (let* ((c (cj/cache-make :ttl 60))
         (build-calls 0))
    (plist-put c :value '(cached))
    (plist-put c :time (- (float-time) 1))
    (let ((result (cj/cache-value-or-rebuild
                   c
                   (lambda () (cl-incf build-calls) '(rebuilt)))))
      (should (= 0 build-calls))
      (should (equal '(cached) result)))))

(ert-deftest test-cj-cache-value-or-rebuild-force-rebuild-overrides-hit ()
  "Normal: :force-rebuild bypasses a valid cache."
  (let* ((c (cj/cache-make :ttl 60))
         (build-calls 0))
    (plist-put c :value '(cached))
    (plist-put c :time (- (float-time) 1))
    (let ((result (cj/cache-value-or-rebuild
                   c
                   (lambda () (cl-incf build-calls) '(rebuilt))
                   :force-rebuild t)))
      (should (= 1 build-calls))
      (should (equal '(rebuilt) result)))))

(ert-deftest test-cj-cache-value-or-rebuild-on-hit-fires ()
  "Normal: :on-hit fires with the cached value when valid."
  (let* ((c (cj/cache-make :ttl 60))
         (hit-with nil))
    (plist-put c :value '(cached))
    (plist-put c :time (- (float-time) 1))
    (cj/cache-value-or-rebuild
     c
     (lambda () '(rebuilt))
     :on-hit (lambda (v) (setq hit-with v)))
    (should (equal '(cached) hit-with))))

(ert-deftest test-cj-cache-value-or-rebuild-on-build-callbacks-fire ()
  "Normal: :on-build-start and :on-build-success fire on a miss."
  (let* ((c (cj/cache-make))
         (events nil))
    (cj/cache-value-or-rebuild
     c
     (lambda () '(built))
     :on-build-start (lambda () (push 'start events))
     :on-build-success (lambda (v) (push (cons 'success v) events)))
    (should (equal '((success built) start) events))))

(ert-deftest test-cj-cache-value-or-rebuild-on-build-error-fires-and-rethrows ()
  "Error: :on-build-error fires with the error and the helper rethrows."
  (let* ((c (cj/cache-make))
         (caught-err nil))
    (should-error
     (cj/cache-value-or-rebuild
      c
      (lambda () (error "boom"))
      :on-build-error (lambda (err) (setq caught-err err))))
    (should caught-err)))

(ert-deftest test-cj-cache-value-or-rebuild-clears-building-flag-on-error ()
  "Boundary: building flag is cleared even when BUILD-FN signals."
  (let ((c (cj/cache-make)))
    (ignore-errors
      (cj/cache-value-or-rebuild
       c
       (lambda () (error "boom"))))
    (should-not (cj/cache-building-p c))))

(ert-deftest test-cj-cache-value-or-rebuild-clears-building-flag-on-success ()
  "Normal: building flag is cleared after a successful build."
  (let ((c (cj/cache-make)))
    (cj/cache-value-or-rebuild c (lambda () 'ok))
    (should-not (cj/cache-building-p c))))

;;; cj/cache-invalidate

(ert-deftest test-cj-cache-invalidate-clears-value-and-time ()
  "Normal: invalidate resets value and time, keeps TTL."
  (let ((c (cj/cache-make :ttl 60)))
    (plist-put c :value '(some))
    (plist-put c :time (float-time))
    (cj/cache-invalidate c)
    (should-not (plist-get c :value))
    (should-not (plist-get c :time))
    (should (= 60 (plist-get c :ttl)))))

(provide 'test-cj-cache)
;;; test-cj-cache.el ends here
