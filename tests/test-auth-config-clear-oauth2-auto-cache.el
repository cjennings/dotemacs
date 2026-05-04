;;; test-auth-config-clear-oauth2-auto-cache.el --- Tests for clear-oauth2-auto-cache -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the in-memory oauth2-auto cache is cleared when bound, and the
;; not-loaded message is shown when unbound.  The error test restores the
;; binding in an unwind-protect so other tests in the same Emacs session
;; do not hit a void-variable error.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'auth-source)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun test-auth-config-clear-oauth2--load ()
  "Load auth-config with external process calls stubbed."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args) 0)))
    (load (expand-file-name "modules/auth-config.el" user-emacs-directory)
          nil t)))

(ert-deftest test-auth-config-clear-oauth2-cache-normal-bound-with-entries-clears ()
  "Normal: bound cache with entries gets cleared, success message shown."
  (test-auth-config-clear-oauth2--load)
  (let ((cache (make-hash-table :test 'equal))
        (last-message nil))
    (puthash "k1" "v1" cache)
    (puthash "k2" "v2" cache)
    (setq oauth2-auto--plstore-cache cache)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest _) (setq last-message fmt))))
      (cj/clear-oauth2-auto-cache)
      (should (zerop (hash-table-count oauth2-auto--plstore-cache)))
      (should (string-match-p "oauth2-auto token cache cleared" last-message)))))

(ert-deftest test-auth-config-clear-oauth2-cache-boundary-bound-empty-stays-empty ()
  "Boundary: empty bound cache stays empty, success message still shown."
  (test-auth-config-clear-oauth2--load)
  (let ((cache (make-hash-table :test 'equal))
        (last-message nil))
    (setq oauth2-auto--plstore-cache cache)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest _) (setq last-message fmt))))
      (cj/clear-oauth2-auto-cache)
      (should (zerop (hash-table-count oauth2-auto--plstore-cache)))
      (should (string-match-p "oauth2-auto token cache cleared" last-message)))))

(ert-deftest test-auth-config-clear-oauth2-cache-error-unbound-warns ()
  "Error: unbound oauth2-auto cache var triggers warning message."
  (test-auth-config-clear-oauth2--load)
  (when (boundp 'oauth2-auto--plstore-cache)
    (makunbound 'oauth2-auto--plstore-cache))
  (unwind-protect
      (let ((last-message nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest _) (setq last-message fmt))))
          (cj/clear-oauth2-auto-cache)
          (should (string-match-p "oauth2-auto not loaded yet" last-message))))
    (setq oauth2-auto--plstore-cache (make-hash-table :test 'equal))))

(provide 'test-auth-config-clear-oauth2-auto-cache)
;;; test-auth-config-clear-oauth2-auto-cache.el ends here
