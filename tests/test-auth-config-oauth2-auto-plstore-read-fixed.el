;;; test-auth-config-oauth2-auto-plstore-read-fixed.el --- Tests for oauth2 cache fix -*- lexical-binding: t; -*-

;;; Commentary:
;; Confirms the oauth2-auto plstore-read advice caches reads on miss,
;; skips plstore-open on hit, and runs plstore-close even when
;; plstore-get signals.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'auth-source)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar oauth2-auto--plstore-cache nil
  "Stub for oauth2-auto's in-memory token cache.")

(defvar oauth2-auto-plstore "/tmp/test-oauth2-stub.plist"
  "Stub for oauth2-auto plstore path.")

(defun test-auth-config-oauth2--load ()
  "Load auth-config with external process calls stubbed."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args) 0)))
    (load (expand-file-name "modules/auth-config.el" user-emacs-directory)
          nil t))
  (setq oauth2-auto--plstore-cache (make-hash-table :test 'equal)))

(ert-deftest test-auth-config-oauth2-plstore-read-normal-cache-miss-reads-and-caches ()
  "Normal: cache miss opens plstore, reads value, caches, returns value."
  (test-auth-config-oauth2--load)
  (let ((opens 0)
        (closes 0))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'oauth2-auto--compute-id)
               (lambda (_u _p) "stable-id"))
              ((symbol-function 'plstore-open)
               (lambda (_) (cl-incf opens) 'fake-plstore))
              ((symbol-function 'plstore-get)
               (lambda (_p id) (cons id "secret-value")))
              ((symbol-function 'plstore-close)
               (lambda (_) (cl-incf closes))))
      (let ((result (cj/oauth2-auto--plstore-read-fixed "u" "p")))
        (should (equal result "secret-value"))
        (should (= opens 1))
        (should (= closes 1))
        (should (equal (gethash "stable-id" oauth2-auto--plstore-cache)
                       "secret-value"))))))

(ert-deftest test-auth-config-oauth2-plstore-read-boundary-cache-hit-skips-plstore ()
  "Boundary: cache hit returns cached value without opening plstore."
  (test-auth-config-oauth2--load)
  (puthash "stable-id" "cached-value" oauth2-auto--plstore-cache)
  (let ((opens 0))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'oauth2-auto--compute-id)
               (lambda (_u _p) "stable-id"))
              ((symbol-function 'plstore-open)
               (lambda (_) (cl-incf opens) 'fake-plstore))
              ((symbol-function 'plstore-get)
               (lambda (_p _id) (error "plstore-get should not be called")))
              ((symbol-function 'plstore-close)
               (lambda (_) nil)))
      (let ((result (cj/oauth2-auto--plstore-read-fixed "u" "p")))
        (should (equal result "cached-value"))
        (should (zerop opens))))))

(ert-deftest test-auth-config-oauth2-plstore-read-error-plstore-close-runs-after-get-fails ()
  "Error: plstore-close runs even when plstore-get signals an error."
  (test-auth-config-oauth2--load)
  (let ((closes 0))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'oauth2-auto--compute-id)
               (lambda (_u _p) "stable-id"))
              ((symbol-function 'plstore-open)
               (lambda (_) 'fake-plstore))
              ((symbol-function 'plstore-get)
               (lambda (_p _id) (error "boom")))
              ((symbol-function 'plstore-close)
               (lambda (_) (cl-incf closes))))
      (should-error (cj/oauth2-auto--plstore-read-fixed "u" "p"))
      (should (= closes 1)))))

(provide 'test-auth-config-oauth2-auto-plstore-read-fixed)
;;; test-auth-config-oauth2-auto-plstore-read-fixed.el ends here
