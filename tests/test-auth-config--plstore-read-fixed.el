;;; test-auth-config--plstore-read-fixed.el --- Tests for the oauth2-auto cache fix -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for `cj/oauth2-auto--plstore-read-fixed' in auth-config.el — the
;; advice that re-enables oauth2-auto's plstore cache.  oauth2-auto is not
;; installed here, so its symbols and the plstore I/O are stubbed at the
;; boundary; the function's own logic (cache-first read, puthash, the
;; unwind-protect close) runs for real.  `require' is stubbed to no-op only
;; for oauth2-auto (other requires delegate through), satisfying the
;; function's `(require 'oauth2-auto)' without loading or provide-ing the
;; package (a provide would fire auth-config's advice-add side effect).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'plstore)
(require 'auth-config)

;; Declared special so the function (which reads these as free package
;; globals) sees the dynamic let-bindings the tests establish.
(defvar oauth2-auto--plstore-cache nil)
(defvar oauth2-auto-plstore nil)

(defvar test-auth--open-count 0 "Times plstore-open was called in a test.")
(defvar test-auth--closed nil "Whether plstore-close ran in a test.")
(defvar test-auth--get-fn nil "Stub behavior for plstore-get: (lambda (ps id) ...).")

(defmacro test-auth--with-env (&rest body)
  "Run BODY with a faked oauth2-auto + plstore environment.
Resets the open counter and closed flag and gives a fresh cache each time."
  (declare (indent 0))
  `(let* ((oauth2-auto--plstore-cache (make-hash-table :test 'equal))
          (oauth2-auto-plstore "/tmp/oauth2-test.plist")
          (test-auth--open-count 0)
          (test-auth--closed nil)
          (orig-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'require)
                (lambda (feat &rest args)
                  (if (eq feat 'oauth2-auto)
                      'oauth2-auto
                    (apply orig-require feat args))))
               ((symbol-function 'oauth2-auto--compute-id)
                (lambda (_u _p) "ID"))
               ((symbol-function 'plstore-open)
                (lambda (_f) (cl-incf test-auth--open-count) 'PS))
               ((symbol-function 'plstore-get)
                (lambda (ps id) (funcall test-auth--get-fn ps id)))
               ((symbol-function 'plstore-close)
                (lambda (_p) (setq test-auth--closed t))))
       ,@body)))

;;; Normal Cases

(ert-deftest test-auth-config-plstore-read-fixed-cache-hit ()
  "Normal: a cache hit returns the cached value without opening the plstore."
  (let ((test-auth--get-fn (lambda (_ps _id) (error "should not read"))))
    (test-auth--with-env
      (puthash "ID" "CACHED" oauth2-auto--plstore-cache)
      (should (equal (cj/oauth2-auto--plstore-read-fixed "u" "p") "CACHED"))
      (should (= test-auth--open-count 0)))))

(ert-deftest test-auth-config-plstore-read-fixed-cache-miss-reads-and-caches ()
  "Normal: a miss reads from the plstore, caches the value, and closes."
  (let ((test-auth--get-fn (lambda (_ps id) (cons id "TOK"))))
    (test-auth--with-env
      (should (equal (cj/oauth2-auto--plstore-read-fixed "u" "p") "TOK"))
      (should (equal (gethash "ID" oauth2-auto--plstore-cache) "TOK"))
      (should (= test-auth--open-count 1))
      (should test-auth--closed))))

;;; Boundary Cases

(ert-deftest test-auth-config-plstore-read-fixed-value-cached-after-first-read ()
  "Boundary: a non-nil value is cached, so a second call does not re-open."
  (let ((test-auth--get-fn (lambda (_ps id) (cons id "TOK"))))
    (test-auth--with-env
      (cj/oauth2-auto--plstore-read-fixed "u" "p")
      (cj/oauth2-auto--plstore-read-fixed "u" "p")
      (should (= test-auth--open-count 1)))))

(ert-deftest test-auth-config-plstore-read-fixed-nil-value-rereads ()
  "Boundary: a nil value caches nil, so every call re-opens the plstore.
This documents current behavior — `gethash' on a nil entry is a miss."
  (let ((test-auth--get-fn (lambda (_ps _id) (cons "ID" nil))))
    (test-auth--with-env
      (should-not (cj/oauth2-auto--plstore-read-fixed "u" "p"))
      (should-not (cj/oauth2-auto--plstore-read-fixed "u" "p"))
      (should (= test-auth--open-count 2)))))

;;; Error Cases

(ert-deftest test-auth-config-plstore-read-fixed-closes-on-error ()
  "Error: a read failure still closes the plstore via unwind-protect."
  (let ((test-auth--get-fn (lambda (&rest _) (error "boom"))))
    (test-auth--with-env
      (should-error (cj/oauth2-auto--plstore-read-fixed "u" "p"))
      (should test-auth--closed))))

(provide 'test-auth-config--plstore-read-fixed)
;;; test-auth-config--plstore-read-fixed.el ends here
