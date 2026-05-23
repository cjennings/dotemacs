;;; test-system-lib-auth-source-secret-value.el --- Tests for the auth-source secret primitive -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/auth-source-secret-value' is the shared low-level accessor that the
;; calendar-sync, ai-config, transcription, and slack helpers all delegate to.
;; It searches authinfo for HOST (and optional USER), resolves a
;; function-valued secret by calling it, and returns the value or nil.  These
;; tests stub `auth-source-search' (the external boundary) and capture its args.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-lib)

(defvar test-ass--args nil "Captured args of the stubbed `auth-source-search'.")

(defmacro test-ass--with-search (return-value &rest body)
  "Run BODY with `auth-source-search' stubbed to return RETURN-VALUE.
Captures the call args in `test-ass--args'."
  (declare (indent 1))
  `(let ((test-ass--args nil))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest args) (setq test-ass--args args) ,return-value)))
       ,@body)))

;;; Normal

(ert-deftest test-auth-source-secret-value-returns-string-secret ()
  "Normal: a string :secret is returned as-is."
  (test-ass--with-search (list (list :secret "sk-abc"))
    (should (equal "sk-abc" (cj/auth-source-secret-value "api.example.com")))))

(ert-deftest test-auth-source-secret-value-calls-function-secret ()
  "Normal: a function :secret is funcalled (the netrc backend returns one)."
  (test-ass--with-search (list (list :secret (lambda () "from-fn")))
    (should (equal "from-fn" (cj/auth-source-secret-value "api.example.com")))))

(ert-deftest test-auth-source-secret-value-passes-user-when-given ()
  "Normal: USER and HOST are forwarded to `auth-source-search'."
  (test-ass--with-search (list (list :secret "x"))
    (cj/auth-source-secret-value "h" "apikey")
    (should (equal "h" (plist-get test-ass--args :host)))
    (should (equal "apikey" (plist-get test-ass--args :user)))))

;;; Boundary

(ert-deftest test-auth-source-secret-value-omits-user-when-absent ()
  "Boundary: with no USER, :user is not added to the search spec."
  (test-ass--with-search (list (list :secret "x"))
    (cj/auth-source-secret-value "h")
    (should-not (plist-member test-ass--args :user))))

(ert-deftest test-auth-source-secret-value-nil-on-no-match ()
  "Boundary: no matching entry yields nil."
  (test-ass--with-search nil
    (should (null (cj/auth-source-secret-value "h")))))

(ert-deftest test-auth-source-secret-value-nil-on-entry-without-secret ()
  "Boundary: a matching entry with no :secret yields nil."
  (test-ass--with-search (list (list :host "h"))
    (should (null (cj/auth-source-secret-value "h")))))

(provide 'test-system-lib-auth-source-secret-value)
;;; test-system-lib-auth-source-secret-value.el ends here
