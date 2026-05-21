;;; test-calendar-sync--calendar-url.el --- Tests for feed URL resolution  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `calendar-sync--calendar-url', which resolves the .ics
;; feed URL for a calendar plist.  An explicit :url wins; otherwise the
;; URL is looked up in auth-source (e.g. ~/.authinfo.gpg) under the
;; calendar's :secret-host.  auth-source-search is mocked at the boundary
;; so tests never touch disk or GPG.  Covers Normal, Boundary, and Error.

;;; Code:

(require 'ert)
(require 'calendar-sync)

(defmacro test-cs-url--with-auth (result &rest body)
  "Run BODY with `auth-source-search' stubbed to return RESULT.
Records each call's :host argument into `calls', a binding BODY can read."
  (declare (indent 1))
  `(let ((calls '()))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest args)
                  (push (plist-get args :host) calls)
                  ,result)))
       ,@body)))

;;; Normal

(ert-deftest test-calendar-sync--calendar-url-normal-explicit-url ()
  "Normal: an explicit :url is returned verbatim."
  (test-cs-url--with-auth nil
    (should (equal "https://example.com/feed.ics"
                   (calendar-sync--calendar-url
                    '(:name "x" :url "https://example.com/feed.ics"))))
    ;; auth-source must not be consulted when :url is present
    (should (null calls))))

(ert-deftest test-calendar-sync--calendar-url-normal-string-secret ()
  "Normal: :secret-host resolves via auth-source with a string secret."
  (test-cs-url--with-auth (list (list :host "calendar-google"
                                      :secret "https://g.example/basic.ics"))
    (should (equal "https://g.example/basic.ics"
                   (calendar-sync--calendar-url
                    '(:name "google" :secret-host "calendar-google"))))
    (should (equal '("calendar-google") calls))))

(ert-deftest test-calendar-sync--calendar-url-normal-function-secret ()
  "Normal: a function-valued :secret is funcalled (the netrc backend's form)."
  (test-cs-url--with-auth (list (list :host "calendar-proton"
                                      :secret (lambda () "https://p.example/cal.ics")))
    (should (equal "https://p.example/cal.ics"
                   (calendar-sync--calendar-url
                    '(:name "proton" :secret-host "calendar-proton"))))))

;;; Boundary

(ert-deftest test-calendar-sync--calendar-url-boundary-explicit-url-wins ()
  "Boundary: when both :url and :secret-host are set, :url wins and
auth-source is never consulted."
  (test-cs-url--with-auth (list (list :host "h" :secret "from-authinfo"))
    (should (equal "from-url"
                   (calendar-sync--calendar-url
                    '(:name "x" :url "from-url" :secret-host "h"))))
    (should (null calls))))

;;; Error

(ert-deftest test-calendar-sync--calendar-url-error-neither-key ()
  "Error: a calendar with neither :url nor :secret-host yields nil."
  (test-cs-url--with-auth (list (list :host "h" :secret "should-not-reach"))
    (should (null (calendar-sync--calendar-url '(:name "x" :file "f"))))
    (should (null calls))))

(ert-deftest test-calendar-sync--calendar-url-error-no-match ()
  "Error: :secret-host with no auth-source match yields nil."
  (test-cs-url--with-auth nil
    (should (null (calendar-sync--calendar-url
                   '(:name "x" :secret-host "calendar-missing"))))))

(ert-deftest test-calendar-sync--calendar-url-error-match-without-secret ()
  "Error: a match lacking a :secret yields nil, not an error."
  (test-cs-url--with-auth (list (list :host "calendar-google"))
    (should (null (calendar-sync--calendar-url
                   '(:name "x" :secret-host "calendar-google"))))))

(provide 'test-calendar-sync--calendar-url)
;;; test-calendar-sync--calendar-url.el ends here
