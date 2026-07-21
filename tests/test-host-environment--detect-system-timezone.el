;;; test-host-environment--detect-system-timezone.el --- Tests for cj/detect-system-timezone -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/detect-system-timezone'.  The function tries four
;; detection methods in priority order: file-content match against
;; zoneinfo, the TZ env var, /etc/timezone, and the /etc/localtime
;; symlink target.  Tests mock the first two methods to verify the
;; priority chain without touching real system files.  Methods 3 and
;; 4 (file I/O on /etc) are exercised end-to-end on the real host but
;; not asserted strictly — those would be brittle across machines.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'host-environment)

(ert-deftest test-host-environment-detect-tz-env-wins-without-content-scan ()
  "Normal: an explicit TZ wins and the exhaustive zoneinfo scan never runs.
The scan reads hundreds of files; it used to run first on every call even
when a cheap O(1) method would answer."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () (error "content scan should not have run")))
            ((symbol-function 'getenv)
             (lambda (name &rest _) (when (string= name "TZ") "America/Chicago"))))
    (should (equal (cj/detect-system-timezone) "America/Chicago"))))

(ert-deftest test-host-environment-detect-tz-content-scan-is-last-resort ()
  "Boundary: with every cheap method empty, the content scan still answers."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () "America/Los_Angeles"))
            ((symbol-function 'getenv) (lambda (&rest _) nil))
            ((symbol-function 'file-exists-p) (lambda (&rest _) nil))
            ((symbol-function 'file-symlink-p) (lambda (&rest _) nil)))
    (should (equal (cj/detect-system-timezone) "America/Los_Angeles"))))

(ert-deftest test-host-environment-detect-tz-env-var-wins-when-match-nil ()
  "Normal: with match-localtime nil, the TZ env var is used."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () nil))
            ((symbol-function 'getenv)
             (lambda (name &rest _) (when (string= name "TZ") "Europe/Berlin"))))
    (should (equal (cj/detect-system-timezone) "Europe/Berlin"))))

(ert-deftest test-host-environment-detect-tz-falls-through-to-etc-timezone ()
  "Boundary: with match-localtime and TZ both nil, /etc/timezone is read.
Uses a real temp file substituted via cl-letf on the file-existence and
contents primitives."
  (let ((fake-tz "Asia/Tokyo"))
    (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
               (lambda () nil))
              ((symbol-function 'getenv)
               (lambda (_ &rest _) nil))
              ((symbol-function 'file-exists-p)
               (lambda (path) (string= path "/etc/timezone")))
              ((symbol-function 'insert-file-contents)
               (lambda (path &rest _)
                 (when (string= path "/etc/timezone")
                   (insert fake-tz "\n")))))
      (should (equal (cj/detect-system-timezone) fake-tz)))))

(ert-deftest test-host-environment-detect-tz-trims-etc-timezone-whitespace ()
  "Boundary: trailing whitespace in /etc/timezone is trimmed."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () nil))
            ((symbol-function 'getenv)
             (lambda (_ &rest _) nil))
            ((symbol-function 'file-exists-p)
             (lambda (path) (string= path "/etc/timezone")))
            ((symbol-function 'insert-file-contents)
             (lambda (path &rest _)
               (when (string= path "/etc/timezone")
                 (insert "  America/Chicago\n\n")))))
    (should (equal (cj/detect-system-timezone) "America/Chicago"))))

(ert-deftest test-host-environment-detect-tz-returns-nil-when-all-fail ()
  "Error: returns nil when every detection method fails."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () nil))
            ((symbol-function 'getenv)
             (lambda (_ &rest _) nil))
            ((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'file-symlink-p) (lambda (_) nil)))
    (should-not (cj/detect-system-timezone))))

(ert-deftest test-host-environment-detect-tz-symlink-target-extracts-zone ()
  "Boundary: with methods 1-3 nil, a /etc/localtime symlink into zoneinfo
yields the zone after the /zoneinfo/ segment."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () nil))
            ((symbol-function 'getenv) (lambda (_ &rest _) nil))
            ((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'file-symlink-p)
             (lambda (path) (string= path "/etc/localtime")))
            ((symbol-function 'file-truename)
             (lambda (_ &rest _) "/usr/share/zoneinfo/America/Denver")))
    (should (equal (cj/detect-system-timezone) "America/Denver"))))

(ert-deftest test-host-environment-detect-tz-symlink-without-zoneinfo-is-nil ()
  "Error: a symlink target with no /zoneinfo/ segment yields nil."
  (cl-letf (((symbol-function 'cj/match-localtime-to-zoneinfo)
             (lambda () nil))
            ((symbol-function 'getenv) (lambda (_ &rest _) nil))
            ((symbol-function 'file-exists-p) (lambda (_) nil))
            ((symbol-function 'file-symlink-p)
             (lambda (path) (string= path "/etc/localtime")))
            ((symbol-function 'file-truename)
             (lambda (_ &rest _) "/var/lib/elsewhere/localtime")))
    (should-not (cj/detect-system-timezone))))

(provide 'test-host-environment--detect-system-timezone)
;;; test-host-environment--detect-system-timezone.el ends here
