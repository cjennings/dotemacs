;;; test-browser-config--preferred-default.el --- Tests for the first-run browser pick -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--preferred-default-browser, the pure helper that picks
;; the first-run default when no saved choice exists.
;;
;; The behavior it fixes: EWW is listed first in `cj/browser-definitions' and
;; carries a nil executable, so `cj/discover-browsers' always reports it as
;; available and it sorted first.  A fresh machine therefore opened every org
;; link in the Emacs text browser until the user found cj/choose-browser, even
;; with Chrome or Firefox installed.  The helper prefers a real external
;; browser and keeps EWW as the genuine last resort.
;;
;; The helper takes the discovered list as an argument rather than calling
;; `cj/discover-browsers' itself, so these tests drive real data structures
;; and never stub executable-find.
;;
;; Test organization:
;; - Normal Cases: external browser preferred over a leading built-in
;; - Boundary Cases: only built-ins, only externals, single entry, empty list
;; - Error Cases: entries missing the :executable key
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'browser-config)

(defconst test-browser--eww
  '(:executable nil :function eww-browse-url :name "EWW (Emacs Browser)"
                :path nil :program-var nil))

(defconst test-browser--chrome
  '(:executable "google-chrome" :function browse-url-chrome :name "Google Chrome"
                :path "/usr/bin/google-chrome" :program-var browse-url-chrome-program))

(defconst test-browser--firefox
  '(:executable "firefox" :function browse-url-firefox :name "Firefox"
                :path "/usr/bin/firefox" :program-var browse-url-firefox-program))

;;; Normal Cases

(ert-deftest test-browser-config-preferred-default-skips-leading-builtin ()
  "Normal: an installed external browser wins over a built-in listed first."
  (should (equal (cj/--preferred-default-browser
                  (list test-browser--eww test-browser--chrome))
                 test-browser--chrome)))

(ert-deftest test-browser-config-preferred-default-keeps-external-order ()
  "Normal: the FIRST external in list order wins, not merely any external."
  (should (equal (cj/--preferred-default-browser
                  (list test-browser--eww test-browser--chrome test-browser--firefox))
                 test-browser--chrome)))

;;; Boundary Cases

(ert-deftest test-browser-config-preferred-default-builtin-only-falls-back ()
  "Boundary: with no external installed, the built-in is still chosen."
  (should (equal (cj/--preferred-default-browser (list test-browser--eww))
                 test-browser--eww)))

(ert-deftest test-browser-config-preferred-default-external-only ()
  "Boundary: a list of externals returns the first one."
  (should (equal (cj/--preferred-default-browser
                  (list test-browser--firefox test-browser--chrome))
                 test-browser--firefox)))

(ert-deftest test-browser-config-preferred-default-empty-list-is-nil ()
  "Boundary: an empty discovery result yields nil, never an error."
  (should (null (cj/--preferred-default-browser '()))))

(ert-deftest test-browser-config-preferred-default-single-builtin ()
  "Boundary: a one-element built-in list returns that element."
  (should (equal (cj/--preferred-default-browser (list test-browser--eww))
                 test-browser--eww)))

;;; Error Cases

(ert-deftest test-browser-config-preferred-default-missing-executable-key ()
  "Error: a plist with no :executable key counts as built-in, not a crash."
  (let ((malformed '(:name "Odd" :function ignore)))
    (should (equal (cj/--preferred-default-browser
                    (list malformed test-browser--chrome))
                   test-browser--chrome))
    (should (equal (cj/--preferred-default-browser (list malformed))
                   malformed))))

(provide 'test-browser-config--preferred-default)
;;; test-browser-config--preferred-default.el ends here
