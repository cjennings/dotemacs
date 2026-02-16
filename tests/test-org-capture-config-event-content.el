;;; test-org-capture-config-event-content.el --- Tests for cj/org-capture-event-content -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/org-capture-event-content function from org-capture-config.el.
;;
;; Returns selected text for event capture, prioritizing:
;; 1. org-store-link-plist :initial (from browser via org-protocol)
;; 2. org-capture-plist :initial (from Emacs region)
;; 3. Empty string (no selection)
;;
;; Note: org-capture-plist is defined by org-capture at runtime. We ensure
;; it's declared before tests so let-binding works in batch mode.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

;; Ensure org-capture-plist is declared (normally defined by org-capture at runtime)
(defvar org-capture-plist nil)

;;; Normal Cases

(ert-deftest test-org-capture-config-event-content-normal-browser-selection ()
  "Browser selection via org-protocol should be returned with leading newline."
  (let ((org-store-link-plist '(:initial "selected from browser"))
        (org-capture-plist '(:initial "")))
    (should (equal (cj/org-capture-event-content)
                   "\nselected from browser"))))

(ert-deftest test-org-capture-config-event-content-normal-emacs-region ()
  "Emacs region selection should be returned with leading newline."
  (let ((org-store-link-plist nil)
        (org-capture-plist '(:initial "selected in emacs")))
    (should (equal (cj/org-capture-event-content)
                   "\nselected in emacs"))))

(ert-deftest test-org-capture-config-event-content-normal-no-selection ()
  "No selection in either plist should return empty string."
  (let ((org-store-link-plist nil)
        (org-capture-plist '(:initial "")))
    (should (equal (cj/org-capture-event-content) ""))))

(ert-deftest test-org-capture-config-event-content-normal-browser-takes-priority ()
  "Browser selection should take priority over Emacs region."
  (let ((org-store-link-plist '(:initial "from browser"))
        (org-capture-plist '(:initial "from emacs")))
    (should (equal (cj/org-capture-event-content)
                   "\nfrom browser"))))

;;; Boundary Cases

(ert-deftest test-org-capture-config-event-content-boundary-store-link-no-initial ()
  "org-store-link-plist without :initial should fall through to capture plist."
  (let ((org-store-link-plist '(:url "http://example.com"))
        (org-capture-plist '(:initial "from emacs")))
    (should (equal (cj/org-capture-event-content)
                   "\nfrom emacs"))))

(ert-deftest test-org-capture-config-event-content-boundary-store-link-empty-initial ()
  "org-store-link-plist with empty :initial should fall through, not produce stray newline."
  (let ((org-store-link-plist '(:initial ""))
        (org-capture-plist '(:initial "")))
    (should (equal (cj/org-capture-event-content) ""))))

(ert-deftest test-org-capture-config-event-content-boundary-capture-plist-nil-initial ()
  "Nil :initial in capture plist should return empty string."
  (let ((org-store-link-plist nil)
        (org-capture-plist '(:initial nil)))
    (should (equal (cj/org-capture-event-content) ""))))

(ert-deftest test-org-capture-config-event-content-boundary-multiline-selection ()
  "Multi-line selection should be preserved."
  (let ((org-store-link-plist nil)
        (org-capture-plist '(:initial "line one\nline two\nline three")))
    (should (equal (cj/org-capture-event-content)
                   "\nline one\nline two\nline three"))))

(ert-deftest test-org-capture-config-event-content-boundary-both-plists-nil ()
  "Both plists nil should return empty string."
  (let ((org-store-link-plist nil)
        (org-capture-plist nil))
    (should (equal (cj/org-capture-event-content) ""))))

(provide 'test-org-capture-config-event-content)
;;; test-org-capture-config-event-content.el ends here
