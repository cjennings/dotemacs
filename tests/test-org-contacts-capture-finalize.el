;;; test-org-contacts-capture-finalize.el --- Tests for org-contacts capture template finalization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;; Author: Craig Jennings <c@cjennings.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit tests for the org-contacts capture template finalization function
;; that automatically inserts birthday timestamps.

;;; Code:

;; Initialize package system for batch mode
(when noninteractive
  (package-initialize))

(require 'ert)
(require 'org)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar contacts-file "/tmp/test-contacts.org"
  "Stub contacts file for testing.")

;; Declare org-capture-plist for dynamic scoping in tests
(defvar org-capture-plist nil
  "Plist that org-capture uses during capture. Declared for testing.")

;; Load the actual module
(require 'org-contacts-config)

;;; Tests for birthday timestamp finalization

(ert-deftest test-contacts-capture-finalize-with-full-birthday ()
  "Test that finalize adds timestamp for YYYY-MM-DD birthday."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice Anderson\n")
    (insert ":PROPERTIES:\n")
    (insert ":EMAIL: alice@example.com\n")
    (insert ":BIRTHDAY: 1985-03-15\n")
    (insert ":END:\n")
    (insert "Added: [2025-11-01 Fri 20:30]\n")

    ;; Simulate capture context
    (let ((org-capture-plist '(:key "C")))
      (cj/org-contacts-finalize-birthday-timestamp)

      (let ((content (buffer-string)))
        ;; Should have birthday timestamp
        (should (string-match-p "<1985-03-15 [A-Za-z]\\{3\\} \\+1y>" content))
        ;; Timestamp should be after :END:
        (should (string-match-p ":END:\n<1985-03-15" content))))))

(ert-deftest test-contacts-capture-finalize-with-partial-birthday ()
  "Test that finalize adds timestamp for MM-DD birthday with current year."
  (let ((current-year (nth 5 (decode-time))))
    (with-temp-buffer
      (org-mode)
      (insert "* Bob Baker\n")
      (insert ":PROPERTIES:\n")
      (insert ":BIRTHDAY: 07-04\n")
      (insert ":END:\n")

      (let ((org-capture-plist '(:key "C")))
        (cj/org-contacts-finalize-birthday-timestamp)

        (let ((content (buffer-string)))
          ;; Should have birthday timestamp with current year
          (should (string-match-p (format "<%d-07-04 [A-Za-z]\\{3\\} \\+1y>" current-year) content)))))))

(ert-deftest test-contacts-capture-finalize-without-birthday ()
  "Test that finalize does nothing when no birthday property."
  (with-temp-buffer
    (org-mode)
    (insert "* Carol Chen\n")
    (insert ":PROPERTIES:\n")
    (insert ":EMAIL: carol@example.com\n")
    (insert ":END:\n")

    (let ((original-content (buffer-string))
          (org-capture-plist '(:key "C")))
      (cj/org-contacts-finalize-birthday-timestamp)

      ;; Content should be unchanged
      (should (string= (buffer-string) original-content))
      ;; Should have no timestamp
      (should-not (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (buffer-string))))))

(ert-deftest test-contacts-capture-finalize-with-empty-birthday ()
  "Test that finalize skips empty birthday values."
  (with-temp-buffer
    (org-mode)
    (insert "* David Davis\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: \n")
    (insert ":END:\n")

    (let ((original-content (buffer-string))
          (org-capture-plist '(:key "C")))
      (cj/org-contacts-finalize-birthday-timestamp)

      ;; Content should be unchanged
      (should (string= (buffer-string) original-content))
      ;; Should have no timestamp
      (should-not (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (buffer-string))))))

(ert-deftest test-contacts-capture-finalize-prevents-duplicates ()
  "Test that finalize doesn't add duplicate timestamps."
  (with-temp-buffer
    (org-mode)
    (insert "* Eve Evans\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert ":END:\n")
    (insert "<2000-01-01 Sat +1y>\n")

    (let ((org-capture-plist '(:key "C")))
      (cj/org-contacts-finalize-birthday-timestamp)

      ;; Should have exactly one timestamp
      (should (= 1 (how-many "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" (point-min) (point-max)))))))

(ert-deftest test-contacts-capture-finalize-only-for-contact-template ()
  "Test that finalize only runs for 'C' template key."
  (with-temp-buffer
    (org-mode)
    (insert "* Task with birthday property\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert ":END:\n")

    (let ((original-content (buffer-string))
          (org-capture-plist '(:key "t"))) ; Different template key
      (cj/org-contacts-finalize-birthday-timestamp)

      ;; Content should be unchanged
      (should (string= (buffer-string) original-content)))))

(ert-deftest test-contacts-capture-finalize-preserves-existing-content ()
  "Test that finalize preserves all existing content."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice Anderson\n")
    (insert ":PROPERTIES:\n")
    (insert ":EMAIL: alice@example.com\n")
    (insert ":PHONE: 555-1234\n")
    (insert ":BIRTHDAY: 1985-03-15\n")
    (insert ":NICKNAME: Ali\n")
    (insert ":NOTE: Met at conference\n")
    (insert ":END:\n")
    (insert "Added: [2025-11-01 Fri 20:30]\n")

    (let ((org-capture-plist '(:key "C")))
      (cj/org-contacts-finalize-birthday-timestamp)

      (let ((content (buffer-string)))
        ;; All properties should still be present
        (should (string-search ":EMAIL: alice@example.com" content))
        (should (string-search ":PHONE: 555-1234" content))
        (should (string-search ":BIRTHDAY: 1985-03-15" content))
        (should (string-search ":NICKNAME: Ali" content))
        (should (string-search ":NOTE: Met at conference" content))
        ;; Added timestamp should still be there
        (should (string-search "Added: [2025-11-01 Fri 20:30]" content))
        ;; Birthday timestamp should be added
        (should (string-match-p "<1985-03-15 [A-Za-z]\\{3\\} \\+1y>" content))))))

(provide 'test-org-contacts-capture-finalize)
;;; test-org-contacts-capture-finalize.el ends here
