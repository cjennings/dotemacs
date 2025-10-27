;;; test-org-contacts-parse-email.el --- Tests for cj/--parse-email-string -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--parse-email-string function from org-contacts-config.el
;;
;; This function parses a string containing one or more email addresses
;; separated by commas, semicolons, or spaces, and formats them as
;; "Name <email>" strings.
;;
;; Examples:
;; Input:  name="John Doe", email-string="john@example.com"
;; Output: '("John Doe <john@example.com>")
;;
;; Input:  name="Jane Smith", email-string="jane@work.com, jane@home.com"
;; Output: '("Jane Smith <jane@work.com>" "Jane Smith <jane@home.com>")

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Now load the actual production module
(require 'org-contacts-config)

;;; Test Helpers

(defun test-parse-email (name email-string)
  "Test cj/--parse-email-string with NAME and EMAIL-STRING.
Returns the formatted email list."
  (cj/--parse-email-string name email-string))

;;; Normal Cases - Single Email

(ert-deftest test-parse-single-email ()
  "Should format single email address."
  (let ((result (test-parse-email "John Doe" "john@example.com")))
    (should (equal result '("John Doe <john@example.com>")))))

(ert-deftest test-parse-single-email-with-subdomain ()
  "Should handle email with subdomain."
  (let ((result (test-parse-email "Jane Smith" "jane@mail.company.com")))
    (should (equal result '("Jane Smith <jane@mail.company.com>")))))

(ert-deftest test-parse-email-with-numbers ()
  "Should handle email containing numbers."
  (let ((result (test-parse-email "User 123" "user123@test.com")))
    (should (equal result '("User 123 <user123@test.com>")))))

(ert-deftest test-parse-email-with-dots ()
  "Should handle email with dots in local part."
  (let ((result (test-parse-email "Bob Jones" "bob.jones@example.com")))
    (should (equal result '("Bob Jones <bob.jones@example.com>")))))

(ert-deftest test-parse-email-with-hyphen ()
  "Should handle email with hyphens."
  (let ((result (test-parse-email "Alice Brown" "alice-brown@test-domain.com")))
    (should (equal result '("Alice Brown <alice-brown@test-domain.com>")))))

;;; Normal Cases - Multiple Emails with Different Separators

(ert-deftest test-parse-two-emails-comma ()
  "Should parse two emails separated by comma."
  (let ((result (test-parse-email "John Doe" "john@work.com, john@home.com")))
    (should (equal result '("John Doe <john@work.com>" "John Doe <john@home.com>")))))

(ert-deftest test-parse-two-emails-semicolon ()
  "Should parse two emails separated by semicolon."
  (let ((result (test-parse-email "Jane Smith" "jane@work.com; jane@home.com")))
    (should (equal result '("Jane Smith <jane@work.com>" "Jane Smith <jane@home.com>")))))

(ert-deftest test-parse-two-emails-space ()
  "Should parse two emails separated by space."
  (let ((result (test-parse-email "Bob Jones" "bob@work.com bob@home.com")))
    (should (equal result '("Bob Jones <bob@work.com>" "Bob Jones <bob@home.com>")))))

(ert-deftest test-parse-three-emails-mixed-separators ()
  "Should parse emails with mixed separators."
  (let ((result (test-parse-email "Alice" "alice@a.com, alice@b.com; alice@c.com")))
    (should (equal result '("Alice <alice@a.com>" "Alice <alice@b.com>" "Alice <alice@c.com>")))))

(ert-deftest test-parse-multiple-emails-with-spaces ()
  "Should parse comma-separated emails with spaces."
  (let ((result (test-parse-email "User" "a@test.com , b@test.com , c@test.com")))
    (should (equal result '("User <a@test.com>" "User <b@test.com>" "User <c@test.com>")))))

;;; Normal Cases - Whitespace Handling

(ert-deftest test-parse-email-leading-whitespace ()
  "Should trim leading whitespace from email."
  (let ((result (test-parse-email "John" "  john@example.com")))
    (should (equal result '("John <john@example.com>")))))

(ert-deftest test-parse-email-trailing-whitespace ()
  "Should trim trailing whitespace from email."
  (let ((result (test-parse-email "Jane" "jane@example.com  ")))
    (should (equal result '("Jane <jane@example.com>")))))

(ert-deftest test-parse-email-surrounding-whitespace ()
  "Should trim surrounding whitespace from email."
  (let ((result (test-parse-email "Bob" "  bob@example.com  ")))
    (should (equal result '("Bob <bob@example.com>")))))

(ert-deftest test-parse-emails-with-tabs ()
  "Should handle emails separated by tabs."
  (let ((result (test-parse-email "User" "a@test.com\tb@test.com")))
    (should (equal result '("User <a@test.com>" "User <b@test.com>")))))

;;; Edge Cases - Empty and Nil

(ert-deftest test-parse-nil-email-string ()
  "Should return nil for nil email string."
  (let ((result (test-parse-email "John Doe" nil)))
    (should (null result))))

(ert-deftest test-parse-empty-email-string ()
  "Should return nil for empty email string."
  (let ((result (test-parse-email "Jane Smith" "")))
    (should (null result))))

(ert-deftest test-parse-whitespace-only ()
  "Should return nil for whitespace-only string."
  (let ((result (test-parse-email "Bob Jones" "   ")))
    (should (null result))))

(ert-deftest test-parse-tabs-only ()
  "Should return nil for tabs-only string."
  (let ((result (test-parse-email "Alice" "\t\t\t")))
    (should (null result))))

(ert-deftest test-parse-mixed-whitespace-only ()
  "Should return nil for mixed whitespace."
  (let ((result (test-parse-email "User" " \t \n ")))
    (should (null result))))

;;; Edge Cases - Multiple Consecutive Separators

(ert-deftest test-parse-multiple-commas ()
  "Should handle multiple consecutive commas."
  (let ((result (test-parse-email "John" "john@a.com,,,john@b.com")))
    (should (equal result '("John <john@a.com>" "John <john@b.com>")))))

(ert-deftest test-parse-multiple-semicolons ()
  "Should handle multiple consecutive semicolons."
  (let ((result (test-parse-email "Jane" "jane@a.com;;;jane@b.com")))
    (should (equal result '("Jane <jane@a.com>" "Jane <jane@b.com>")))))

(ert-deftest test-parse-multiple-spaces ()
  "Should handle multiple consecutive spaces."
  (let ((result (test-parse-email "Bob" "bob@a.com    bob@b.com")))
    (should (equal result '("Bob <bob@a.com>" "Bob <bob@b.com>")))))

(ert-deftest test-parse-mixed-multiple-separators ()
  "Should handle mixed consecutive separators."
  (let ((result (test-parse-email "User" "a@test.com , ; b@test.com")))
    (should (equal result '("User <a@test.com>" "User <b@test.com>")))))

;;; Edge Cases - Special Name Formats

(ert-deftest test-parse-name-with-title ()
  "Should handle name with title."
  (let ((result (test-parse-email "Dr. John Smith" "john@example.com")))
    (should (equal result '("Dr. John Smith <john@example.com>")))))

(ert-deftest test-parse-name-with-suffix ()
  "Should handle name with suffix."
  (let ((result (test-parse-email "John Doe Jr." "john@example.com")))
    (should (equal result '("John Doe Jr. <john@example.com>")))))

(ert-deftest test-parse-name-with-special-chars ()
  "Should handle name with special characters."
  (let ((result (test-parse-email "O'Brien, Patrick" "patrick@example.com")))
    (should (equal result '("O'Brien, Patrick <patrick@example.com>")))))

(ert-deftest test-parse-unicode-name ()
  "Should handle Unicode characters in name."
  (let ((result (test-parse-email "José García" "jose@example.com")))
    (should (equal result '("José García <jose@example.com>")))))

;;; Edge Cases - Special Email Formats

(ert-deftest test-parse-email-with-plus ()
  "Should handle email with plus sign."
  (let ((result (test-parse-email "User" "user+tag@example.com")))
    (should (equal result '("User <user+tag@example.com>")))))

(ert-deftest test-parse-email-with-underscore ()
  "Should handle email with underscore."
  (let ((result (test-parse-email "User" "user_name@example.com")))
    (should (equal result '("User <user_name@example.com>")))))

(ert-deftest test-parse-very-long-email ()
  "Should handle very long email address."
  (let* ((long-local (make-string 50 ?a))
         (email (concat long-local "@example.com"))
         (result (test-parse-email "User" email)))
    (should (equal result (list (format "User <%s>" email))))))

;;; Integration Tests

(ert-deftest test-parse-realistic-contact ()
  "Should parse realistic contact with multiple emails."
  (let ((result (test-parse-email "John Doe" "john.doe@company.com, jdoe@personal.com")))
    (should (equal result '("John Doe <john.doe@company.com>" "John Doe <jdoe@personal.com>")))))

(ert-deftest test-parse-messy-input ()
  "Should handle messy real-world input."
  (let ((result (test-parse-email "Jane Smith" "  jane@work.com ;  jane@home.com,jane@mobile.com  ")))
    (should (equal result '("Jane Smith <jane@work.com>" "Jane Smith <jane@home.com>" "Jane Smith <jane@mobile.com>")))))

(ert-deftest test-parse-single-with-extra-separators ()
  "Should handle single email with trailing separators."
  (let ((result (test-parse-email "Bob" "bob@example.com;;;")))
    (should (equal result '("Bob <bob@example.com>")))))

(provide 'test-org-contacts-parse-email)
;;; test-org-contacts-parse-email.el ends here
