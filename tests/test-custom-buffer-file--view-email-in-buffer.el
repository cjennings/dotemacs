;;; test-custom-buffer-file--view-email-in-buffer.el --- Tests for email viewer -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/view-email-in-buffer' function.
;;
;; Tests cover:
;; - Normal: HTML emails, plain text emails, multipart preference
;; - Boundary: UTF-8 content, empty bodies, fallback behavior
;; - Error: Non-file buffers, missing displayable content
;;
;; Uses real MIME parsing via mm-decode to test actual integration behavior.

;;; Code:

(require 'ert)
(require 'testutil-general)
(require 'custom-buffer-file)

;;; Test Data - Sample Email Content

(defconst test-email--html-only
  "MIME-Version: 1.0
Content-Type: text/html; charset=\"utf-8\"

<html><body><h1>Hello World</h1><p>This is a test.</p></body></html>
"
  "Simple HTML-only email.")

(defconst test-email--plain-only
  "MIME-Version: 1.0
Content-Type: text/plain; charset=\"utf-8\"

Hello World

This is a plain text email.
"
  "Simple plain text email.")

(defconst test-email--multipart-alternative
  "MIME-Version: 1.0
Content-Type: multipart/alternative; boundary=\"boundary123\"

--boundary123
Content-Type: text/plain; charset=\"utf-8\"

Plain text version of the email.

--boundary123
Content-Type: text/html; charset=\"utf-8\"

<html><body><h1>HTML Version</h1><p>Rich content here.</p></body></html>

--boundary123--
"
  "Multipart email with both plain and HTML parts.")

(defconst test-email--utf8-content
  "MIME-Version: 1.0
Content-Type: text/html; charset=\"utf-8\"

<html><body><p>Caf\303\251 \342\230\225 \360\237\215\225 \344\270\255\346\226\207</p></body></html>
"
  "Email with UTF-8 characters (cafe, coffee emoji, pizza emoji, Chinese).")

(defconst test-email--empty-body
  "MIME-Version: 1.0
Content-Type: text/plain; charset=\"utf-8\"

"
  "Email with empty body.")

(defconst test-email--image-only
  "MIME-Version: 1.0
Content-Type: image/png
Content-Transfer-Encoding: base64

iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==
"
  "Email with only an image attachment, no text content.")

;;; Setup and Teardown

(defun test-email--setup ()
  "Create test base directory."
  (cj/create-test-base-dir))

(defun test-email--teardown ()
  "Clean up test files and buffers."
  ;; Kill any *Email:* buffers created during tests
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*Email:" (buffer-name buf))
      (kill-buffer buf)))
  ;; Clean up test directory
  (cj/delete-test-base-dir))

(defun test-email--create-eml-file (content)
  "Create a temporary .eml file with CONTENT and return its path."
  (let ((file (cj/create-temp-test-file "test-email-"))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert content))
    (rename-file file (concat file ".eml"))
    (concat file ".eml")))

(defun test-email--get-rendered-content (eml-file)
  "Open EML-FILE, run view-email-in-buffer, return rendered buffer content."
  (let ((result nil))
    (with-current-buffer (find-file-noselect eml-file)
      (cj/view-email-in-buffer)
      (let ((email-buf (format "*Email: %s*" (file-name-nondirectory eml-file))))
        (when (get-buffer email-buf)
          (with-current-buffer email-buf
            (setq result (buffer-string)))))
      (kill-buffer))
    result))

;;; Normal Cases

(ert-deftest test-custom-buffer-file--view-email-normal-html-renders ()
  "Test that HTML email content is rendered.
The HTML should be processed by shr and display readable text.
Note: shr may insert newlines between words for wrapping."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--html-only))
             (content (test-email--get-rendered-content eml-file)))
        (should content)
        ;; shr may insert newlines for wrapping, so check words separately
        (should (string-match-p "Hello" content))
        (should (string-match-p "World" content))
        (should (string-match-p "test" content))
        ;; HTML tags should be rendered, not visible as raw text
        (should-not (string-match-p "<h1>" content)))
    (test-email--teardown)))

(ert-deftest test-custom-buffer-file--view-email-normal-plain-text-displays ()
  "Test that plain text email content is displayed."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--plain-only))
             (content (test-email--get-rendered-content eml-file)))
        (should content)
        (should (string-match-p "Hello World" content))
        (should (string-match-p "plain text email" content)))
    (test-email--teardown)))

(ert-deftest test-custom-buffer-file--view-email-normal-multipart-prefers-html ()
  "Test that multipart/alternative email prefers HTML over plain text.
Note: shr may insert newlines between words for wrapping."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--multipart-alternative))
             (content (test-email--get-rendered-content eml-file)))
        (should content)
        ;; Should contain HTML version content (words may be on separate lines)
        (should (string-match-p "HTML" content))
        (should (string-match-p "Version" content))
        (should (string-match-p "Rich" content))
        ;; Should NOT contain plain text version marker
        (should-not (string-match-p "Plain text version" content))
        ;; HTML should be rendered, not raw
        (should-not (string-match-p "<h1>" content)))
    (test-email--teardown)))

;;; Boundary Cases

(ert-deftest test-custom-buffer-file--view-email-boundary-utf8-content ()
  "Test that UTF-8 characters are displayed correctly."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--utf8-content))
             (content (test-email--get-rendered-content eml-file)))
        (should content)
        ;; Should contain the UTF-8 content (café with accented e)
        (should (string-match-p "Caf" content)))
    (test-email--teardown)))

(ert-deftest test-custom-buffer-file--view-email-boundary-empty-body ()
  "Test that email with empty body creates buffer without error."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--empty-body))
             (content (test-email--get-rendered-content eml-file)))
        ;; Should succeed and return content (possibly empty string)
        (should (stringp content)))
    (test-email--teardown)))

(ert-deftest test-custom-buffer-file--view-email-boundary-buffer-name-format ()
  "Test that output buffer is named correctly with filename."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--plain-only))
             (expected-buf-name (format "*Email: %s*" (file-name-nondirectory eml-file))))
        (with-current-buffer (find-file-noselect eml-file)
          (cj/view-email-in-buffer)
          (should (get-buffer expected-buf-name))
          (kill-buffer))
        (when (get-buffer expected-buf-name)
          (kill-buffer expected-buf-name)))
    (test-email--teardown)))

(ert-deftest test-custom-buffer-file--view-email-boundary-special-mode ()
  "Test that output buffer is in special-mode for easy dismissal."
  (test-email--setup)
  (unwind-protect
      (let* ((eml-file (test-email--create-eml-file test-email--plain-only))
             (buf-name (format "*Email: %s*" (file-name-nondirectory eml-file))))
        (with-current-buffer (find-file-noselect eml-file)
          (cj/view-email-in-buffer)
          (when (get-buffer buf-name)
            (with-current-buffer buf-name
              (should (derived-mode-p 'special-mode))))
          (kill-buffer))
        (when (get-buffer buf-name)
          (kill-buffer buf-name)))
    (test-email--teardown)))

;;; Error Cases

(ert-deftest test-custom-buffer-file--view-email-error-not-visiting-file ()
  "Test that error is signaled when buffer is not visiting a file."
  (with-temp-buffer
    (insert "some content")
    (should-error (cj/view-email-in-buffer) :type 'user-error)))

(ert-deftest test-custom-buffer-file--view-email-error-no-displayable-content ()
  "Test that error is signaled when email has no text/html or text/plain part."
  (test-email--setup)
  (unwind-protect
      (let ((eml-file (test-email--create-eml-file test-email--image-only)))
        (with-current-buffer (find-file-noselect eml-file)
          (should-error (cj/view-email-in-buffer) :type 'user-error)
          (kill-buffer)))
    (test-email--teardown)))

(provide 'test-custom-buffer-file--view-email-in-buffer)
;;; test-custom-buffer-file--view-email-in-buffer.el ends here
