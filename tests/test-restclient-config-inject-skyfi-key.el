;;; test-restclient-config-inject-skyfi-key.el --- Tests for cj/restclient--inject-skyfi-key -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/restclient--inject-skyfi-key function.
;; Replaces the :skyfi-key placeholder in a restclient buffer with the real
;; API key from authinfo. Tests cover Normal, Boundary, and Error cases.

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'restclient-config)

;; ---------------------------------------------------------------------------
;; Test Helpers
;; ---------------------------------------------------------------------------

(defvar test-inject--fake-key "sk_test_fake_key_12345"
  "Fake API key used in tests.")

(defmacro test-inject--with-skyfi-buffer (content &rest body)
  "Create a temp buffer with CONTENT simulating skyfi-api.rest, then run BODY.
Sets buffer-file-name to skyfi-api.rest and activates restclient-mode.
Binds `cj/skyfi-api-key' to return `test-inject--fake-key'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (setq buffer-file-name (expand-file-name "data/skyfi-api.rest" user-emacs-directory))
     (restclient-mode)
     (cl-letf (((symbol-function 'cj/skyfi-api-key)
                (lambda () test-inject--fake-key)))
       ,@body)))

;; ---------------------------------------------------------------------------
;;; Normal Cases
;; ---------------------------------------------------------------------------

(ert-deftest test-inject-skyfi-key-replaces-placeholder ()
  "Replaces :skyfi-key = PLACEHOLDER with real key."
  (test-inject--with-skyfi-buffer
      ":skyfi-key = PLACEHOLDER\n#\nGET https://example.com\n"
    (cj/restclient--inject-skyfi-key)
    (goto-char (point-min))
    (should (string-match-p (format ":skyfi-key = %s" test-inject--fake-key)
                            (buffer-string)))))

(ert-deftest test-inject-skyfi-key-preserves-other-content ()
  "Rest of buffer content unchanged after injection."
  (let ((rest-content "# SkyFi API\nGET https://app.skyfi.com/platform-api/\nAPIKey: :skyfi-key\n"))
    (test-inject--with-skyfi-buffer
        (concat ":skyfi-key = PLACEHOLDER\n" rest-content)
      (cj/restclient--inject-skyfi-key)
      (should (string-match-p "# SkyFi API" (buffer-string)))
      (should (string-match-p "APIKey: :skyfi-key" (buffer-string))))))

(ert-deftest test-inject-skyfi-key-only-replaces-skyfi-key-line ()
  "Does not modify other restclient variable lines."
  (test-inject--with-skyfi-buffer
      ":skyfi-key = PLACEHOLDER\n:other-var = keep-me\n"
    (cj/restclient--inject-skyfi-key)
    (should (string-match-p ":other-var = keep-me" (buffer-string)))))

;; ---------------------------------------------------------------------------
;;; Boundary Cases
;; ---------------------------------------------------------------------------

(ert-deftest test-inject-skyfi-key-no-key-line-no-error ()
  "Buffer with no :skyfi-key line — no change, no error."
  (test-inject--with-skyfi-buffer
      "# Just comments\nGET https://example.com\n"
    (let ((before (buffer-string)))
      (cj/restclient--inject-skyfi-key)
      (should (string= before (buffer-string))))))

(ert-deftest test-inject-skyfi-key-already-has-value ()
  "Buffer where :skyfi-key already has a real value — still replaces (idempotent)."
  (test-inject--with-skyfi-buffer
      ":skyfi-key = old_real_key_abc\n"
    (cj/restclient--inject-skyfi-key)
    (should (string-match-p (format ":skyfi-key = %s" test-inject--fake-key)
                            (buffer-string)))))

(ert-deftest test-inject-skyfi-key-empty-buffer ()
  "Empty buffer — no error."
  (test-inject--with-skyfi-buffer ""
    (cj/restclient--inject-skyfi-key)
    (should (string= "" (buffer-string)))))

(ert-deftest test-inject-skyfi-key-only-first-occurrence ()
  "Multiple :skyfi-key lines — only first replaced."
  (test-inject--with-skyfi-buffer
      ":skyfi-key = PLACEHOLDER\n:skyfi-key = SECOND\n"
    (cj/restclient--inject-skyfi-key)
    (let ((content (buffer-string)))
      (should (string-match-p (format ":skyfi-key = %s" test-inject--fake-key) content))
      (should (string-match-p ":skyfi-key = SECOND" content)))))

;; ---------------------------------------------------------------------------
;;; Error Cases
;; ---------------------------------------------------------------------------

(ert-deftest test-inject-skyfi-key-wrong-mode-no-replacement ()
  "Wrong major mode — no replacement happens."
  (with-temp-buffer
    (insert ":skyfi-key = PLACEHOLDER\n")
    (setq buffer-file-name (expand-file-name "data/skyfi-api.rest" user-emacs-directory))
    (fundamental-mode)
    (let ((before (buffer-string)))
      (cj/restclient--inject-skyfi-key)
      (should (string= before (buffer-string))))))

(ert-deftest test-inject-skyfi-key-wrong-filename-no-replacement ()
  "Wrong filename — no replacement happens."
  (with-temp-buffer
    (insert ":skyfi-key = PLACEHOLDER\n")
    (setq buffer-file-name "/tmp/other-file.rest")
    (restclient-mode)
    (let ((before (buffer-string)))
      (cj/restclient--inject-skyfi-key)
      (should (string= before (buffer-string))))))

(ert-deftest test-inject-skyfi-key-no-filename-no-replacement ()
  "No filename (scratch buffer) — no replacement happens."
  (with-temp-buffer
    (insert ":skyfi-key = PLACEHOLDER\n")
    (restclient-mode)
    (setq buffer-file-name nil)
    (let ((before (buffer-string)))
      (cj/restclient--inject-skyfi-key)
      (should (string= before (buffer-string))))))

(ert-deftest test-inject-skyfi-key-auth-returns-nil-no-error ()
  "Auth-source returns nil — no error, no replacement."
  (with-temp-buffer
    (insert ":skyfi-key = PLACEHOLDER\n")
    (setq buffer-file-name (expand-file-name "data/skyfi-api.rest" user-emacs-directory))
    (restclient-mode)
    (cl-letf (((symbol-function 'cj/skyfi-api-key)
               (lambda () nil)))
      (let ((before (buffer-string)))
        (cj/restclient--inject-skyfi-key)
        (should (string= before (buffer-string)))))))

(provide 'test-restclient-config-inject-skyfi-key)
;;; test-restclient-config-inject-skyfi-key.el ends here
