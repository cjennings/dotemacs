;;; test-gptel-tools-web-fetch.el --- Tests for web_fetch gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Validators and helpers tested directly.  The orchestrator's network
;; call is stubbed via `cl-letf' on `url-retrieve-synchronously' / the
;; module's `--retrieve' helper; HTML stripping runs against real
;; pandoc / w3m (both are installed in this dev environment, and
;; verifying they don't mangle inputs is the point).

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "gptel-tools" user-emacs-directory))
  (setq load-prefer-newer t)
  (unless (featurep 'gptel)
    (defvar gptel-tools nil)
    (defun gptel-make-tool (&rest _args) nil)
    (defun gptel-get-tool (&rest _args) nil)
    (provide 'gptel)))

(require 'web_fetch)

;; ---------- validate-url

(ert-deftest test-gptel-tools-web-fetch-validate-url-http ()
  "Normal: http URL passes."
  (should (equal (cj/gptel-web-fetch--validate-url "http://example.com")
                 "http://example.com")))

(ert-deftest test-gptel-tools-web-fetch-validate-url-https ()
  "Normal: https URL passes."
  (should (equal (cj/gptel-web-fetch--validate-url "https://example.com/path")
                 "https://example.com/path")))

(ert-deftest test-gptel-tools-web-fetch-validate-url-error-non-string ()
  "Error: non-string URL signals."
  (should-error (cj/gptel-web-fetch--validate-url nil))
  (should-error (cj/gptel-web-fetch--validate-url 42)))

(ert-deftest test-gptel-tools-web-fetch-validate-url-error-empty ()
  "Error: empty URL signals."
  (should-error (cj/gptel-web-fetch--validate-url "")))

(ert-deftest test-gptel-tools-web-fetch-validate-url-error-non-http-scheme ()
  "Error: schemes other than http/https are rejected."
  (should-error (cj/gptel-web-fetch--validate-url "file:///etc/hostname"))
  (should-error (cj/gptel-web-fetch--validate-url "ftp://example.com"))
  (should-error (cj/gptel-web-fetch--validate-url "javascript:alert(1)"))
  (should-error (cj/gptel-web-fetch--validate-url "example.com"))) ; no scheme

;; ---------- effective-max-bytes

(ert-deftest test-gptel-tools-web-fetch-max-bytes-default-on-nil ()
  "Boundary: nil falls back to the default cap."
  (should (= (cj/gptel-web-fetch--effective-max-bytes nil)
             cj/gptel-web-fetch--default-max-bytes)))

(ert-deftest test-gptel-tools-web-fetch-max-bytes-clamp-low ()
  "Boundary: zero / negative fall back to the default."
  (should (= (cj/gptel-web-fetch--effective-max-bytes 0)
             cj/gptel-web-fetch--default-max-bytes))
  (should (= (cj/gptel-web-fetch--effective-max-bytes -1)
             cj/gptel-web-fetch--default-max-bytes)))

(ert-deftest test-gptel-tools-web-fetch-max-bytes-cap-high ()
  "Boundary: values above the hard cap are clamped."
  (should (= (cj/gptel-web-fetch--effective-max-bytes (* 10 1024 1024))
             cj/gptel-web-fetch--hard-max-bytes)))

(ert-deftest test-gptel-tools-web-fetch-max-bytes-normal ()
  "Normal: a sensible value passes through."
  (should (= (cj/gptel-web-fetch--effective-max-bytes 50000) 50000)))

;; ---------- truncate

(ert-deftest test-gptel-tools-web-fetch-truncate-under-cap ()
  "Normal: small input returns unchanged."
  (should (equal (cj/gptel-web-fetch--truncate "short" 1000) "short")))

(ert-deftest test-gptel-tools-web-fetch-truncate-at-cap ()
  "Boundary: input exactly at cap returns unchanged."
  (let ((s (make-string 10 ?x)))
    (should (equal (cj/gptel-web-fetch--truncate s 10) s))))

(ert-deftest test-gptel-tools-web-fetch-truncate-over-cap ()
  "Boundary: oversize input is truncated and marked."
  (let* ((s (make-string 1000 ?x))
         (out (cj/gptel-web-fetch--truncate s 100)))
    (should (string-match-p "\\[truncated:" out))
    (should (string-match-p "1000 bytes total" out))))

;; ---------- html-to-text

(ert-deftest test-gptel-tools-web-fetch-html-to-text-strips-tags ()
  "Normal: pandoc / w3m strip HTML tags from real markup."
  (let ((out (cj/gptel-web-fetch--html-to-text
              "<html><body><h1>Hello</h1><p>World</p></body></html>")))
    (should (string-match-p "Hello" out))
    (should (string-match-p "World" out))
    (should-not (string-match-p "<h1>" out))
    (should-not (string-match-p "<p>" out))))

(ert-deftest test-gptel-tools-web-fetch-html-to-text-error-when-neither-on-path ()
  "Error: when neither pandoc nor w3m is on PATH, signals user-error."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (cj/gptel-web-fetch--html-to-text "<p>x</p>"))))

;; ---------- run (orchestrator)

(ert-deftest test-gptel-tools-web-fetch-run-normal-strips-html ()
  "Normal: orchestrator returns stripped text by default."
  (cl-letf (((symbol-function 'cj/gptel-web-fetch--retrieve)
             (lambda (_url)
               (cons 200 "<html><body><p>fetched</p></body></html>"))))
    (let ((out (cj/gptel-web-fetch--run "https://example.com")))
      (should (string-match-p "fetched" out))
      (should-not (string-match-p "<p>" out)))))

(ert-deftest test-gptel-tools-web-fetch-run-raw-returns-body-verbatim ()
  "Normal: raw=t returns the response body without HTML stripping."
  (cl-letf (((symbol-function 'cj/gptel-web-fetch--retrieve)
             (lambda (_url)
               (cons 200 "<html><body><p>raw</p></body></html>"))))
    (let ((out (cj/gptel-web-fetch--run "https://example.com" t)))
      (should (string-match-p "<p>raw</p>" out)))))

(ert-deftest test-gptel-tools-web-fetch-run-error-on-4xx ()
  "Error: HTTP 4xx response signals."
  (cl-letf (((symbol-function 'cj/gptel-web-fetch--retrieve)
             (lambda (_url) (cons 404 "not found"))))
    (should-error (cj/gptel-web-fetch--run "https://example.com"))))

(ert-deftest test-gptel-tools-web-fetch-run-error-on-5xx ()
  "Error: HTTP 5xx response signals."
  (cl-letf (((symbol-function 'cj/gptel-web-fetch--retrieve)
             (lambda (_url) (cons 503 "service unavailable"))))
    (should-error (cj/gptel-web-fetch--run "https://example.com"))))

(ert-deftest test-gptel-tools-web-fetch-run-truncates-oversized-body ()
  "Boundary: an oversize body is truncated by the run wrapper."
  (let ((big (concat "<html><body>"
                     (make-string 1000 ?x)
                     "</body></html>")))
    (cl-letf (((symbol-function 'cj/gptel-web-fetch--retrieve)
               (lambda (_url) (cons 200 big))))
      (let ((out (cj/gptel-web-fetch--run "https://example.com" t 200)))
        (should (string-match-p "\\[truncated:" out))))))

(ert-deftest test-gptel-tools-web-fetch-run-error-on-bad-scheme ()
  "Error: non-http URL fails fast at the validator."
  (should-error (cj/gptel-web-fetch--run "file:///etc/passwd")))

(provide 'test-gptel-tools-web-fetch)
;;; test-gptel-tools-web-fetch.el ends here
