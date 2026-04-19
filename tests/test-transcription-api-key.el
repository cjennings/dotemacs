;;; test-transcription-api-key.el --- Tests for auth-source password helper -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--auth-source-password', the parameterized helper that
;; replaced the per-backend `cj/--get-openai-api-key' and
;; `cj/--get-assemblyai-api-key' functions.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap))

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args) nil))

(require 'transcription-config)

(defvar test-api-key-requested-host nil
  "Captures the :host passed to the mocked `auth-source-search'.")

(defmacro test-api-key-with-auth-source (secret-value &rest body)
  "Run BODY with `auth-source-search' stubbed to return SECRET-VALUE.
SECRET-VALUE is a string, a function returning a string, or nil (to
simulate no matching entry).  The host passed to `auth-source-search'
is captured in `test-api-key-requested-host'."
  (declare (indent 1))
  `(let ((test-api-key-requested-host nil))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest args)
                  (setq test-api-key-requested-host (plist-get args :host))
                  (when ,secret-value
                    (list (list :secret ,secret-value))))))
       ,@body)))

;;; Normal Cases

(ert-deftest test-api-key-normal-returns-string-secret ()
  "Returns the :secret value when stored as a string."
  (test-api-key-with-auth-source "sk-abc123"
    (should (equal "sk-abc123" (cj/--auth-source-password "api.openai.com")))))

(ert-deftest test-api-key-normal-calls-function-secret ()
  "Invokes the :secret value when stored as a function."
  (test-api-key-with-auth-source (lambda () "aai-from-fn")
    (should (equal "aai-from-fn" (cj/--auth-source-password "api.assemblyai.com")))))

(ert-deftest test-api-key-normal-passes-host-through ()
  "The caller-supplied HOST is forwarded to `auth-source-search'."
  (test-api-key-with-auth-source "x"
    (cj/--auth-source-password "api.openai.com")
    (should (equal "api.openai.com" test-api-key-requested-host)))
  (test-api-key-with-auth-source "x"
    (cj/--auth-source-password "api.assemblyai.com")
    (should (equal "api.assemblyai.com" test-api-key-requested-host)))
  (test-api-key-with-auth-source "x"
    (cj/--auth-source-password "example.com")
    (should (equal "example.com" test-api-key-requested-host))))

;;; Boundary Cases

(ert-deftest test-api-key-boundary-returns-nil-when-entry-missing ()
  "Returns nil when `auth-source-search' finds no matching entry."
  (test-api-key-with-auth-source nil
    (should (null (cj/--auth-source-password "api.openai.com")))
    (should (null (cj/--auth-source-password "api.assemblyai.com")))))

(ert-deftest test-api-key-boundary-empty-string-host ()
  "Empty host string is forwarded unchanged (auth-source decides)."
  (test-api-key-with-auth-source "x"
    (cj/--auth-source-password "")
    (should (equal "" test-api-key-requested-host))))

;;; Backend Descriptor Integration

(ert-deftest test-api-key-integration-openai-backend-descriptor ()
  "openai-api backend descriptor has the expected auth-host and env-var."
  (let ((desc (cj/--backend-plist 'openai-api)))
    (should (equal "api.openai.com" (plist-get desc :auth-host)))
    (should (equal "OPENAI_API_KEY"  (plist-get desc :env-var)))
    (should (equal "oai-transcribe"  (plist-get desc :script)))))

(ert-deftest test-api-key-integration-assemblyai-backend-descriptor ()
  "assemblyai backend descriptor has the expected auth-host and env-var."
  (let ((desc (cj/--backend-plist 'assemblyai)))
    (should (equal "api.assemblyai.com"     (plist-get desc :auth-host)))
    (should (equal "ASSEMBLYAI_API_KEY"     (plist-get desc :env-var)))
    (should (equal "assemblyai-transcribe"  (plist-get desc :script)))))

(ert-deftest test-api-key-integration-local-whisper-needs-no-key ()
  "local-whisper descriptor explicitly has nil auth-host and env-var."
  (let ((desc (cj/--backend-plist 'local-whisper)))
    (should (null (plist-get desc :auth-host)))
    (should (null (plist-get desc :env-var)))
    (should (equal "local-whisper" (plist-get desc :script)))))

(ert-deftest test-api-key-integration-unknown-backend-errors ()
  "Requesting an unknown backend's descriptor signals user-error."
  (should-error (cj/--backend-plist 'nonexistent-backend) :type 'user-error))

(provide 'test-transcription-api-key)
;;; test-transcription-api-key.el ends here
