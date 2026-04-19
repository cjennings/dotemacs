;;; test-transcription-api-key.el --- Tests for API-key retrieval -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for `cj/--get-openai-api-key' and
;; `cj/--get-assemblyai-api-key'.  Pin down current auth-source-search
;; behavior so the upcoming consolidation into a single parameterized
;; helper can be verified as a pure refactor.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap))

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args) nil))

(require 'transcription-config)

;; Captures the most recent :host passed to auth-source-search so each
;; test can assert that the right host was queried.
(defvar test-api-key-requested-host nil)

(defmacro test-api-key-with-auth-source (secret-value &rest body)
  "Run BODY with `auth-source-search' stubbed to return SECRET-VALUE.
SECRET-VALUE is either a string, a function returning a string, or nil
\(to simulate no matching entry).  The host passed to `auth-source-search'
is captured in `test-api-key-requested-host'."
  (declare (indent 1))
  `(let ((test-api-key-requested-host nil))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest args)
                  (setq test-api-key-requested-host (plist-get args :host))
                  (when ,secret-value
                    (list (list :secret ,secret-value))))))
       ,@body)))

;;; Normal Cases — OpenAI

(ert-deftest test-api-key-openai-normal-returns-string-secret ()
  "OpenAI key retrieval returns the string stored under :secret."
  (test-api-key-with-auth-source "sk-abc123"
    (should (equal "sk-abc123" (cj/--get-openai-api-key)))
    (should (equal "api.openai.com" test-api-key-requested-host))))

(ert-deftest test-api-key-openai-normal-calls-function-secret ()
  "OpenAI key retrieval invokes :secret when it's a function."
  (test-api-key-with-auth-source (lambda () "sk-from-fn")
    (should (equal "sk-from-fn" (cj/--get-openai-api-key)))))

;;; Normal Cases — AssemblyAI

(ert-deftest test-api-key-assemblyai-normal-returns-string-secret ()
  "AssemblyAI key retrieval returns the string stored under :secret."
  (test-api-key-with-auth-source "aai-xyz789"
    (should (equal "aai-xyz789" (cj/--get-assemblyai-api-key)))
    (should (equal "api.assemblyai.com" test-api-key-requested-host))))

(ert-deftest test-api-key-assemblyai-normal-calls-function-secret ()
  "AssemblyAI key retrieval invokes :secret when it's a function."
  (test-api-key-with-auth-source (lambda () "aai-from-fn")
    (should (equal "aai-from-fn" (cj/--get-assemblyai-api-key)))))

;;; Boundary Cases

(ert-deftest test-api-key-openai-boundary-returns-nil-when-entry-missing ()
  "OpenAI key retrieval returns nil when auth-source has no matching entry."
  (test-api-key-with-auth-source nil
    (should (null (cj/--get-openai-api-key)))))

(ert-deftest test-api-key-assemblyai-boundary-returns-nil-when-entry-missing ()
  "AssemblyAI key retrieval returns nil when auth-source has no matching entry."
  (test-api-key-with-auth-source nil
    (should (null (cj/--get-assemblyai-api-key)))))

(ert-deftest test-api-key-openai-boundary-queries-correct-host ()
  "OpenAI key retrieval always queries `api.openai.com', never another host."
  (test-api-key-with-auth-source "sk-x"
    (cj/--get-openai-api-key)
    (should (equal "api.openai.com" test-api-key-requested-host))))

(ert-deftest test-api-key-assemblyai-boundary-queries-correct-host ()
  "AssemblyAI key retrieval always queries `api.assemblyai.com'."
  (test-api-key-with-auth-source "aai-x"
    (cj/--get-assemblyai-api-key)
    (should (equal "api.assemblyai.com" test-api-key-requested-host))))

(provide 'test-transcription-api-key)
;;; test-transcription-api-key.el ends here
