;;; test-transcription-process-environment.el --- Tests for env-var assembly -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--build-process-environment', which returns a
;; `process-environment'-compatible list augmented with the API-key env
;; var for the active backend (or unchanged for local backends).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap))

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args) nil))

(require 'transcription-config)

(defmacro test-env-with-api-key (key-value &rest body)
  "Run BODY with `cj/--auth-source-password' returning KEY-VALUE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'cj/--auth-source-password)
              (lambda (&rest _) ,key-value)))
     ,@body))

;;; Normal Cases

(ert-deftest test-build-env-normal-openai-prepends-key-var ()
  "openai-api backend prepends OPENAI_API_KEY=<key>."
  (test-env-with-api-key "sk-abc"
    (let ((process-environment '("PATH=/usr/bin")))
      (let ((result (cj/--build-process-environment 'openai-api)))
        (should (equal "OPENAI_API_KEY=sk-abc" (car result)))
        (should (member "PATH=/usr/bin" result))))))

(ert-deftest test-build-env-normal-assemblyai-prepends-key-var ()
  "assemblyai backend prepends ASSEMBLYAI_API_KEY=<key>."
  (test-env-with-api-key "aai-xyz"
    (let ((process-environment '("PATH=/usr/bin")))
      (let ((result (cj/--build-process-environment 'assemblyai)))
        (should (equal "ASSEMBLYAI_API_KEY=aai-xyz" (car result)))
        (should (member "PATH=/usr/bin" result))))))

(ert-deftest test-build-env-normal-local-whisper-unchanged ()
  "local-whisper has no API key; environment is returned unchanged."
  (let ((process-environment '("PATH=/usr/bin" "HOME=/tmp")))
    (let ((result (cj/--build-process-environment 'local-whisper)))
      (should (equal '("PATH=/usr/bin" "HOME=/tmp") result)))))

(ert-deftest test-build-env-normal-preserves-parent-environment ()
  "Every entry in the parent `process-environment' remains present."
  (test-env-with-api-key "k"
    (let ((process-environment '("A=1" "B=2" "C=3")))
      (let ((result (cj/--build-process-environment 'openai-api)))
        (should (member "A=1" result))
        (should (member "B=2" result))
        (should (member "C=3" result))))))

;;; Boundary Cases

(ert-deftest test-build-env-boundary-empty-parent-env ()
  "Empty parent `process-environment' yields a list with just the API-key var."
  (test-env-with-api-key "k"
    (let ((process-environment '()))
      (let ((result (cj/--build-process-environment 'openai-api)))
        (should (equal '("OPENAI_API_KEY=k") result))))))

(ert-deftest test-build-env-boundary-does-not-mutate-input ()
  "Parent `process-environment' is not destructively modified."
  (test-env-with-api-key "k"
    (let* ((parent '("PATH=/usr/bin"))
           (process-environment parent))
      (cj/--build-process-environment 'openai-api)
      (should (equal '("PATH=/usr/bin") parent)))))

;;; Error Cases

(ert-deftest test-build-env-error-openai-missing-key ()
  "openai-api with no key configured signals user-error."
  (test-env-with-api-key nil
    (should-error (cj/--build-process-environment 'openai-api) :type 'user-error)))

(ert-deftest test-build-env-error-assemblyai-missing-key ()
  "assemblyai with no key configured signals user-error."
  (test-env-with-api-key nil
    (should-error (cj/--build-process-environment 'assemblyai) :type 'user-error)))

(ert-deftest test-build-env-error-unknown-backend ()
  "Unknown backend bubbles up user-error from cj/--backend-plist."
  (should-error (cj/--build-process-environment 'nonexistent) :type 'user-error))

(provide 'test-transcription-process-environment)
;;; test-transcription-process-environment.el ends here
