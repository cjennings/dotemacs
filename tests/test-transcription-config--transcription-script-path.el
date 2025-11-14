;;; test-transcription-config--transcription-script-path.el --- Tests for cj/--transcription-script-path -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--transcription-script-path function from transcription-config.el
;;
;; This function returns the absolute path to the transcription script based on
;; the current value of cj/transcribe-backend.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub notification function
(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args)
    "Stub notification function for testing."
    nil))

;; Now load the actual production module
(require 'transcription-config)

;;; Setup and Teardown

(defun test-transcription-script-path-setup ()
  "Set up test environment."
  ;; Save original backend setting
  (setq test-transcription-original-backend cj/transcribe-backend))

(defun test-transcription-script-path-teardown ()
  "Clean up test environment."
  ;; Restore original backend setting
  (setq cj/transcribe-backend test-transcription-original-backend))

;;; Normal Cases

(ert-deftest test-transcription-config--transcription-script-path-normal-openai-api-returns-oai-transcribe ()
  "Should return oai-transcribe script path for openai-api backend."
  (test-transcription-script-path-setup)
  (unwind-protect
      (progn
        (setq cj/transcribe-backend 'openai-api)
        (let ((result (cj/--transcription-script-path)))
          (should (stringp result))
          (should (string-suffix-p "scripts/oai-transcribe" result))
          (should (string-prefix-p (expand-file-name user-emacs-directory) result))))
    (test-transcription-script-path-teardown)))

(ert-deftest test-transcription-config--transcription-script-path-normal-assemblyai-returns-assemblyai-transcribe ()
  "Should return assemblyai-transcribe script path for assemblyai backend."
  (test-transcription-script-path-setup)
  (unwind-protect
      (progn
        (setq cj/transcribe-backend 'assemblyai)
        (let ((result (cj/--transcription-script-path)))
          (should (stringp result))
          (should (string-suffix-p "scripts/assemblyai-transcribe" result))
          (should (string-prefix-p (expand-file-name user-emacs-directory) result))))
    (test-transcription-script-path-teardown)))

(ert-deftest test-transcription-config--transcription-script-path-normal-local-whisper-returns-local-whisper ()
  "Should return local-whisper script path for local-whisper backend."
  (test-transcription-script-path-setup)
  (unwind-protect
      (progn
        (setq cj/transcribe-backend 'local-whisper)
        (let ((result (cj/--transcription-script-path)))
          (should (stringp result))
          (should (string-suffix-p "scripts/local-whisper" result))
          (should (string-prefix-p (expand-file-name user-emacs-directory) result))))
    (test-transcription-script-path-teardown)))

(ert-deftest test-transcription-config--transcription-script-path-normal-returns-absolute-path ()
  "Should return absolute path starting with user-emacs-directory."
  (test-transcription-script-path-setup)
  (unwind-protect
      (progn
        (setq cj/transcribe-backend 'openai-api)
        (let ((result (cj/--transcription-script-path)))
          (should (file-name-absolute-p result))
          (should (string-prefix-p "/" result))))
    (test-transcription-script-path-teardown)))

;;; Boundary Cases

(ert-deftest test-transcription-config--transcription-script-path-boundary-path-format-consistent ()
  "Should return paths in consistent format across backends."
  (test-transcription-script-path-setup)
  (unwind-protect
      (let (paths)
        (dolist (backend '(openai-api assemblyai local-whisper))
          (setq cj/transcribe-backend backend)
          (push (cj/--transcription-script-path) paths))
        ;; All paths should have same structure: <emacs-dir>/scripts/<name>
        (should (= (length paths) 3))
        (should (seq-every-p (lambda (p) (string-match-p "/scripts/[^/]+$" p)) paths)))
    (test-transcription-script-path-teardown)))

(provide 'test-transcription-config--transcription-script-path)
;;; test-transcription-config--transcription-script-path.el ends here
