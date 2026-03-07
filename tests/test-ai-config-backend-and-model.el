;;; test-ai-config-backend-and-model.el --- Tests for cj/gptel-backend-and-model -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/gptel-backend-and-model from ai-config.el.
;;
;; Returns a formatted string "backend: model [timestamp]" for use in
;; org headings marking AI responses.  Uses pcase to extract the display
;; name from vector backends, falling back to "AI" otherwise.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-ai-config)
(require 'ai-config)

;;; Normal Cases

(ert-deftest test-ai-config-backend-and-model-normal-vector-backend-extracts-name ()
  "Vector backend should use element at index 1 as display name."
  (let ((gptel-backend (vector 'cl-struct "Claude"))
        (gptel-model "claude-opus-4-6"))
    (let ((result (cj/gptel-backend-and-model)))
      (should (string-match-p "^Claude:" result))
      (should (string-match-p "claude-opus-4-6" result)))))

(ert-deftest test-ai-config-backend-and-model-normal-contains-timestamp ()
  "Result should contain a bracketed timestamp."
  (let ((gptel-backend nil)
        (gptel-model nil))
    (should (string-match-p "\\[[-0-9]+ [0-9]+:[0-9]+:[0-9]+\\]"
                            (cj/gptel-backend-and-model)))))

(ert-deftest test-ai-config-backend-and-model-normal-format-structure ()
  "Result should follow 'backend: model [timestamp]' format."
  (let ((gptel-backend (vector 'cl-struct "TestBackend"))
        (gptel-model "test-model"))
    (should (string-match-p "^TestBackend: test-model \\["
                            (cj/gptel-backend-and-model)))))

;;; Boundary Cases

(ert-deftest test-ai-config-backend-and-model-boundary-nil-backend-shows-ai ()
  "Nil backend should fall back to \"AI\" display name."
  (let ((gptel-backend nil)
        (gptel-model "some-model"))
    (should (string-match-p "^AI:" (cj/gptel-backend-and-model)))))

(ert-deftest test-ai-config-backend-and-model-boundary-nil-model-shows-empty ()
  "Nil model should produce empty string in model position."
  (let ((gptel-backend nil)
        (gptel-model nil))
    (should (string-match-p "^AI:  \\[" (cj/gptel-backend-and-model)))))

(ert-deftest test-ai-config-backend-and-model-boundary-string-backend-shows-ai ()
  "String backend (not vector) should fall back to \"AI\"."
  (let ((gptel-backend "just-a-string")
        (gptel-model "model"))
    (should (string-match-p "^AI:" (cj/gptel-backend-and-model)))))

(ert-deftest test-ai-config-backend-and-model-boundary-symbol-model-formatted ()
  "Symbol model should be formatted as its print representation."
  (let ((gptel-backend nil)
        (gptel-model 'some-model))
    (should (string-match-p "some-model" (cj/gptel-backend-and-model)))))

(ert-deftest test-ai-config-backend-and-model-boundary-timestamp-reflects-today ()
  "Timestamp should contain today's date."
  (let ((gptel-backend nil)
        (gptel-model nil)
        (today (format-time-string "%Y-%m-%d")))
    (should (string-match-p (regexp-quote today)
                            (cj/gptel-backend-and-model)))))

(provide 'test-ai-config-backend-and-model)
;;; test-ai-config-backend-and-model.el ends here
