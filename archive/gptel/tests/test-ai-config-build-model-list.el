;;; test-ai-config-build-model-list.el --- Tests for cj/gptel--build-model-list -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/gptel--build-model-list from ai-config.el.
;;
;; Pure function that takes a backends alist and a model-fetching function,
;; and produces a flat list of (DISPLAY-STRING BACKEND MODEL-STRING BACKEND-NAME)
;; entries suitable for completing-read.  Exercises the mapping and string
;; formatting logic that was previously embedded in cj/gptel-change-model.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-ai-config)
(require 'ai-config)

;;; Normal Cases

(ert-deftest test-ai-config-build-model-list-normal-single-backend-single-model ()
  "One backend with one model should produce one entry."
  (let* ((backend-obj 'fake-backend)
         (backends `(("Claude" . ,backend-obj)))
         (result (cj/gptel--build-model-list backends (lambda (_) '("opus")))))
    (should (= 1 (length result)))
    (should (equal (car (nth 0 result)) "Claude: opus"))
    (should (eq (nth 1 (nth 0 result)) backend-obj))
    (should (equal (nth 2 (nth 0 result)) "opus"))
    (should (equal (nth 3 (nth 0 result)) "Claude"))))

(ert-deftest test-ai-config-build-model-list-normal-single-backend-multiple-models ()
  "One backend with multiple models should produce one entry per model."
  (let* ((backends '(("Claude" . backend-a)))
         (result (cj/gptel--build-model-list
                  backends (lambda (_) '("opus" "sonnet" "haiku")))))
    (should (= 3 (length result)))
    (should (equal (mapcar #'car result)
                   '("Claude: opus" "Claude: sonnet" "Claude: haiku")))))

(ert-deftest test-ai-config-build-model-list-normal-multiple-backends ()
  "Multiple backends should interleave their models in backend order."
  (let* ((backends '(("Claude" . backend-a) ("OpenAI" . backend-b)))
         (result (cj/gptel--build-model-list
                  backends
                  (lambda (b)
                    (if (eq b 'backend-a) '("opus") '("gpt-4o"))))))
    (should (= 2 (length result)))
    (should (equal (car (nth 0 result)) "Claude: opus"))
    (should (equal (car (nth 1 result)) "OpenAI: gpt-4o"))))

(ert-deftest test-ai-config-build-model-list-normal-preserves-backend-object ()
  "Each entry should carry the original backend object for later use."
  (let* ((obj (vector 'struct "Claude"))
         (backends `(("Claude" . ,obj)))
         (result (cj/gptel--build-model-list backends (lambda (_) '("opus")))))
    (should (eq (nth 1 (nth 0 result)) obj))))

(ert-deftest test-ai-config-build-model-list-normal-symbol-models-converted ()
  "Symbol model identifiers should be converted to strings via model-to-string."
  (let* ((backends '(("Claude" . backend-a)))
         (result (cj/gptel--build-model-list
                  backends (lambda (_) '(opus sonnet)))))
    (should (equal (nth 2 (nth 0 result)) "opus"))
    (should (equal (nth 2 (nth 1 result)) "sonnet"))))

;;; Boundary Cases

(ert-deftest test-ai-config-build-model-list-boundary-empty-backends ()
  "Empty backends list should produce empty result."
  (should (null (cj/gptel--build-model-list nil (lambda (_) '("x"))))))

(ert-deftest test-ai-config-build-model-list-boundary-backend-with-no-models ()
  "Backend returning no models should contribute no entries."
  (let* ((backends '(("Claude" . backend-a)))
         (result (cj/gptel--build-model-list backends (lambda (_) nil))))
    (should (null result))))

(ert-deftest test-ai-config-build-model-list-boundary-mixed-empty-and-populated ()
  "Only backends with models should produce entries."
  (let* ((backends '(("Claude" . backend-a) ("Empty" . backend-b) ("OpenAI" . backend-c)))
         (result (cj/gptel--build-model-list
                  backends
                  (lambda (b)
                    (cond ((eq b 'backend-a) '("opus"))
                          ((eq b 'backend-b) nil)
                          ((eq b 'backend-c) '("gpt-4o")))))))
    (should (= 2 (length result)))
    (should (equal (nth 3 (nth 0 result)) "Claude"))
    (should (equal (nth 3 (nth 1 result)) "OpenAI"))))

(ert-deftest test-ai-config-build-model-list-boundary-model-with-special-characters ()
  "Model names with special characters should be preserved in display string."
  (let* ((backends '(("Claude" . backend-a)))
         (result (cj/gptel--build-model-list
                  backends (lambda (_) '("claude-haiku-4-5-20251001")))))
    (should (equal (car (nth 0 result)) "Claude: claude-haiku-4-5-20251001"))))

(provide 'test-ai-config-build-model-list)
;;; test-ai-config-build-model-list.el ends here
