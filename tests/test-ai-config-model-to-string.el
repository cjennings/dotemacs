;;; test-ai-config-model-to-string.el --- Tests for cj/gptel--model-to-string -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/gptel--model-to-string from ai-config.el.
;;
;; Pure function that converts a model identifier (string, symbol, or
;; other type) to a string representation.  Branches on input type:
;; string (identity), symbol (symbol-name), fallback (format).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-ai-config)
(require 'ai-config)

;;; Normal Cases

(ert-deftest test-ai-config-model-to-string-normal-string-returns-string ()
  "String model name should be returned unchanged."
  (should (equal (cj/gptel--model-to-string "claude-opus-4-6") "claude-opus-4-6")))

(ert-deftest test-ai-config-model-to-string-normal-symbol-returns-symbol-name ()
  "Symbol model name should return its symbol-name."
  (should (equal (cj/gptel--model-to-string 'gpt-4o) "gpt-4o")))

(ert-deftest test-ai-config-model-to-string-normal-number-returns-formatted ()
  "Numeric input should be formatted as a string."
  (should (equal (cj/gptel--model-to-string 42) "42")))

;;; Boundary Cases

(ert-deftest test-ai-config-model-to-string-boundary-empty-string-returns-empty ()
  "Empty string should be returned as empty string."
  (should (equal (cj/gptel--model-to-string "") "")))

(ert-deftest test-ai-config-model-to-string-boundary-nil-returns-nil-string ()
  "Nil is a symbol, so should return \"nil\"."
  (should (equal (cj/gptel--model-to-string nil) "nil")))

(ert-deftest test-ai-config-model-to-string-boundary-keyword-symbol-includes-colon ()
  "Keyword symbol should return its name including the colon."
  (should (equal (cj/gptel--model-to-string :some-model) ":some-model")))

(ert-deftest test-ai-config-model-to-string-boundary-list-uses-format-fallback ()
  "List input should hit the fallback format branch."
  (should (equal (cj/gptel--model-to-string '(a b)) "(a b)")))

(ert-deftest test-ai-config-model-to-string-boundary-vector-uses-format-fallback ()
  "Vector input should hit the fallback format branch."
  (should (equal (cj/gptel--model-to-string [1 2]) "[1 2]")))

(ert-deftest test-ai-config-model-to-string-boundary-string-with-spaces-unchanged ()
  "String with spaces should be returned unchanged."
  (should (equal (cj/gptel--model-to-string "model with spaces") "model with spaces")))

(provide 'test-ai-config-model-to-string)
;;; test-ai-config-model-to-string.el ends here
