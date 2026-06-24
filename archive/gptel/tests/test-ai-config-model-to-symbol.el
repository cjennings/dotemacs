;;; test-ai-config-model-to-symbol.el --- Tests for cj/gptel--model-to-symbol -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/gptel--model-to-symbol from ai-config.el.
;;
;; Pure function that coerces a model identifier (string, symbol, or other
;; type) to a symbol.  `gptel-model' MUST be a symbol -- gptel's modeline
;; code calls `symbolp' on it and signals wrong-type-argument on a string,
;; which manifests as a redisplay hang.  The function's invariant is that
;; the result is always a symbol, so a value coerced through it is safe to
;; assign to `gptel-model'.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-ai-config)
(require 'ai-config)

;;; Normal Cases

(ert-deftest test-ai-config-model-to-symbol-normal-string-interns ()
  "Normal: a string model name is interned to the matching symbol."
  (should (eq (cj/gptel--model-to-symbol "claude-opus-4-8") 'claude-opus-4-8)))

(ert-deftest test-ai-config-model-to-symbol-normal-symbol-returns-symbol ()
  "Normal: a symbol model name is returned unchanged."
  (should (eq (cj/gptel--model-to-symbol 'gpt-4o) 'gpt-4o)))

(ert-deftest test-ai-config-model-to-symbol-normal-result-always-symbol ()
  "Normal: the invariant -- the result is always a symbol (the crash guard)."
  (should (symbolp (cj/gptel--model-to-symbol "gpt-5.5")))
  (should (symbolp (cj/gptel--model-to-symbol 'gpt-5.5))))

;;; Boundary Cases

(ert-deftest test-ai-config-model-to-symbol-boundary-empty-string-is-symbol ()
  "Boundary: empty string interns to a symbol (still satisfies the invariant)."
  (should (symbolp (cj/gptel--model-to-symbol ""))))

(ert-deftest test-ai-config-model-to-symbol-boundary-nil-returns-nil ()
  "Boundary: nil is already a symbol, returned unchanged."
  (should (eq (cj/gptel--model-to-symbol nil) nil))
  (should (symbolp (cj/gptel--model-to-symbol nil))))

(ert-deftest test-ai-config-model-to-symbol-boundary-string-with-spaces-interns ()
  "Boundary: a string with spaces interns to a single symbol with that name."
  (should (eq (cj/gptel--model-to-symbol "model with spaces")
              (intern "model with spaces"))))

;;; Error/Odd Cases

(ert-deftest test-ai-config-model-to-symbol-number-formats-then-interns ()
  "Error: a non-string, non-symbol value is formatted then interned to a symbol."
  (should (eq (cj/gptel--model-to-symbol 42) (intern "42")))
  (should (symbolp (cj/gptel--model-to-symbol 42))))

(provide 'test-ai-config-model-to-symbol)
;;; test-ai-config-model-to-symbol.el ends here
