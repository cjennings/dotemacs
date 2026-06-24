;;; test-ai-config-current-model-selection.el --- Tests for cj/gptel--current-model-selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/gptel--current-model-selection from ai-config.el.
;;
;; Pure function that formats the active backend and model into a display
;; string like "Anthropic - Claude: claude-opus-4-6".  Used as the default
;; selection in the model-switching completing-read prompt.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-ai-config)
(require 'ai-config)

;;; Normal Cases

(ert-deftest test-ai-config-current-model-selection-normal-matching-backend ()
  "When current backend is in the backends alist, use its display name."
  (let* ((backend-obj 'my-backend)
         (backends `(("Anthropic - Claude" . ,backend-obj))))
    (should (equal (cj/gptel--current-model-selection backends backend-obj "opus")
                   "Anthropic - Claude: opus"))))

(ert-deftest test-ai-config-current-model-selection-normal-symbol-model ()
  "Symbol model should be converted to string in the output."
  (let* ((backend-obj 'my-backend)
         (backends `(("Claude" . ,backend-obj))))
    (should (equal (cj/gptel--current-model-selection backends backend-obj 'opus)
                   "Claude: opus"))))

(ert-deftest test-ai-config-current-model-selection-normal-multiple-backends ()
  "Should find the correct backend name among multiple backends."
  (let* ((backend-a 'backend-a)
         (backend-b 'backend-b)
         (backends `(("Claude" . ,backend-a) ("OpenAI" . ,backend-b))))
    (should (equal (cj/gptel--current-model-selection backends backend-b "gpt-4o")
                   "OpenAI: gpt-4o"))))

;;; Boundary Cases

(ert-deftest test-ai-config-current-model-selection-boundary-nil-backend-shows-ai ()
  "Nil backend (not in alist) should fall back to \"AI\"."
  (should (equal (cj/gptel--current-model-selection '(("Claude" . x)) nil "opus")
                 "AI: opus")))

(ert-deftest test-ai-config-current-model-selection-boundary-unknown-backend-shows-ai ()
  "Backend not found in alist should fall back to \"AI\"."
  (should (equal (cj/gptel--current-model-selection
                  '(("Claude" . backend-a)) 'unknown-backend "opus")
                 "AI: opus")))

(ert-deftest test-ai-config-current-model-selection-boundary-nil-model ()
  "Nil model should produce \"nil\" in the model position (symbolp nil)."
  (let* ((backend 'my-backend)
         (backends `(("Claude" . ,backend))))
    (should (equal (cj/gptel--current-model-selection backends backend nil)
                   "Claude: nil"))))

(ert-deftest test-ai-config-current-model-selection-boundary-empty-backends ()
  "Empty backends alist should fall back to \"AI\" for backend name."
  (should (equal (cj/gptel--current-model-selection nil 'anything "model")
                 "AI: model")))

(ert-deftest test-ai-config-current-model-selection-boundary-both-nil ()
  "Nil backend and nil model should produce \"AI: nil\"."
  (should (equal (cj/gptel--current-model-selection nil nil nil)
                 "AI: nil")))

(provide 'test-ai-config-current-model-selection)
;;; test-ai-config-current-model-selection.el ends here
