;;; test-ai-config-gptel-backend-libs.el --- Tests for gptel backend-lib loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the "gptel-make-anthropic void" bug.  The local
;; gptel fork (:load-path "~/code/gptel", :ensure nil) ships no generated
;; autoloads, so (require 'gptel) alone never loads gptel-anthropic /
;; gptel-openai where the gptel-make-* constructors live.  The fix is to
;; require those backend libraries explicitly before constructing backends.
;;
;; These tests don't load gptel itself (it isn't reliably loadable in batch);
;; they stub `require' and the constructors to verify the loader requires both
;; libs and that `cj/ensure-gptel-backends' calls it before building backends.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-config)

;; gptel defvars these at runtime; declare them here so the wiring test can
;; let-bind them in a batch session where gptel itself is not loaded.
(defvar gptel-backend)
(defvar gptel-model)

(ert-deftest test-ai-config-gptel-load-backend-libs-requires-both ()
  "Normal: the loader requires gptel-anthropic and gptel-openai so the fork's
make-* constructors exist despite the missing autoloads."
  (let ((required '()))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (push feature required) feature)))
      (cj/--gptel-load-backend-libs))
    (should (memq 'gptel-anthropic required))
    (should (memq 'gptel-openai required))))

(ert-deftest test-ai-config-ensure-gptel-backends-loads-libs-first ()
  "Regression: `cj/ensure-gptel-backends' loads the backend libs before it
calls the constructors, so a fork without autoloads no longer signals
`void-function gptel-make-anthropic'."
  (let ((loaded nil)
        (gptel-claude-backend nil)
        (gptel-chatgpt-backend nil)
        (gptel-backend nil)
        (gptel-model nil))
    (cl-letf (((symbol-function 'cj/--gptel-load-backend-libs)
               (lambda () (setq loaded t)))
              ((symbol-function 'gptel-make-anthropic) (lambda (&rest _) 'claude))
              ((symbol-function 'gptel-make-openai) (lambda (&rest _) 'chatgpt))
              ((symbol-function 'cj/anthropic-api-key) (lambda () "k"))
              ((symbol-function 'cj/openai-api-key) (lambda () "k")))
      (cj/ensure-gptel-backends))
    (should loaded)
    (should (eq gptel-claude-backend 'claude))
    (should (eq gptel-chatgpt-backend 'chatgpt))))

(provide 'test-ai-config-gptel-backend-libs)
;;; test-ai-config-gptel-backend-libs.el ends here
