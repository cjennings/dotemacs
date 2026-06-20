;;; test-ai-config--apply-model-selection.el --- Tests for cj/--gptel-apply-model-selection -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--gptel-apply-model-selection is the apply step extracted from the
;; interactive cj/gptel-change-model: it sets gptel-backend/gptel-model globally
;; or buffer-locally and returns the confirmation message.  The extraction also
;; dropped a dead `(if (stringp model) ...)' branch (model is always a symbol by
;; that point).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-config)

(defvar gptel-backend)
(defvar gptel-model)

(ert-deftest test-ai-config-apply-model-global-sets-globals ()
  "Normal: global scope assigns the global vars and reports (global)."
  (let ((gptel-backend nil) (gptel-model nil))
    (let ((msg (cj/--gptel-apply-model-selection "global" 'mybackend 'mymodel "MyAI")))
      (should (eq gptel-backend 'mybackend))
      (should (eq gptel-model 'mymodel))
      (should (string-match-p "MyAI" msg))
      (should (string-match-p "mymodel" msg))
      (should (string-match-p "global" msg)))))

(ert-deftest test-ai-config-apply-model-buffer-sets-buffer-locals ()
  "Normal: buffer scope makes the vars buffer-local and reports (buffer-local)."
  (let ((gptel-backend 'orig) (gptel-model 'origm))
    (with-temp-buffer
      (let ((msg (cj/--gptel-apply-model-selection "buffer" 'be 'mo "Name")))
        (should (local-variable-p 'gptel-backend))
        (should (local-variable-p 'gptel-model))
        (should (eq gptel-backend 'be))
        (should (eq gptel-model 'mo))
        (should (string-match-p "buffer-local" msg))))
    ;; outside the temp buffer the globals are untouched
    (should (eq gptel-backend 'orig))
    (should (eq gptel-model 'origm))))

(provide 'test-ai-config--apply-model-selection)
;;; test-ai-config--apply-model-selection.el ends here
