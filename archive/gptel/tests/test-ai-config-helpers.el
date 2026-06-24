;;; test-ai-config-helpers.el --- Tests for ai-config helper functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers helpers that don't depend on a live gptel install:
;;
;;   cj/auth-source-secret
;;   cj/anthropic-api-key   (caching wrapper)
;;   cj/openai-api-key      (caching wrapper)
;;   cj/gptel--add-file-to-context
;;   cj/gptel-clear-buffer
;;   cj/gptel-context-clear
;;   cj/gptel-insert-model-heading
;;
;; External primitives (`auth-source-search', `gptel-add-file', etc.)
;; are stubbed so the tests never touch the keyring or the network.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-config)

;; Make `gptel-context--alist' a real dynamic variable for the fallback
;; test below.  Under lexical-binding a plain `let' is lexical, so the
;; `setq' inside `cj/gptel-context-clear' would otherwise miss it.
(defvar gptel-context--alist nil
  "Dynamic stand-in for the gptel-context alist (gptel not loaded here).")

;;; cj/auth-source-secret

(ert-deftest test-ai-config-auth-source-secret-returns-string ()
  "Normal: a plain-string secret comes back as-is."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) '((:secret "plaintext")))))
    (should (equal (cj/auth-source-secret "example.com" "user")
                   "plaintext"))))

(ert-deftest test-ai-config-auth-source-secret-unwraps-function ()
  "Normal: a function secret is funcall'd to retrieve the value."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) (list (list :secret (lambda () "called"))))))
    (should (equal (cj/auth-source-secret "example.com" "user")
                   "called"))))

(ert-deftest test-ai-config-auth-source-secret-errors-when-missing ()
  "Error: an empty result raises a clear error."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) nil)))
    (should-error (cj/auth-source-secret "nope.example.com" "user")
                  :type 'error)))

;;; cj/anthropic-api-key / cj/openai-api-key

(ert-deftest test-ai-config-anthropic-api-key-caches-after-first-call ()
  "Normal: a subsequent call returns the cached value without re-fetching."
  (let ((cj/anthropic-api-key-cached nil)
        (call-count 0))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (cl-incf call-count)
                 '((:secret "anth-key")))))
      (should (equal (cj/anthropic-api-key) "anth-key"))
      (should (equal (cj/anthropic-api-key) "anth-key"))
      (should (= call-count 1)))))

(ert-deftest test-ai-config-openai-api-key-caches-after-first-call ()
  "Normal: same caching contract as the anthropic key."
  (let ((cj/openai-api-key-cached nil)
        (call-count 0))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (cl-incf call-count)
                 '((:secret "oai-key")))))
      (should (equal (cj/openai-api-key) "oai-key"))
      (should (equal (cj/openai-api-key) "oai-key"))
      (should (= call-count 1)))))

;;; cj/gptel--add-file-to-context

(ert-deftest test-ai-config-add-file-to-context-adds-existing-file ()
  "Normal: an existing file is added and the function returns t."
  (let ((tmp (make-temp-file "ai-config-add-file-")))
    (unwind-protect
        (let ((gptel-context--alist nil)
              (added nil))
          (cl-letf (((symbol-function 'gptel-add-file)
                     (lambda (f) (setq added f)))
                    ((symbol-function 'message) #'ignore))
            (should (eq (cj/gptel--add-file-to-context tmp) t))
            (should (equal added tmp))))
      (delete-file tmp))))

(ert-deftest test-ai-config-add-file-to-context-skips-missing-file ()
  "Boundary: a non-existent path returns nil and doesn't call gptel-add-file."
  (let ((called nil))
    (cl-letf (((symbol-function 'gptel-add-file)
               (lambda (_) (setq called t))))
      (should-not (cj/gptel--add-file-to-context "/no/such/path"))
      (should-not called))))

(ert-deftest test-ai-config-add-file-to-context-skips-nil-path ()
  "Boundary: a nil path returns nil without calling gptel-add-file."
  (let ((called nil))
    (cl-letf (((symbol-function 'gptel-add-file)
               (lambda (_) (setq called t))))
      (should-not (cj/gptel--add-file-to-context nil))
      (should-not called))))

;;; cj/gptel-clear-buffer

(ert-deftest test-ai-config-clear-buffer-erases-in-gptel-org-buffer ()
  "Normal: a gptel-mode org buffer is erased and the fresh org prefix is reinserted."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (setq-local gptel-mode t)
    (insert "* Existing conversation\nstuff\n")
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/gptel-clear-buffer))
      (should (string-match-p "cleared" msg)))
    ;; The fresh prefix is an org heading starting with "* ".
    (should (string-prefix-p "* " (buffer-string)))
    (should-not (string-match-p "Existing conversation" (buffer-string)))))

(ert-deftest test-ai-config-clear-buffer-noop-when-not-gptel-org ()
  "Boundary: in a non-gptel buffer the function messages and changes nothing."
  (with-temp-buffer
    (insert "untouched\n")
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/gptel-clear-buffer))
      (should (string-match-p "Not a GPTel buffer" msg))
      (should (equal (buffer-string) "untouched\n")))))

;;; cj/gptel-context-clear

(ert-deftest test-ai-config-context-clear-uses-remove-all-when-available ()
  "Normal: when `gptel-context-remove-all' is bound, it wins the cond.
The stub must be a command because `cj/gptel-context-clear' invokes it
via `call-interactively'."
  (let ((called nil)
        (msg nil))
    (cl-letf (((symbol-function 'gptel-context-remove-all)
               (lambda () (interactive) (setq called 'remove-all)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-context-clear))
    (should (eq called 'remove-all))
    (should (string-match-p "cleared" msg))))

(ert-deftest test-ai-config-context-clear-falls-back-to-alist-setq ()
  "Boundary: when no clearing function exists, the alist is set to nil."
  (let ((gptel-context--alist '((:dummy)))
        (msg nil))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (not (memq sym '(gptel-context-remove-all gptel-context-clear)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-context-clear))
    (should (null gptel-context--alist))
    (should (string-match-p "cleared" msg))))

;;; cj/gptel-insert-model-heading

(ert-deftest test-ai-config-insert-model-heading-inserts-at-given-position ()
  "Normal: an Org heading is inserted at RESPONSE-BEGIN-POS."
  (with-temp-buffer
    (insert "response text")
    (cl-letf (((symbol-function 'cj/gptel-backend-and-model)
               (lambda () "Anthropic: claude-test [2026-05-13 12:00:00]")))
      (cj/gptel-insert-model-heading (point-min) (point-max)))
    (should (string-prefix-p "* Anthropic: claude-test" (buffer-string)))
    (should (string-match-p "\nresponse text" (buffer-string)))))

(provide 'test-ai-config-helpers)
;;; test-ai-config-helpers.el ends here
