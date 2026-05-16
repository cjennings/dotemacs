;;; test-ai-quick-ask.el --- Tests for ai-quick-ask -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the helpers and orchestration in ai-quick-ask.el.  The
;; quick-ask buffer is exercised via `cl-letf' stubs on
;; `gptel-request' and friends so no network call ever happens.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'testutil-ai-config)
;; Stub gptel-request so cj/gptel-quick-ask doesn't try to hit the network.
(unless (fboundp 'gptel-request)
  (defun gptel-request (&rest _args) nil))

(require 'ai-quick-ask)

;; ------------------------------ pure helpers

(ert-deftest test-ai-quick-ask-initial-text-shape ()
  "Initial text is Q: <prompt> blank line then the response marker."
  (should (equal (cj/gptel-quick--initial-text "hello?")
                 "Q: hello?\n\nA: ")))

(ert-deftest test-ai-quick-ask-extract-response-normal ()
  "Extracts text after the response marker."
  (should (equal (cj/gptel-quick--extract-response "Q: x\n\nA: hello world")
                 "hello world")))

(ert-deftest test-ai-quick-ask-extract-response-multiline ()
  "Multi-line response is returned in full."
  (should (equal (cj/gptel-quick--extract-response
                  "Q: x\n\nA: first line\nsecond line\n")
                 "first line\nsecond line\n")))

(ert-deftest test-ai-quick-ask-extract-response-no-marker ()
  "Buffer without the marker returns nil."
  (should-not (cj/gptel-quick--extract-response "no marker here")))

(ert-deftest test-ai-quick-ask-extract-response-empty ()
  "Empty buffer returns nil."
  (should-not (cj/gptel-quick--extract-response "")))

(ert-deftest test-ai-quick-ask-seed-text-shape ()
  "Seed text has user heading, prompt, AI heading, response."
  (let ((seed (cj/gptel-quick--seed-text "ask" "reply")))
    (should (string-match-p "^\\* .* \\[" seed))
    (should (string-match-p "ask" seed))
    (should (string-match-p "^\\* AI" seed))
    (should (string-match-p "reply" seed))))

(ert-deftest test-ai-quick-ask-seed-text-nil-response ()
  "Seed text with a nil response leaves an empty body for the AI side."
  (let ((seed (cj/gptel-quick--seed-text "ask" nil)))
    (should (string-match-p "^\\* AI" seed))))

;; ------------------------------ ask

(ert-deftest test-ai-quick-ask-creates-buffer ()
  "Ask creates the *GPTel-Quick* buffer in cj/gptel-quick-mode."
  (when (get-buffer cj/gptel-quick--buffer-name)
    (kill-buffer cj/gptel-quick--buffer-name))
  (let (request-called)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (&rest _) (setq request-called t)))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) nil)))
      (cj/gptel-quick-ask "test prompt")
      (let ((buf (get-buffer cj/gptel-quick--buffer-name)))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'cj/gptel-quick-mode))
          (should (equal cj/gptel-quick--prompt "test prompt"))
          (should (string-match-p "Q: test prompt" (buffer-string))))
        (kill-buffer buf))
      (should request-called))))

(ert-deftest test-ai-quick-ask-error-empty-prompt ()
  "Empty prompt signals."
  (should-error (cj/gptel-quick-ask "")))

;; ------------------------------ dismiss

(ert-deftest test-ai-quick-ask-dismiss-kills-buffer ()
  "Dismiss kills the *GPTel-Quick* buffer."
  (let ((buf (get-buffer-create cj/gptel-quick--buffer-name)))
    (should (buffer-live-p buf))
    (cj/gptel-quick-dismiss)
    (should-not (buffer-live-p buf))))

(ert-deftest test-ai-quick-ask-dismiss-no-op-when-absent ()
  "Dismiss with no quick buffer is a no-op."
  (when (get-buffer cj/gptel-quick--buffer-name)
    (kill-buffer cj/gptel-quick--buffer-name))
  ;; Should not error
  (cj/gptel-quick-dismiss))

;; ------------------------------ continue

(ert-deftest test-ai-quick-ask-continue-seeds-ai-assistant ()
  "Continue seeds *AI-Assistant* with prompt + response and kills quick buffer."
  (when (get-buffer cj/gptel-quick--buffer-name)
    (kill-buffer cj/gptel-quick--buffer-name))
  (when (get-buffer "*AI-Assistant*")
    (kill-buffer "*AI-Assistant*"))
  (let ((display-called nil))
    (cl-letf (((symbol-function 'display-buffer-in-side-window)
               (lambda (&rest _) (setq display-called t))))
      ;; Prepare a quick buffer with prompt + response
      (with-current-buffer (get-buffer-create cj/gptel-quick--buffer-name)
        (cj/gptel-quick-mode)
        (let ((inhibit-read-only t))
          (insert (cj/gptel-quick--initial-text "what is X?"))
          (insert "X is a thing."))
        (setq-local cj/gptel-quick--prompt "what is X?")
        ;; Provide a stub *AI-Assistant* so continue doesn't try to call gptel.
        (get-buffer-create "*AI-Assistant*")
        (cj/gptel-quick-continue))
      (should display-called)
      ;; *AI-Assistant* got the seed
      (with-current-buffer "*AI-Assistant*"
        (let ((body (buffer-string)))
          (should (string-match-p "what is X?" body))
          (should (string-match-p "X is a thing\\." body))))
      ;; Quick buffer was dismissed
      (should-not (get-buffer cj/gptel-quick--buffer-name))))
  (kill-buffer "*AI-Assistant*"))

(ert-deftest test-ai-quick-ask-continue-error-outside-quick-buffer ()
  "Continue signals when called outside a quick-ask buffer."
  (with-temp-buffer
    (should-error (cj/gptel-quick-continue))))

(provide 'test-ai-quick-ask)
;;; test-ai-quick-ask.el ends here
