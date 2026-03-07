;;; test-ai-config-fresh-org-prefix.el --- Tests for cj/gptel--fresh-org-prefix -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/gptel--fresh-org-prefix from ai-config.el.
;;
;; Generates an org-mode level-1 heading containing the user's login
;; name and a bracketed timestamp, used as the user message prefix in
;; gptel org-mode conversations.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'testutil-ai-config)
(require 'ai-config)

;;; Normal Cases

(ert-deftest test-ai-config-fresh-org-prefix-normal-starts-with-org-heading ()
  "Result should start with '* ' for an org level-1 heading."
  (should (string-prefix-p "* " (cj/gptel--fresh-org-prefix))))

(ert-deftest test-ai-config-fresh-org-prefix-normal-contains-username ()
  "Result should contain the current user's login name."
  (should (string-match-p (regexp-quote user-login-name)
                          (cj/gptel--fresh-org-prefix))))

(ert-deftest test-ai-config-fresh-org-prefix-normal-contains-timestamp ()
  "Result should contain a bracketed timestamp in YYYY-MM-DD HH:MM:SS format."
  (should (string-match-p "\\[[-0-9]+ [0-9]+:[0-9]+:[0-9]+\\]"
                          (cj/gptel--fresh-org-prefix))))

(ert-deftest test-ai-config-fresh-org-prefix-normal-ends-with-newline ()
  "Result should end with a newline."
  (should (string-suffix-p "\n" (cj/gptel--fresh-org-prefix))))

(ert-deftest test-ai-config-fresh-org-prefix-normal-format-order ()
  "Result should have star, then username, then timestamp in order."
  (let ((result (cj/gptel--fresh-org-prefix)))
    (should (string-match
             (format "^\\* %s \\[" (regexp-quote user-login-name))
             result))))

;;; Boundary Cases

(ert-deftest test-ai-config-fresh-org-prefix-boundary-timestamp-reflects-today ()
  "Timestamp should contain today's date."
  (let ((today (format-time-string "%Y-%m-%d")))
    (should (string-match-p (regexp-quote today)
                            (cj/gptel--fresh-org-prefix)))))

(ert-deftest test-ai-config-fresh-org-prefix-boundary-overridden-username ()
  "Result should reflect a dynamically-bound user-login-name."
  (let ((user-login-name "testuser"))
    (should (string-match-p "testuser" (cj/gptel--fresh-org-prefix)))))

(ert-deftest test-ai-config-fresh-org-prefix-boundary-empty-username ()
  "Empty user-login-name should produce heading with empty name slot."
  (let ((user-login-name ""))
    (should (string-match-p "^\\*  \\[" (cj/gptel--fresh-org-prefix)))))

(provide 'test-ai-config-fresh-org-prefix)
;;; test-ai-config-fresh-org-prefix.el ends here
