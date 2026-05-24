;;; test-eww-config-user-agent-advice.el --- Tests for EWW user-agent advice -*- lexical-binding: t; -*-

;;; Commentary:
;; my-eww--inject-user-agent is an :around advice on url-retrieve(-synchronously)
;; that adds a desktop User-Agent only for requests originating in an EWW
;; buffer.  These tests pin that scoping: package.el and other non-EWW URL
;; callers must pass through untouched, so the advice can't change how the
;; rest of Emacs fetches URLs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eww)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eww-config)

;;; Normal Cases

(ert-deftest test-eww-ua-injected-in-eww-buffer ()
  "Normal: in an eww-mode buffer the advice adds the configured User-Agent."
  (with-temp-buffer
    (setq major-mode 'eww-mode)
    (let (seen)
      (my-eww--inject-user-agent
       (lambda (&rest _) (setq seen (assoc "User-Agent" url-request-extra-headers))))
      (should seen)
      (should (equal (cdr seen) my-eww-user-agent)))))

;;; Boundary Cases

(ert-deftest test-eww-ua-not-injected-outside-eww ()
  "Boundary: outside eww-mode (package.el, normal url fetches) no UA is added."
  (with-temp-buffer
    (fundamental-mode)
    (let ((url-request-extra-headers nil) captured)
      (my-eww--inject-user-agent
       (lambda (&rest _) (setq captured url-request-extra-headers)))
      (should (null (assoc "User-Agent" captured))))))

(ert-deftest test-eww-ua-replaces-existing-and-keeps-other-headers ()
  "Boundary: an existing User-Agent is replaced (not duplicated) and other
headers survive, when the request comes from EWW."
  (with-temp-buffer
    (setq major-mode 'eww-mode)
    (let ((url-request-extra-headers '(("User-Agent" . "old") ("Accept" . "x")))
          seen)
      (my-eww--inject-user-agent
       (lambda (&rest _) (setq seen url-request-extra-headers)))
      (should (= 1 (cl-count "User-Agent" seen :key #'car :test #'string-equal)))
      (should (equal (cdr (assoc "User-Agent" seen)) my-eww-user-agent))
      (should (assoc "Accept" seen)))))

(provide 'test-eww-config-user-agent-advice)
;;; test-eww-config-user-agent-advice.el ends here
