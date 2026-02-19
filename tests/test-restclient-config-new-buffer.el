;;; test-restclient-config-new-buffer.el --- Tests for cj/restclient-new-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/restclient-new-buffer function.
;; Creates a scratch *restclient* buffer in restclient-mode.
;; Covers Normal and Boundary cases.

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'restclient-config)

;;; Normal Cases

(ert-deftest test-restclient-new-buffer-creates-buffer ()
  "Creates a buffer named *restclient*."
  (unwind-protect
      (progn
        (cj/restclient-new-buffer)
        (should (get-buffer "*restclient*")))
    (when (get-buffer "*restclient*")
      (kill-buffer "*restclient*"))))

(ert-deftest test-restclient-new-buffer-sets-mode ()
  "Buffer is in restclient-mode."
  (unwind-protect
      (progn
        (cj/restclient-new-buffer)
        (with-current-buffer "*restclient*"
          (should (eq major-mode 'restclient-mode))))
    (when (get-buffer "*restclient*")
      (kill-buffer "*restclient*"))))

(ert-deftest test-restclient-new-buffer-switches-to-buffer ()
  "Switches to the *restclient* buffer."
  (unwind-protect
      (progn
        (cj/restclient-new-buffer)
        (should (string= (buffer-name (current-buffer)) "*restclient*")))
    (when (get-buffer "*restclient*")
      (kill-buffer "*restclient*"))))

;;; Boundary Cases

(ert-deftest test-restclient-new-buffer-reuses-existing ()
  "Reuses existing *restclient* buffer instead of creating a duplicate."
  (unwind-protect
      (let ((buf (get-buffer-create "*restclient*")))
        (with-current-buffer buf
          (restclient-mode)
          (insert "# existing content"))
        (cj/restclient-new-buffer)
        (should (eq (current-buffer) buf))
        (should (string-match-p "existing content" (buffer-string))))
    (when (get-buffer "*restclient*")
      (kill-buffer "*restclient*"))))

(provide 'test-restclient-config-new-buffer)
;;; test-restclient-config-new-buffer.el ends here
