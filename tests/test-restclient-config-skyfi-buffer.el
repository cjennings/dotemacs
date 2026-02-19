;;; test-restclient-config-skyfi-buffer.el --- Tests for cj/restclient-skyfi-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/restclient-skyfi-buffer function.
;; Opens the SkyFi API template file.
;; Covers Normal and Error cases.

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'restclient-config)

;;; Normal Cases

(ert-deftest test-restclient-skyfi-buffer-opens-file ()
  "Opens existing skyfi-api.rest file and switches to it."
  (let ((skyfi-file (expand-file-name "data/skyfi-api.rest" user-emacs-directory)))
    (when (file-exists-p skyfi-file)
      (unwind-protect
          (progn
            (cj/restclient-skyfi-buffer)
            (should (string-match-p "skyfi-api\\.rest"
                                    (buffer-file-name (current-buffer)))))
        (when-let ((buf (get-file-buffer skyfi-file)))
          (kill-buffer buf))))))

;;; Error Cases

(ert-deftest test-restclient-skyfi-buffer-missing-file-signals-error ()
  "Signals user-error when skyfi-api.rest does not exist."
  (let ((cj/restclient-data-dir "/tmp/nonexistent-restclient-test-dir/"))
    (should-error (cj/restclient-skyfi-buffer) :type 'user-error)))

(provide 'test-restclient-config-skyfi-buffer)
;;; test-restclient-config-skyfi-buffer.el ends here
