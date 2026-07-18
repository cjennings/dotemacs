;;; test-httpd-config--defer.el --- Tests for httpd-config lazy loading -*- lexical-binding: t -*-

;;; Commentary:
;; Pins httpd-config's load-time behavior: merely loading the module must
;; not create the www directory (that belongs to the moment simple-httpd
;; actually loads) and must not pull in simple-httpd itself —
;; impatient-mode requires it on demand.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;;; Boundary Cases

(ert-deftest test-httpd-config-load-creates-no-www-dir ()
  "Boundary: loading httpd-config does not create www/ in user-emacs-directory."
  (let* ((sandbox (make-temp-file "httpd-config-test-" t))
         (user-emacs-directory (file-name-as-directory sandbox)))
    (unwind-protect
        (progn
          (require 'httpd-config)
          (should-not (file-directory-p
                       (expand-file-name "www" user-emacs-directory))))
      (delete-directory sandbox t))))

(ert-deftest test-httpd-config-load-does-not-load-simple-httpd ()
  "Boundary: loading httpd-config leaves simple-httpd unloaded."
  (require 'httpd-config)
  (should-not (featurep 'simple-httpd)))

(provide 'test-httpd-config--defer)
;;; test-httpd-config--defer.el ends here
