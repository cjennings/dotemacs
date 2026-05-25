;;; test-system-defaults-vc-follow-symlinks.el --- Tests for VC symlink default -*- lexical-binding: t; -*-

;;; Commentary:

;; system-defaults.el has startup side effects, so load it with unrelated
;; external interactions stubbed and assert the setting this file owns.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-system-defaults)

(ert-deftest test-system-defaults-vc-follow-symlinks-normal-sets-t ()
  "Normal: system-defaults follows version-controlled symlinks without asking."
  (test-system-defaults--with-load-environment
    (let ((vc-follow-symlinks nil))
      (test-system-defaults--load)
      (should (eq vc-follow-symlinks t)))))

(provide 'test-system-defaults-vc-follow-symlinks)
;;; test-system-defaults-vc-follow-symlinks.el ends here
