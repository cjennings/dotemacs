;;; test-erc-config--generate-buffer-name.el --- Tests for cj/erc-generate-buffer-name -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/erc-generate-buffer-name formats an ERC buffer name as SERVER-CHANNEL.
;; It was defined inside the erc use-package :config (so unreachable under
;; `make test'); lifting it to top level makes it unit-testable.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'erc-config)

(ert-deftest test-erc-generate-buffer-name-server-and-channel ()
  "Normal: a target yields SERVER-CHANNEL."
  (should (equal (cj/erc-generate-buffer-name '(:server "libera" :target "#emacs"))
                 "libera-#emacs")))

(ert-deftest test-erc-generate-buffer-name-server-only ()
  "Boundary: no target yields just the server name."
  (should (equal (cj/erc-generate-buffer-name '(:server "libera"))
                 "libera")))

(ert-deftest test-erc-generate-buffer-name-missing-pieces ()
  "Boundary: missing server/target degrade to empty strings, not nil."
  (should (equal (cj/erc-generate-buffer-name '(:target "#emacs")) "-#emacs"))
  (should (equal (cj/erc-generate-buffer-name '()) "")))

(provide 'test-erc-config--generate-buffer-name)
;;; test-erc-config--generate-buffer-name.el ends here
