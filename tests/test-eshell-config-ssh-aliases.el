;;; test-eshell-config-ssh-aliases.el --- Tests for eshell SSH alias building -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--eshell-ssh-alias-commands from eshell-config.el.
;;
;; The helper takes the `cj/eshell-ssh-hosts' alist of (ALIAS-NAME . REMOTE-PATH)
;; and returns (ALIAS-NAME . COMMAND) pairs where COMMAND is the `cd' string
;; `eshell/alias' runs.  Testing the pure helper avoids needing a live Eshell.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eshell-config)

(declare-function cj/--eshell-ssh-alias-commands "eshell-config" (hosts))

;;; Normal Cases

(ert-deftest test-eshell-config-ssh-alias-commands-normal-two-hosts ()
  "Normal: two hosts produce two cd-command alias pairs."
  (let* ((hosts '(("gosb"   . "/sshx:cjennings@wolf.usbx.me:/home/cjennings/")
                  ("gowolf" . "/sshx:cjennings@wolf.usbx.me:/home/cjennings/")))
         (result (cj/--eshell-ssh-alias-commands hosts)))
    (should (= 2 (length result)))
    (should (equal (assoc "gosb" result)
                   '("gosb" . "cd /sshx:cjennings@wolf.usbx.me:/home/cjennings/")))
    (should (equal (assoc "gowolf" result)
                   '("gowolf" . "cd /sshx:cjennings@wolf.usbx.me:/home/cjennings/")))))

(ert-deftest test-eshell-config-ssh-alias-commands-normal-single-host ()
  "Normal: a single host produces one pair, order preserved."
  (let ((result (cj/--eshell-ssh-alias-commands
                 '(("gocj" . "/sshx:cjennings@cjennings.net:/var/cjennings/")))))
    (should (equal result
                   '(("gocj" . "cd /sshx:cjennings@cjennings.net:/var/cjennings/"))))))

;;; Boundary Cases

(ert-deftest test-eshell-config-ssh-alias-commands-boundary-empty ()
  "Boundary: an empty host list produces no SSH aliases."
  (should (null (cj/--eshell-ssh-alias-commands '()))))

(provide 'test-eshell-config-ssh-aliases)
;;; test-eshell-config-ssh-aliases.el ends here.
