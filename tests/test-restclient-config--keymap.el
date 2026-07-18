;;; test-restclient-config--keymap.el --- Tests for the restclient prefix keymap -*- lexical-binding: t -*-

;;; Commentary:
;; Pins the C-; R prefix wiring.  The bindings must go through
;; `cj/restclient-map' + `cj/register-prefix-map' (like the other C-;
;; prefixes), not raw `global-set-key' calls that silently depend on
;; keybindings.el having installed C-; first.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'restclient-config)

;;; Normal Cases

(ert-deftest test-restclient-keymap-registered-under-custom-prefix ()
  "Normal: cj/restclient-map is bound at R inside cj/custom-keymap."
  (should (boundp 'cj/restclient-map))
  (should (eq cj/restclient-map (keymap-lookup cj/custom-keymap "R"))))

(ert-deftest test-restclient-keymap-binds-new-buffer-and-open-file ()
  "Normal: the prefix map carries the two restclient commands."
  (should (eq #'cj/restclient-new-buffer (keymap-lookup cj/restclient-map "n")))
  (should (eq #'cj/restclient-open-file (keymap-lookup cj/restclient-map "o"))))

(provide 'test-restclient-config--keymap)
;;; test-restclient-config--keymap.el ends here
