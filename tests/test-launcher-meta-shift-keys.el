;;; test-launcher-meta-shift-keys.el --- Meta+Shift launcher keys reach their commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Meta+Shift+<letter> emits the uppercase event (M-E, M-R, M-B), so a
;; binding written as "M-S-e" on a lowercase letter is never reached by
;; the keychord.  These launchers must be bound under the uppercase
;; event the keyboard actually produces.

;;; Code:

(require 'ert)
(require 'testutil-general)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eww-config)
(require 'elfeed-config)
(require 'calibredb-epub-config)

(ert-deftest test-launcher-meta-shift-e-opens-eww ()
  "Normal: M-E (Meta+Shift+e) is bound to `eww'."
  (should (eq (key-binding (kbd "M-E")) 'eww)))

(ert-deftest test-launcher-meta-shift-r-opens-elfeed ()
  "Normal: M-R (Meta+Shift+r) is bound to `cj/elfeed-open'."
  (should (eq (key-binding (kbd "M-R")) 'cj/elfeed-open)))

(ert-deftest test-launcher-meta-shift-b-opens-calibredb ()
  "Normal: M-B (Meta+Shift+b) is bound to `calibredb'."
  (should (eq (key-binding (kbd "M-B")) 'calibredb)))

(provide 'test-launcher-meta-shift-keys)
;;; test-launcher-meta-shift-keys.el ends here
