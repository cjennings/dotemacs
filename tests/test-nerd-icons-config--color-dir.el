;;; test-nerd-icons-config--color-dir.el --- Tests for cj/--nerd-icons-color-dir -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the :filter-return advice that attaches a color face to the
;; output of `nerd-icons-icon-for-dir'. Without this, directory icons render
;; in the buffer default face — so a face-remap of `default' (buffer-local
;; or global) catches the icons unintentionally.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'nerd-icons-config)

(ert-deftest test-nerd-icons-config--color-dir-attaches-face ()
  "Normal: adds nerd-icons-yellow to the face stack of a propertized icon."
  (let ((icon (propertize "X" 'face '(:family "Symbols Nerd Font Mono" :height 1.0))))
    (let ((result (cj/--nerd-icons-color-dir icon)))
      (should (memq 'nerd-icons-yellow
                    (ensure-list (get-text-property 0 'face result)))))))

(ert-deftest test-nerd-icons-config--color-dir-preserves-family ()
  "Normal: retains the original :family / :height attributes."
  (let ((icon (propertize "X" 'face '(:family "Symbols Nerd Font Mono" :height 1.0))))
    (let* ((result (cj/--nerd-icons-color-dir icon))
           (face   (get-text-property 0 'face result))
           (specs  (ensure-list face))
           (plist  (seq-find (lambda (x) (and (listp x) (plist-member x :family))) specs)))
      (should (equal (plist-get plist :family) "Symbols Nerd Font Mono"))
      (should (equal (plist-get plist :height) 1.0)))))

(ert-deftest test-nerd-icons-config--color-dir-empty-string ()
  "Boundary: an empty icon string returns unchanged (no range to propertize)."
  (let ((icon ""))
    (should (equal (cj/--nerd-icons-color-dir icon) ""))))

(ert-deftest test-nerd-icons-config--color-dir-non-string-passthrough ()
  "Error: a nil input returns nil rather than erroring."
  (should-not (cj/--nerd-icons-color-dir nil)))

(ert-deftest test-nerd-icons-config--color-dir-idempotent ()
  "Boundary: calling twice on the same icon adds the face only once.
nerd-icons memoizes its return strings — without this guard, repeated
renders would stack `nerd-icons-yellow' over and over on the cached string."
  (let ((icon (propertize "X" 'face '(:family "Symbols Nerd Font Mono" :height 1.0))))
    (cj/--nerd-icons-color-dir icon)
    (cj/--nerd-icons-color-dir icon)
    (cj/--nerd-icons-color-dir icon)
    (let* ((face (get-text-property 0 'face icon))
           (specs (ensure-list face))
           (yellows (cl-count 'nerd-icons-yellow specs)))
      (should (= yellows 1)))))

(ert-deftest test-nerd-icons-config--color-dir-precedence-over-completion-face ()
  "Normal: when the dir icon already carries nerd-icons-completion-dir-face
\(what `nerd-icons-completion-get-icon' passes), the advice prepends
nerd-icons-yellow so it is first in the face list and wins the merge.  Locks
the dir-precedence decision: the prepended advice face outranks the package's
:face, even though that face lives in a different package."
  (let* ((icon (propertize "X" 'face 'nerd-icons-completion-dir-face))
         (result (cj/--nerd-icons-color-dir icon))
         (faces (ensure-list (get-text-property 0 'face result))))
    (should (memq 'nerd-icons-yellow faces))
    (should (memq 'nerd-icons-completion-dir-face faces))
    (should (= 0 (cl-position 'nerd-icons-yellow faces)))
    (should (< (cl-position 'nerd-icons-yellow faces)
               (cl-position 'nerd-icons-completion-dir-face faces)))))

(provide 'test-nerd-icons-config--color-dir)
;;; test-nerd-icons-config--color-dir.el ends here
