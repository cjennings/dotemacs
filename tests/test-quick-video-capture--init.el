;;; test-quick-video-capture--init.el --- Tests for video-capture lazy init -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialization is split so module load has no startup side effects:
;; `cj/setup-video-download' registers only the org-capture template (lazily,
;; on first capture or first protocol call), while the org-protocol handler is
;; registered separately via `with-eval-after-load'.  These tests pin that
;; separation so setup can't drag in org-protocol plumbing on its own.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Load these for real so `org-capture-templates' and
;; `org-protocol-protocol-alist' are genuine special vars the tests can
;; dynamically rebind (a `let' over a non-special symbol would be lexical
;; and invisible to the module's functions).
(require 'org-capture)
(require 'org-protocol)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'quick-video-capture)

;;; cj/setup-video-download

(ert-deftest test-qvc-setup-registers-capture-template ()
  "Normal: setup registers the \"v\" capture template and flips the flag."
  (let ((org-capture-templates nil)
        (cj/video-download-initialized nil))
    (cl-letf (((symbol-function 'cj/log-silently) #'ignore))
      (cj/setup-video-download)
      (should (assoc "v" org-capture-templates))
      (should cj/video-download-initialized))))

(ert-deftest test-qvc-setup-is-idempotent ()
  "Boundary: a second setup call does not duplicate the capture template."
  (let ((org-capture-templates nil)
        (cj/video-download-initialized nil))
    (cl-letf (((symbol-function 'cj/log-silently) #'ignore))
      (cj/setup-video-download)
      (cj/setup-video-download)
      (should (= 1 (length (seq-filter (lambda (e) (equal (car e) "v"))
                                       org-capture-templates)))))))

(ert-deftest test-qvc-setup-does-not-register-protocol ()
  "Boundary: setup touches only capture state, not the org-protocol alist.
Protocol registration is the job of the `with-eval-after-load' block, so
setup must not add to `org-protocol-protocol-alist' on its own."
  (let ((org-capture-templates nil)
        (org-protocol-protocol-alist nil)
        (cj/video-download-initialized nil))
    (cl-letf (((symbol-function 'cj/log-silently) #'ignore))
      (cj/setup-video-download)
      (should-not (assoc "video-download" org-protocol-protocol-alist)))))

(provide 'test-quick-video-capture--init)
;;; test-quick-video-capture--init.el ends here
