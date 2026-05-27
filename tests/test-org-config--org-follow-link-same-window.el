;;; test-org-config--org-follow-link-same-window.el --- same-window link follow -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--org-follow-link-same-window' follows the Org link at point, opening
;; file: links in the *current* window instead of org's default other-window
;; (`org-link-frame-setup' file entry).  It is the kernel behind the S-mouse-1
;; and mouse-3 bindings (`cj/org-follow-link-at-mouse-same-window').  Off a
;; link it does nothing, so a right-click in empty space is a silent no-op
;; rather than org's "No link found" user-error.
;;
;; `org-open-at-point' (the visit boundary) is stubbed to record whether it
;; was called and what the file entry of `org-link-frame-setup' resolved to at
;; call time; real temp Org buffers place point on / off a link.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-config)

(ert-deftest test-org-config-follow-link-same-window-file-link ()
  "Normal: on a file link, follow it with the file frame-setup as find-file."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (frame-file nil)
          (called nil))
      (insert "see [[file:/tmp/notes.org][notes]] here")
      (org-mode)
      (goto-char (point-min))
      (search-forward "notes]")
      (backward-char 2)                 ; land inside the link's description
      (cl-letf (((symbol-function 'org-open-at-point)
                 (lambda (&rest _)
                   (setq called t
                         frame-file (cdr (assq 'file org-link-frame-setup))))))
        (cj/--org-follow-link-same-window))
      (should called)
      (should (eq frame-file 'find-file)))))

(ert-deftest test-org-config-follow-link-same-window-off-link-noop ()
  "Boundary: off any link, do nothing (no error, no visit)."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (called nil))
      (insert "plain text, no link at all")
      (org-mode)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-open-at-point)
                 (lambda (&rest _) (setq called t))))
        (cj/--org-follow-link-same-window))
      (should-not called))))

(ert-deftest test-org-config-follow-link-same-window-http-link ()
  "Boundary: on a non-file link, still follow it (frame-setup is harmless)."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (called nil))
      (insert "site [[https://example.com][example]] end")
      (org-mode)
      (goto-char (point-min))
      (search-forward "example]")
      (backward-char 2)
      (cl-letf (((symbol-function 'org-open-at-point)
                 (lambda (&rest _) (setq called t))))
        (cj/--org-follow-link-same-window))
      (should called))))

(provide 'test-org-config--org-follow-link-same-window)
;;; test-org-config--org-follow-link-same-window.el ends here
