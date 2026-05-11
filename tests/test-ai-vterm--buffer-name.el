;;; test-ai-vterm--buffer-name.el --- Tests for cj/--ai-vterm-buffer-name -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the buffer-name transform.  Given an absolute project
;; directory, the helper returns "agent [<basename>]".  The naming pattern
;; is what the display-buffer-alist rule keys on, so a regression here
;; silently breaks routing to the right side-window.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--buffer-name-normal-project ()
  "Normal: a typical project path yields agent [<basename>]."
  (should (equal (cj/--ai-vterm-buffer-name "/home/cjennings/projects/foo")
                 "agent [foo]")))

(ert-deftest test-ai-vterm--buffer-name-trailing-slash ()
  "Boundary: trailing slash collapses before basename extraction."
  (should (equal (cj/--ai-vterm-buffer-name "/home/cjennings/projects/foo/")
                 "agent [foo]")))

(ert-deftest test-ai-vterm--buffer-name-dot-prefix-dir ()
  "Boundary: dot-prefix dirs (.emacs.d) preserve the dot in the basename."
  (should (equal (cj/--ai-vterm-buffer-name "/home/cjennings/.emacs.d")
                 "agent [.emacs.d]")))

(ert-deftest test-ai-vterm--buffer-name-space-in-basename ()
  "Boundary: a space in the basename round-trips into the buffer name."
  (should (equal (cj/--ai-vterm-buffer-name "/tmp/my work")
                 "agent [my work]")))

(ert-deftest test-ai-vterm--buffer-name-deeply-nested ()
  "Normal: only the last path component is used."
  (should (equal (cj/--ai-vterm-buffer-name "/a/b/c/d/e/leaf")
                 "agent [leaf]")))

(provide 'test-ai-vterm--buffer-name)
;;; test-ai-vterm--buffer-name.el ends here
