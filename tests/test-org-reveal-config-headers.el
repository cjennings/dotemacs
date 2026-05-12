;;; test-org-reveal-config-headers.el --- Tests for org-reveal header helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for detecting and removing reveal.js headers from Org buffers.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub ox-reveal dependency (not available in batch mode)
(provide 'ox-reveal)

(require 'org-reveal-config)

(defmacro test-org-reveal-headers--with-org-buffer (content &rest body)
  "Run BODY in a temporary org-mode buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (org-mode)
     ,@body))

(ert-deftest test-org-reveal-has-header-detects-reveal-header ()
  "Normal: a reveal metadata line marks the buffer as reveal-enabled."
  (test-org-reveal-headers--with-org-buffer "#+REVEAL_THEME: black\n* Slide\n"
    (should (cj/--reveal-has-header-p))))

(ert-deftest test-org-reveal-has-header-ignores-plain-org-header ()
  "Boundary: ordinary Org metadata alone is not an org-reveal header."
  (test-org-reveal-headers--with-org-buffer "#+TITLE: Notes\n* Heading\n"
    (should-not (cj/--reveal-has-header-p))))

(ert-deftest test-org-reveal-remove-headers-removes-generated-metadata ()
  "Normal: remove generated reveal presentation headers."
  (let ((template (cj/--reveal-header-template "My Talk")))
    (test-org-reveal-headers--with-org-buffer (concat template "* Slide 1\nBody\n")
      (should (= 9 (cj/--reveal-remove-headers)))
      (should (equal (buffer-string) "* Slide 1\nBody\n")))))

(ert-deftest test-org-reveal-remove-headers-preserves-body-content ()
  "Normal: non-header body content remains unchanged."
  (test-org-reveal-headers--with-org-buffer
      "#+TITLE: Talk\n#+REVEAL_THEME: black\n* Slide\nSome text\n"
    (cj/--reveal-remove-headers)
    (should (equal (buffer-string) "* Slide\nSome text\n"))))

(ert-deftest test-org-reveal-remove-headers-only-removes-leading-block ()
  "Boundary: later Org metadata remains untouched."
  (test-org-reveal-headers--with-org-buffer
      "#+TITLE: Talk\n#+REVEAL_THEME: black\n* Slide\n#+TITLE: Nested\n"
    (cj/--reveal-remove-headers)
    (should (equal (buffer-string) "* Slide\n#+TITLE: Nested\n"))))

(ert-deftest test-org-reveal-insert-header-errors-when-present ()
  "Regression: inserting headers should not duplicate an existing reveal block."
  (test-org-reveal-headers--with-org-buffer "#+REVEAL_THEME: black\n* Slide\n"
    (should-error (cj/reveal-insert-header) :type 'user-error)))

(provide 'test-org-reveal-config-headers)
;;; test-org-reveal-config-headers.el ends here
