;;; test-hugo-config--collect-drafts.el --- Tests for draft collector  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/hugo--collect-drafts. Walks a directory of Org files
;; and returns an alist of (TITLE . FILEPATH) for posts where :draft is t.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'hugo-config)

(defun test-hugo-config--collect-drafts--mkdir ()
  "Return a fresh temp directory for fixture files."
  (make-temp-file "hugo-test-drafts-" t))

(defun test-hugo-config--collect-drafts--write (dir name contents)
  "Write CONTENTS into NAME inside DIR. Return the absolute path."
  (let ((file (expand-file-name name dir)))
    (with-temp-file file (insert contents))
    file))

;;; Normal Cases

(ert-deftest test-hugo-config--collect-drafts-normal-mixed-dir ()
  "Normal: directory with a draft and a published post returns only the draft."
  (let ((dir (test-hugo-config--collect-drafts--mkdir)))
    (unwind-protect
        (progn
          (test-hugo-config--collect-drafts--write
           dir "draft.org"
           "#+title: Unfinished Thought
#+hugo_draft: true

Body.")
          (test-hugo-config--collect-drafts--write
           dir "published.org"
           "#+title: Shipped
#+hugo_draft: false

Body.")
          (let ((result (cj/hugo--collect-drafts dir)))
            (should (= 1 (length result)))
            (should (string= "Unfinished Thought" (car (car result))))))
      (delete-directory dir t))))

;;; Boundary Cases

(ert-deftest test-hugo-config--collect-drafts-boundary-empty-dir ()
  "Boundary: empty directory returns nil."
  (let ((dir (test-hugo-config--collect-drafts--mkdir)))
    (unwind-protect
        (should (null (cj/hugo--collect-drafts dir)))
      (delete-directory dir t))))

(ert-deftest test-hugo-config--collect-drafts-boundary-all-published ()
  "Boundary: directory with only published posts returns nil."
  (let ((dir (test-hugo-config--collect-drafts--mkdir)))
    (unwind-protect
        (progn
          (test-hugo-config--collect-drafts--write
           dir "one.org" "#+title: One\n#+hugo_draft: false\n")
          (test-hugo-config--collect-drafts--write
           dir "two.org" "#+title: Two\n#+hugo_draft: false\n")
          (should (null (cj/hugo--collect-drafts dir))))
      (delete-directory dir t))))

;;; Error Cases

(ert-deftest test-hugo-config--collect-drafts-error-non-hugo-org ()
  "Error: Org files without #+hugo_draft: are not posts; returns nil."
  (let ((dir (test-hugo-config--collect-drafts--mkdir)))
    (unwind-protect
        (progn
          (test-hugo-config--collect-drafts--write
           dir "random.org" "#+title: Not a post\n\nJust an org file.")
          (should (null (cj/hugo--collect-drafts dir))))
      (delete-directory dir t))))

(provide 'test-hugo-config--collect-drafts)
;;; test-hugo-config--collect-drafts.el ends here
