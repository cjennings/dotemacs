;;; test-hugo-config--post-metadata.el --- Tests for hugo post metadata parser  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/hugo--post-metadata. Reads the Hugo front matter region of
;; an Org file and returns (:title TITLE :draft BOOL) or nil for non-Hugo posts.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'hugo-config)

(defun test-hugo-config--post-metadata--write-fixture (contents)
  "Write CONTENTS to a unique temp .org file and return its path."
  (let ((file (make-temp-file "hugo-test-" nil ".org")))
    (with-temp-file file (insert contents))
    file))

;;; Normal Cases

(ert-deftest test-hugo-config--post-metadata-normal-draft-with-title ()
  "Normal: draft post with title returns plist with title and :draft t."
  (let ((file (test-hugo-config--post-metadata--write-fixture
               "#+title: My First Post
#+date: 2026-01-01
#+hugo_draft: true

Body text.")))
    (unwind-protect
        (let ((meta (cj/hugo--post-metadata file)))
          (should (equal (plist-get meta :title) "My First Post"))
          (should (eq (plist-get meta :draft) t)))
      (delete-file file))))

;;; Boundary Cases

(ert-deftest test-hugo-config--post-metadata-boundary-published-no-title ()
  "Boundary: published post without #+title: falls back to file basename."
  (let ((file (test-hugo-config--post-metadata--write-fixture
               "#+hugo_draft: false

Body only.")))
    (unwind-protect
        (let ((meta (cj/hugo--post-metadata file)))
          (should (stringp (plist-get meta :title)))
          (should (string= (plist-get meta :title)
                           (file-name-base file)))
          (should (eq (plist-get meta :draft) nil)))
      (delete-file file))))

;;; Error Cases

(ert-deftest test-hugo-config--post-metadata-error-missing-hugo-draft ()
  "Error: Org file without #+hugo_draft: keyword is not a Hugo post; returns nil."
  (let ((file (test-hugo-config--post-metadata--write-fixture
               "#+title: Just an Org file

No Hugo front matter here.")))
    (unwind-protect
        (should (null (cj/hugo--post-metadata file)))
      (delete-file file))))

(provide 'test-hugo-config--post-metadata)
;;; test-hugo-config--post-metadata.el ends here
