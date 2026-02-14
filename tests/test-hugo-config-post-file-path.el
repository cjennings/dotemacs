;;; test-hugo-config-post-file-path.el --- Tests for cj/hugo--post-file-path -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/hugo--post-file-path function from hugo-config.el
;;
;; This function takes a post title, generates a slug via org-hugo-slug,
;; and returns the full file path under cj/hugo-content-org-dir.
;;
;; We mock org-hugo-slug to isolate our path construction logic from
;; the ox-hugo package (external dependency).

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(unless (boundp 'website-dir)
  (defvar website-dir "/tmp/test-website/"))
(unless (fboundp 'env-macos-p)
  (defun env-macos-p () nil))
(unless (fboundp 'env-windows-p)
  (defun env-windows-p () nil))
(unless (fboundp 'org-hugo-slug)
  (defun org-hugo-slug (title) title))

;; Stub ox-hugo require since MELPA packages aren't available in batch
(provide 'ox-hugo)

(require 'hugo-config)

;;; Normal Cases

(ert-deftest test-hugo-config-post-file-path-normal-simple-title ()
  "Should construct path from slugified title."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "my-first-post")))
    (let ((result (cj/hugo--post-file-path "My First Post")))
      (should (string-suffix-p "my-first-post.org" result)))))

(ert-deftest test-hugo-config-post-file-path-normal-under-content-org-dir ()
  "Should place the file under cj/hugo-content-org-dir."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "test-post")))
    (let ((result (cj/hugo--post-file-path "Test Post")))
      (should (string-prefix-p (expand-file-name cj/hugo-content-org-dir) result)))))

(ert-deftest test-hugo-config-post-file-path-normal-org-extension ()
  "Should always produce a .org file."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "any-slug")))
    (let ((result (cj/hugo--post-file-path "Any Title")))
      (should (string-suffix-p ".org" result)))))

(ert-deftest test-hugo-config-post-file-path-normal-different-titles ()
  "Different titles should produce different file paths."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title)
               (if (string= title "Alpha") "alpha" "beta"))))
    (let ((path-a (cj/hugo--post-file-path "Alpha"))
          (path-b (cj/hugo--post-file-path "Beta")))
      (should-not (string= path-a path-b)))))

;;; Boundary Cases

(ert-deftest test-hugo-config-post-file-path-boundary-title-with-special-chars ()
  "Should handle titles with special characters via slug."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "whats-new-in-2026")))
    (let ((result (cj/hugo--post-file-path "What's New in 2026?!")))
      (should (string-suffix-p "whats-new-in-2026.org" result)))))

(ert-deftest test-hugo-config-post-file-path-boundary-single-word-title ()
  "Should handle single word title."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "hello")))
    (let ((result (cj/hugo--post-file-path "Hello")))
      (should (string-suffix-p "hello.org" result)))))

(ert-deftest test-hugo-config-post-file-path-boundary-very-long-title ()
  "Should handle a very long title."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "this-is-a-very-long-title-that-goes-on-and-on")))
    (let ((result (cj/hugo--post-file-path "This Is A Very Long Title That Goes On And On")))
      (should (string-suffix-p "this-is-a-very-long-title-that-goes-on-and-on.org" result)))))

;;; Error Cases

(ert-deftest test-hugo-config-post-file-path-error-empty-title ()
  "Empty title should still produce a valid path (slug handles it)."
  (cl-letf (((symbol-function 'org-hugo-slug)
             (lambda (title) "")))
    (let ((result (cj/hugo--post-file-path "")))
      (should (string-suffix-p ".org" result)))))

(provide 'test-hugo-config-post-file-path)
;;; test-hugo-config-post-file-path.el ends here
