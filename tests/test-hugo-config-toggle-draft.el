;;; test-hugo-config-toggle-draft.el --- Tests for cj/hugo-toggle-draft -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/hugo-toggle-draft function from hugo-config.el
;;
;; This function toggles the #+hugo_draft keyword between "true" and "false"
;; in the current buffer, using regex search and replace-match.
;;
;; We test the buffer manipulation logic directly using temp buffers,
;; mocking only save-buffer to avoid file I/O.

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

(require 'hugo-config)

;;; Test Helpers

(defun test-hugo-toggle-draft-in-buffer (content)
  "Run cj/hugo-toggle-draft on buffer with CONTENT.
Returns the buffer string after toggling.  Mocks save-buffer."
  (with-temp-buffer
    (insert content)
    (cl-letf (((symbol-function 'save-buffer) #'ignore))
      (cj/hugo-toggle-draft))
    (buffer-string)))

(defconst test-hugo-draft-true-post
  "#+hugo_base_dir: ../../
#+hugo_section: log
#+hugo_auto_set_lastmod: t
#+title: Test Post
#+date: 2026-02-14
#+hugo_tags: test
#+hugo_draft: true
#+hugo_custom_front_matter: :description \"A test post.\"

Some content here.
"
  "Sample post with draft set to true.")

(defconst test-hugo-draft-false-post
  "#+hugo_base_dir: ../../
#+hugo_section: log
#+hugo_auto_set_lastmod: t
#+title: Test Post
#+date: 2026-02-14
#+hugo_tags: test
#+hugo_draft: false
#+hugo_custom_front_matter: :description \"A test post.\"

Some content here.
"
  "Sample post with draft set to false.")

;;; Normal Cases

(ert-deftest test-hugo-config-toggle-draft-normal-true-to-false ()
  "Should toggle draft from true to false."
  (let ((result (test-hugo-toggle-draft-in-buffer test-hugo-draft-true-post)))
    (should (string-match-p "#\\+hugo_draft: false" result))
    (should-not (string-match-p "#\\+hugo_draft: true" result))))

(ert-deftest test-hugo-config-toggle-draft-normal-false-to-true ()
  "Should toggle draft from false to true."
  (let ((result (test-hugo-toggle-draft-in-buffer test-hugo-draft-false-post)))
    (should (string-match-p "#\\+hugo_draft: true" result))
    (should-not (string-match-p "#\\+hugo_draft: false" result))))

(ert-deftest test-hugo-config-toggle-draft-normal-preserves-other-content ()
  "Should not modify any other lines in the buffer."
  (let ((result (test-hugo-toggle-draft-in-buffer test-hugo-draft-true-post)))
    (should (string-match-p "#\\+title: Test Post" result))
    (should (string-match-p "#\\+hugo_tags: test" result))
    (should (string-match-p "Some content here\\." result))))

(ert-deftest test-hugo-config-toggle-draft-normal-roundtrip ()
  "Toggling twice should return to original state."
  (let* ((first (test-hugo-toggle-draft-in-buffer test-hugo-draft-true-post))
         (second (with-temp-buffer
                   (insert first)
                   (cl-letf (((symbol-function 'save-buffer) #'ignore))
                     (cj/hugo-toggle-draft))
                   (buffer-string))))
    (should (string= second test-hugo-draft-true-post))))

;;; Boundary Cases

(ert-deftest test-hugo-config-toggle-draft-boundary-extra-spaces ()
  "Should handle extra spaces between keyword and value."
  (let ((result (test-hugo-toggle-draft-in-buffer "#+hugo_draft:   true\n")))
    (should (string-match-p "#\\+hugo_draft:   false" result))))

(ert-deftest test-hugo-config-toggle-draft-boundary-keyword-first-line ()
  "Should work when #+hugo_draft is the first line."
  (let ((result (test-hugo-toggle-draft-in-buffer "#+hugo_draft: true\nContent.")))
    (should (string-match-p "#\\+hugo_draft: false" result))
    (should (string-match-p "Content\\." result))))

(ert-deftest test-hugo-config-toggle-draft-boundary-keyword-last-line ()
  "Should work when #+hugo_draft is the last line."
  (let ((result (test-hugo-toggle-draft-in-buffer "#+title: Test\n#+hugo_draft: false")))
    (should (string-match-p "#\\+hugo_draft: true" result))))

(ert-deftest test-hugo-config-toggle-draft-boundary-only-draft-line ()
  "Should work when buffer contains only the draft keyword."
  (let ((result (test-hugo-toggle-draft-in-buffer "#+hugo_draft: true")))
    (should (string= result "#+hugo_draft: false"))))

(ert-deftest test-hugo-config-toggle-draft-boundary-preserves-point ()
  "Point should be restored to original position after toggle."
  (with-temp-buffer
    (insert test-hugo-draft-true-post)
    (goto-char 50)
    (let ((original-point (point)))
      (cl-letf (((symbol-function 'save-buffer) #'ignore))
        (cj/hugo-toggle-draft))
      (should (= (point) original-point)))))

;;; Error Cases

(ert-deftest test-hugo-config-toggle-draft-error-no-keyword ()
  "Should signal user-error when no #+hugo_draft keyword is present."
  (should-error
   (with-temp-buffer
     (insert "#+title: A Post Without Draft\n#+hugo_tags: test\n")
     (cl-letf (((symbol-function 'save-buffer) #'ignore))
       (cj/hugo-toggle-draft)))
   :type 'user-error))

(ert-deftest test-hugo-config-toggle-draft-error-empty-buffer ()
  "Should signal user-error on empty buffer."
  (should-error
   (with-temp-buffer
     (cl-letf (((symbol-function 'save-buffer) #'ignore))
       (cj/hugo-toggle-draft)))
   :type 'user-error))

(ert-deftest test-hugo-config-toggle-draft-error-invalid-value ()
  "Should signal user-error when draft has unexpected value."
  (should-error
   (with-temp-buffer
     (insert "#+hugo_draft: maybe\n")
     (cl-letf (((symbol-function 'save-buffer) #'ignore))
       (cj/hugo-toggle-draft)))
   :type 'user-error))

(provide 'test-hugo-config-toggle-draft)
;;; test-hugo-config-toggle-draft.el ends here
