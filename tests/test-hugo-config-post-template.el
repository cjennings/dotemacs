;;; test-hugo-config-post-template.el --- Tests for cj/hugo--post-template -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/hugo--post-template function from hugo-config.el
;;
;; This function generates the Org front matter template for a Hugo post.
;; It takes a title and date, and returns the complete template string
;; with all required Hugo keywords.

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

;;; Normal Cases

(ert-deftest test-hugo-config-post-template-normal-contains-title ()
  "Template should contain the provided title."
  (let ((result (cj/hugo--post-template "My First Post" "2026-02-14")))
    (should (string-match-p "#\\+title: My First Post" result))))

(ert-deftest test-hugo-config-post-template-normal-contains-date ()
  "Template should contain the provided date."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+date: 2026-02-14" result))))

(ert-deftest test-hugo-config-post-template-normal-contains-base-dir ()
  "Template should contain hugo_base_dir pointing to site root."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+hugo_base_dir: \\.\\./\\.\\." result))))

(ert-deftest test-hugo-config-post-template-normal-contains-section ()
  "Template should set hugo_section to log."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+hugo_section: log" result))))

(ert-deftest test-hugo-config-post-template-normal-draft-true ()
  "New posts should default to draft: true."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+hugo_draft: true" result))))

(ert-deftest test-hugo-config-post-template-normal-has-lastmod ()
  "Template should enable auto-set lastmod."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+hugo_auto_set_lastmod: t" result))))

(ert-deftest test-hugo-config-post-template-normal-has-tags-placeholder ()
  "Template should include empty hugo_tags keyword."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+hugo_tags:" result))))

(ert-deftest test-hugo-config-post-template-normal-has-description ()
  "Template should include description custom front matter."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-match-p "#\\+hugo_custom_front_matter: :description" result))))

(ert-deftest test-hugo-config-post-template-normal-ends-with-blank-line ()
  "Template should end with a blank line for content insertion."
  (let ((result (cj/hugo--post-template "Test" "2026-02-14")))
    (should (string-suffix-p "\n\n" result))))

;;; Boundary Cases

(ert-deftest test-hugo-config-post-template-boundary-title-with-quotes ()
  "Title containing quotes should be inserted literally."
  (let ((result (cj/hugo--post-template "It's a \"Test\"" "2026-02-14")))
    (should (string-match-p "#\\+title: It's a \"Test\"" result))))

(ert-deftest test-hugo-config-post-template-boundary-title-with-colons ()
  "Title with colons should be inserted literally."
  (let ((result (cj/hugo--post-template "Part 1: The Beginning" "2026-02-14")))
    (should (string-match-p "#\\+title: Part 1: The Beginning" result))))

(ert-deftest test-hugo-config-post-template-boundary-empty-title ()
  "Empty title should produce valid template with empty title line."
  (let ((result (cj/hugo--post-template "" "2026-02-14")))
    (should (string-match-p "#\\+title: \n" result))
    (should (string-match-p "#\\+date: 2026-02-14" result))))

(ert-deftest test-hugo-config-post-template-boundary-keyword-order ()
  "Keywords should appear in the expected order."
  (let* ((result (cj/hugo--post-template "Test" "2026-02-14"))
         (pos-base (string-match "#\\+hugo_base_dir" result))
         (pos-section (string-match "#\\+hugo_section" result))
         (pos-title (string-match "#\\+title" result))
         (pos-date (string-match "#\\+date" result))
         (pos-draft (string-match "#\\+hugo_draft" result)))
    (should (< pos-base pos-section))
    (should (< pos-section pos-title))
    (should (< pos-title pos-date))
    (should (< pos-date pos-draft))))

;;; Error Cases

(ert-deftest test-hugo-config-post-template-error-nil-title ()
  "Nil title should produce template with 'nil' string (format behavior)."
  (let ((result (cj/hugo--post-template nil "2026-02-14")))
    (should (stringp result))
    (should (string-match-p "#\\+date: 2026-02-14" result))))

(ert-deftest test-hugo-config-post-template-error-nil-date ()
  "Nil date should produce template with 'nil' string (format behavior)."
  (let ((result (cj/hugo--post-template "Test" nil)))
    (should (stringp result))
    (should (string-match-p "#\\+title: Test" result))))

(provide 'test-hugo-config-post-template)
;;; test-hugo-config-post-template.el ends here
