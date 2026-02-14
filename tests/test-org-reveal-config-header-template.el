;;; test-org-reveal-config-header-template.el --- Tests for cj/--reveal-header-template -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--reveal-header-template function from org-reveal-config.el
;;
;; This function takes a title string and returns a complete #+REVEAL_ header
;; block for an org-reveal presentation.  It uses user-full-name and
;; format-time-string internally, so we mock those for deterministic output.
;; The reveal.js constants (root, theme, transition) are tested via their
;; presence in the output.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub ox-reveal dependency (not available in batch mode)
(provide 'ox-reveal)

(require 'org-reveal-config)

;; Helper to call template with deterministic date and author
(defun test-reveal--header (title)
  "Call cj/--reveal-header-template with TITLE, mocking time and user."
  (cl-letf (((symbol-function 'user-full-name) (lambda () "Test Author"))
            ((symbol-function 'format-time-string)
             (lambda (_fmt) "2026-02-14")))
    (cj/--reveal-header-template title)))

;;; Normal Cases

(ert-deftest test-org-reveal-config-header-template-normal-contains-title ()
  "Output should contain #+TITLE: with the given title."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+TITLE: My Talk$" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-author ()
  "Output should contain #+AUTHOR: with the user's full name."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+AUTHOR: Test Author$" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-date ()
  "Output should contain #+DATE: with today's date."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+DATE: 2026-02-14$" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-reveal-root ()
  "Output should contain #+REVEAL_ROOT: with file:// URL."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+REVEAL_ROOT: file://" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-theme ()
  "Output should contain #+REVEAL_THEME: with the default theme."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p
             (format "^#\\+REVEAL_THEME: %s$" cj/reveal-default-theme)
             result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-transition ()
  "Output should contain #+REVEAL_TRANS: with the default transition."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p
             (format "^#\\+REVEAL_TRANS: %s$" cj/reveal-default-transition)
             result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-init-options ()
  "Output should contain #+REVEAL_INIT_OPTIONS: with slideNumber and hash."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+REVEAL_INIT_OPTIONS:.*slideNumber" result))
    (should (string-match-p "^#\\+REVEAL_INIT_OPTIONS:.*hash" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-plugins ()
  "Output should contain #+REVEAL_PLUGINS: listing all plugins."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+REVEAL_PLUGINS:.*highlight" result))
    (should (string-match-p "^#\\+REVEAL_PLUGINS:.*notes" result))
    (should (string-match-p "^#\\+REVEAL_PLUGINS:.*search" result))
    (should (string-match-p "^#\\+REVEAL_PLUGINS:.*zoom" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-highlight-css ()
  "Output should contain #+REVEAL_HIGHLIGHT_CSS: with monokai."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+REVEAL_HIGHLIGHT_CSS:.*monokai" result))))

(ert-deftest test-org-reveal-config-header-template-normal-contains-options ()
  "Output should contain #+OPTIONS: disabling toc and num."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-match-p "^#\\+OPTIONS: toc:nil num:nil$" result))))

(ert-deftest test-org-reveal-config-header-template-normal-ends-with-blank-line ()
  "Output should end with a trailing newline (blank line separator)."
  (let ((result (test-reveal--header "My Talk")))
    (should (string-suffix-p "\n\n" result))))

(ert-deftest test-org-reveal-config-header-template-normal-all-keywords-present ()
  "All required org keywords should be present in the output."
  (let ((result (test-reveal--header "My Talk"))
        (keywords '("#+TITLE:" "#+AUTHOR:" "#+DATE:"
                    "#+REVEAL_ROOT:" "#+REVEAL_THEME:" "#+REVEAL_TRANS:"
                    "#+REVEAL_INIT_OPTIONS:" "#+REVEAL_PLUGINS:"
                    "#+REVEAL_HIGHLIGHT_CSS:" "#+OPTIONS:")))
    (dolist (kw keywords)
      (should (string-match-p (regexp-quote kw) result)))))

;;; Boundary Cases

(ert-deftest test-org-reveal-config-header-template-boundary-empty-title ()
  "Empty title should produce valid header with empty #+TITLE."
  (let ((result (test-reveal--header "")))
    (should (string-match-p "^#\\+TITLE: $" result))
    (should (string-match-p "#\\+REVEAL_THEME:" result))))

(ert-deftest test-org-reveal-config-header-template-boundary-unicode-title ()
  "Unicode title should be preserved in #+TITLE."
  (let ((result (test-reveal--header "日本語プレゼン")))
    (should (string-match-p "^#\\+TITLE: 日本語プレゼン$" result))))

(ert-deftest test-org-reveal-config-header-template-boundary-title-with-special-chars ()
  "Special characters in title should not break the template."
  (let ((result (test-reveal--header "What's New? (2026 Edition)")))
    (should (string-match-p "^#\\+TITLE: What's New\\? (2026 Edition)$" result))))

(ert-deftest test-org-reveal-config-header-template-boundary-title-with-percent ()
  "Percent signs in title should not break format string."
  (let ((result (test-reveal--header "100% Complete")))
    (should (string-match-p "^#\\+TITLE: 100% Complete$" result))))

(ert-deftest test-org-reveal-config-header-template-boundary-very-long-title ()
  "Very long title should produce valid output."
  (let* ((long-title (make-string 200 ?x))
         (result (test-reveal--header long-title)))
    (should (string-match-p "#\\+TITLE:" result))
    (should (string-match-p "#\\+REVEAL_THEME:" result))))

;;; Error Cases

(ert-deftest test-org-reveal-config-header-template-error-nil-title ()
  "Nil title should signal an error rather than silently producing garbage."
  (should-error (test-reveal--header nil) :type 'user-error))

(provide 'test-org-reveal-config-header-template)
;;; test-org-reveal-config-header-template.el ends here
