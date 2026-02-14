;;; test-org-reveal-config-title-to-filename.el --- Tests for cj/--reveal-title-to-filename -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--reveal-title-to-filename function from org-reveal-config.el
;;
;; This function takes a presentation title string, downcases it, replaces
;; whitespace runs with hyphens, and appends ".org".  It is a pure string
;; function with no external dependencies — zero mocking required.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub ox-reveal dependency (not available in batch mode)
(provide 'ox-reveal)

(require 'org-reveal-config)

;;; Normal Cases

(ert-deftest test-org-reveal-config-title-to-filename-normal-simple-title ()
  "Simple title should become lowercase-hyphenated.org."
  (should (equal "my-first-talk.org"
                 (cj/--reveal-title-to-filename "My First Talk"))))

(ert-deftest test-org-reveal-config-title-to-filename-normal-two-words ()
  "Two-word title should produce single hyphen."
  (should (equal "hello-world.org"
                 (cj/--reveal-title-to-filename "Hello World"))))

(ert-deftest test-org-reveal-config-title-to-filename-normal-already-lowercase ()
  "Already lowercase title should be unchanged except for extension."
  (should (equal "demo-talk.org"
                 (cj/--reveal-title-to-filename "demo talk"))))

(ert-deftest test-org-reveal-config-title-to-filename-normal-single-word ()
  "Single word title should just get .org appended."
  (should (equal "overview.org"
                 (cj/--reveal-title-to-filename "Overview"))))

(ert-deftest test-org-reveal-config-title-to-filename-normal-always-org-extension ()
  "Result should always end with .org."
  (should (string-suffix-p ".org"
                           (cj/--reveal-title-to-filename "Anything"))))

;;; Boundary Cases

(ert-deftest test-org-reveal-config-title-to-filename-boundary-multiple-spaces ()
  "Multiple consecutive spaces should collapse to single hyphen."
  (should (equal "foo-bar.org"
                 (cj/--reveal-title-to-filename "foo   bar"))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-tabs ()
  "Tabs should be treated as whitespace and replaced."
  (should (equal "tab-separated.org"
                 (cj/--reveal-title-to-filename "tab\tseparated"))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-mixed-whitespace ()
  "Mixed spaces and tabs should collapse to single hyphen."
  (should (equal "mixed-ws.org"
                 (cj/--reveal-title-to-filename "mixed \t ws"))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-leading-trailing-spaces ()
  "Leading and trailing spaces become leading/trailing hyphens."
  (let ((result (cj/--reveal-title-to-filename " padded ")))
    (should (equal "-padded-.org" result))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-unicode-title ()
  "Unicode characters should be preserved (only whitespace replaced)."
  (should (equal "日本語-talk.org"
                 (cj/--reveal-title-to-filename "日本語 Talk"))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-numbers-in-title ()
  "Numbers should be preserved in slug."
  (should (equal "q4-2026-results.org"
                 (cj/--reveal-title-to-filename "Q4 2026 Results"))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-special-chars-preserved ()
  "Non-whitespace special characters should be preserved (not stripped)."
  (should (equal "what's-new?.org"
                 (cj/--reveal-title-to-filename "What's New?"))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-very-long-title ()
  "Very long title should still produce valid filename."
  (let* ((long-title (mapconcat #'identity (make-list 20 "word") " "))
         (result (cj/--reveal-title-to-filename long-title)))
    (should (string-suffix-p ".org" result))
    (should-not (string-match-p " " result))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-empty-string ()
  "Empty string should produce just .org."
  (should (equal ".org" (cj/--reveal-title-to-filename ""))))

(ert-deftest test-org-reveal-config-title-to-filename-boundary-newline ()
  "Newlines should be treated as whitespace."
  (should (equal "line-one-line-two.org"
                 (cj/--reveal-title-to-filename "Line One\nLine Two"))))

;;; Error Cases

(ert-deftest test-org-reveal-config-title-to-filename-error-nil-input ()
  "Nil input should signal an error (not crash silently)."
  (should-error (cj/--reveal-title-to-filename nil)))

(provide 'test-org-reveal-config-title-to-filename)
;;; test-org-reveal-config-title-to-filename.el ends here
