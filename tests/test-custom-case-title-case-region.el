;;; test-custom-case-title-case-region.el --- Tests for cj/title-case-region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/title-case-region function from custom-case.el.
;;
;; This function title-cases text in the active region (or current line if no
;; region). Major words are capitalized; minor words (a, an, and, as, at, but,
;; by, for, if, in, is, nor, of, on, or, so, the, to, yet) are lowercased
;; except: at the start of the text, or after a skip-reset character (: ! ?).
;; Characters directly after separators (- \ ' .) are NOT capitalized.
;; The region is downcased first, so all-caps input is handled.

;;; Code:

(require 'ert)

;; Ensure custom-case is loadable without keybinding dependencies
(unless (boundp 'cj/custom-keymap)
  (defvar cj/custom-keymap (make-sparse-keymap)))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-case)

;; Helper: apply title case to STRING via region and return result
(defun test-title-case--on-string (string)
  "Apply cj/title-case-region to STRING and return the result."
  (with-temp-buffer
    (insert string)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (cj/title-case-region)
    (buffer-string)))

;; Helper: apply title case to current line (no region) and return result
(defun test-title-case--on-line (string)
  "Insert STRING, place point on the line, call cj/title-case-region without
an active region, and return the result."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (cj/title-case-region)
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-custom-case-title-case-region-normal-simple-sentence ()
  "Simple sentence should capitalize major words."
  (should (equal (test-title-case--on-string "the quick brown fox")
                 "The Quick Brown Fox")))

(ert-deftest test-custom-case-title-case-region-normal-minor-words-lowercased ()
  "Minor words (articles, short prepositions, conjunctions) should be lowercase."
  (should (equal (test-title-case--on-string "war and peace in the modern age")
                 "War and Peace in the Modern Age")))

(ert-deftest test-custom-case-title-case-region-normal-first-word-always-capitalized ()
  "First word should always be capitalized, even if it's a minor word."
  (should (equal (test-title-case--on-string "the art of war")
                 "The Art of War")))

(ert-deftest test-custom-case-title-case-region-normal-all-minor-words ()
  "All minor words in the skip list should be lowercased in mid-sentence."
  (should (equal (test-title-case--on-string
                  "go a an and as at but by for if in is nor of on or so the to yet go")
                 "Go a an and as at but by for if in is nor of on or so the to yet Go")))

(ert-deftest test-custom-case-title-case-region-normal-four-letter-words-capitalized ()
  "Words of four or more letters should always be capitalized.
Note: 'is' is explicitly in the minor word list, so it stays lowercase."
  (should (equal (test-title-case--on-string "this is from that with over")
                 "This is From That With Over")))

(ert-deftest test-custom-case-title-case-region-normal-allcaps-input ()
  "All-caps input should be downcased first, then title-cased."
  (should (equal (test-title-case--on-string "THE QUICK BROWN FOX")
                 "The Quick Brown Fox")))

(ert-deftest test-custom-case-title-case-region-normal-mixed-case-input ()
  "Mixed case input should be normalized to title case."
  (should (equal (test-title-case--on-string "tHe qUiCk BrOwN fOx")
                 "The Quick Brown Fox")))

(ert-deftest test-custom-case-title-case-region-normal-colon-resets-capitalization ()
  "Words after a colon should be capitalized, even minor words."
  (should (equal (test-title-case--on-string "warning: an important message")
                 "Warning: An Important Message")))

(ert-deftest test-custom-case-title-case-region-normal-exclamation-resets ()
  "Words after an exclamation mark should be capitalized."
  (should (equal (test-title-case--on-string "wow! the crowd goes wild")
                 "Wow! The Crowd Goes Wild")))

(ert-deftest test-custom-case-title-case-region-normal-question-resets ()
  "Word immediately after a question mark should be capitalized, even if minor."
  (should (equal (test-title-case--on-string "really? the answer is no")
                 "Really? The Answer is No")))

(ert-deftest test-custom-case-title-case-region-normal-hyphenated-word ()
  "Second part of a hyphenated word should NOT be capitalized."
  (should (equal (test-title-case--on-string "a well-known fact")
                 "A Well-known Fact")))

(ert-deftest test-custom-case-title-case-region-normal-apostrophe ()
  "Letters after an apostrophe should NOT be capitalized."
  (should (equal (test-title-case--on-string "it's a wonderful life")
                 "It's a Wonderful Life")))

(ert-deftest test-custom-case-title-case-region-normal-no-region-uses-line ()
  "Without an active region, should title-case the current line."
  (should (equal (test-title-case--on-line "the quick brown fox")
                 "The Quick Brown Fox")))

;;; Boundary Cases

(ert-deftest test-custom-case-title-case-region-boundary-single-word ()
  "Single word should be capitalized."
  (should (equal (test-title-case--on-string "hello") "Hello")))

(ert-deftest test-custom-case-title-case-region-boundary-single-minor-word ()
  "Single minor word should still be capitalized (it's the first word)."
  (should (equal (test-title-case--on-string "the") "The")))

(ert-deftest test-custom-case-title-case-region-boundary-single-character ()
  "Single character should be capitalized."
  (should (equal (test-title-case--on-string "a") "A")))

(ert-deftest test-custom-case-title-case-region-boundary-empty-string ()
  "Empty string should remain empty."
  (should (equal (test-title-case--on-string "") "")))

(ert-deftest test-custom-case-title-case-region-boundary-only-whitespace ()
  "Whitespace-only string should remain unchanged."
  (should (equal (test-title-case--on-string "   ") "   ")))

(ert-deftest test-custom-case-title-case-region-boundary-multiple-spaces ()
  "Multiple spaces between words should be preserved."
  (should (equal (test-title-case--on-string "the   quick   fox")
                 "The   Quick   Fox")))

(ert-deftest test-custom-case-title-case-region-boundary-unicode-words ()
  "Unicode characters should pass through without error."
  (should (equal (test-title-case--on-string "the café is nice")
                 "The Café is Nice")))

(ert-deftest test-custom-case-title-case-region-boundary-numbers-in-text ()
  "Numbers mixed with text should not break title casing."
  (should (equal (test-title-case--on-string "chapter 3 of the book")
                 "Chapter 3 of the Book")))

(ert-deftest test-custom-case-title-case-region-boundary-backslash-separator ()
  "Backslash should act as separator, preventing capitalization after it."
  (should (equal (test-title-case--on-string "foo\\bar baz")
                 "Foo\\bar Baz")))

(ert-deftest test-custom-case-title-case-region-boundary-period-separator ()
  "Period should act as separator, preventing capitalization after it."
  (should (equal (test-title-case--on-string "foo.bar baz")
                 "Foo.bar Baz")))

(ert-deftest test-custom-case-title-case-region-boundary-multiple-colons ()
  "Multiple colons should each reset capitalization."
  (should (equal (test-title-case--on-string "part: the first: an overview")
                 "Part: The First: An Overview")))

(ert-deftest test-custom-case-title-case-region-boundary-colon-with-minor-word ()
  "Minor word immediately after colon should be capitalized."
  (should (equal (test-title-case--on-string "note: a brief summary")
                 "Note: A Brief Summary")))

(ert-deftest test-custom-case-title-case-region-boundary-partial-region ()
  "Only the selected region should be affected."
  (with-temp-buffer
    (insert "the quick brown fox jumps over")
    ;; Select only "quick brown fox"
    (set-mark 5)
    (goto-char 20)
    (activate-mark)
    (cj/title-case-region)
    (should (equal (buffer-string) "the Quick Brown Fox jumps over"))))

(ert-deftest test-custom-case-title-case-region-boundary-long-title ()
  "Long title with many minor words should be handled correctly."
  (should (equal (test-title-case--on-string
                  "the lord of the rings: the return of the king")
                 "The Lord of the Rings: The Return of the King")))

;;; Error Cases

(ert-deftest test-custom-case-title-case-region-error-numeric-only ()
  "String of only numbers should not error."
  (should (equal (test-title-case--on-string "12345") "12345")))

(ert-deftest test-custom-case-title-case-region-error-punctuation-only ()
  "String of only punctuation should not error."
  (should (equal (test-title-case--on-string "!@#$%") "!@#$%")))

(provide 'test-custom-case-title-case-region)
;;; test-custom-case-title-case-region.el ends here
