;;; test-calibredb-epub-config--bookmark-name.el --- Nov bookmark naming tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the clean "Author, Title" bookmark naming that replaces nov.el's
;; filename-based default.  The name is parsed from the EPUB filename (Calibre's
;; "<Title> - <Author>.epub" convention), restoring colons that Calibre
;; sanitized to underscores and reordering to "Author, Title".

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)

;;; cj/--nov-clean-title

(ert-deftest test-nov-clean-title-passthrough ()
  "Normal: a clean string is returned unchanged."
  (should (equal (cj/--nov-clean-title "Agatha Christie") "Agatha Christie"))
  (should (equal (cj/--nov-clean-title "The A.B.C. Murders") "The A.B.C. Murders")))

(ert-deftest test-nov-clean-title-restores-colon ()
  "Boundary: Calibre's \"_ \" colon substitution is restored to \": \"."
  (should (equal (cj/--nov-clean-title "Frege_ A Guide for the Perplexed")
                 "Frege: A Guide for the Perplexed"))
  (should (equal (cj/--nov-clean-title "The Fool's Progress_ An Honest Novel")
                 "The Fool's Progress: An Honest Novel")))

(ert-deftest test-nov-clean-title-stray-underscore-and-whitespace ()
  "Boundary: a non-colon underscore becomes a space; whitespace collapses."
  (should (equal (cj/--nov-clean-title "a_b") "a b"))
  (should (equal (cj/--nov-clean-title "  x   y  ") "x y")))

(ert-deftest test-nov-clean-title-rejects-blank-and-nonstring ()
  "Error: nil, empty, all-whitespace, or non-string yields nil."
  (should-not (cj/--nov-clean-title nil))
  (should-not (cj/--nov-clean-title ""))
  (should-not (cj/--nov-clean-title "   "))
  (should-not (cj/--nov-clean-title 42)))

;;; cj/--nov-bookmark-name-from-file

(ert-deftest test-nov-bookmark-name-real-examples ()
  "Normal: real Calibre filenames become \"Author, Title\" with colons restored."
  (should (equal (cj/--nov-bookmark-name-from-file
                  "/books/Frege_ A Guide for the Perplexed - Edward Kanterian.epub")
                 "Edward Kanterian, Frege: A Guide for the Perplexed"))
  (should (equal (cj/--nov-bookmark-name-from-file
                  "/books/The A.B.C. Murders - Agatha Christie.epub")
                 "Agatha Christie, The A.B.C. Murders"))
  (should (equal (cj/--nov-bookmark-name-from-file
                  "/books/The Fool's Progress_ An Honest Novel - Edward Abbey.epub")
                 "Edward Abbey, The Fool's Progress: An Honest Novel")))

(ert-deftest test-nov-bookmark-name-splits-on-last-separator ()
  "Boundary: a title containing \" - \" splits on the LAST separator."
  (should (equal (cj/--nov-bookmark-name-from-file "/b/Title - Part Two - Some Author.epub")
                 "Some Author, Title - Part Two")))

(ert-deftest test-nov-bookmark-name-no-separator ()
  "Boundary: a filename with no \" - \" falls back to the cleaned whole name."
  (should (equal (cj/--nov-bookmark-name-from-file "/b/Untitled_ Draft.epub")
                 "Untitled: Draft")))

(ert-deftest test-nov-bookmark-name-nil-and-empty ()
  "Error: nil or empty path yields nil."
  (should-not (cj/--nov-bookmark-name-from-file nil))
  (should-not (cj/--nov-bookmark-name-from-file "")))

;;; cj/--nov-bookmark-rename-record

(ert-deftest test-nov-bookmark-rename-record-replaces-name ()
  "Normal: the record's name is rebuilt from its filename; the alist is kept."
  (let* ((record (cons "The A.B.C. Murders - Agatha Christie.epub"
                       '((filename . "/b/The A.B.C. Murders - Agatha Christie.epub")
                         (index . 0))))
         (out (cj/--nov-bookmark-rename-record record)))
    (should (equal (car out) "Agatha Christie, The A.B.C. Murders"))
    (should (equal (cdr out) (cdr record)))))

(ert-deftest test-nov-bookmark-rename-record-keeps-original-without-filename ()
  "Boundary: a record with no usable filename is returned unchanged."
  (let ((record (cons "whatever" '((index . 0)))))
    (should (equal (cj/--nov-bookmark-rename-record record) record))))

(provide 'test-calibredb-epub-config--bookmark-name)
;;; test-calibredb-epub-config--bookmark-name.el ends here
