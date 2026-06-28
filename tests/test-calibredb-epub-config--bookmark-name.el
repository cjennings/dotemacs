;;; test-calibredb-epub-config--bookmark-name.el --- Reading bookmark naming tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the clean "Author, Title" bookmark naming that replaces the
;; filename-based default for both EPUB (nov) and PDF (pdf-view) bookmarks.
;; The name is parsed from the file's name (Calibre's "<Title> - <Author>.<ext>"
;; convention), restoring colons that Calibre sanitized to underscores and
;; reordering to "Author, Title".  The parser is extension-agnostic, so the
;; same advice serves both nov-bookmark-make-record and
;; pdf-view-bookmark-make-record.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)

;;; cj/--reading-clean-title

(ert-deftest test-reading-clean-title-passthrough ()
  "Normal: a clean string is returned unchanged."
  (should (equal (cj/--reading-clean-title "Agatha Christie") "Agatha Christie"))
  (should (equal (cj/--reading-clean-title "The A.B.C. Murders") "The A.B.C. Murders")))

(ert-deftest test-reading-clean-title-restores-colon ()
  "Boundary: Calibre's \"_ \" colon substitution is restored to \": \"."
  (should (equal (cj/--reading-clean-title "Frege_ A Guide for the Perplexed")
                 "Frege: A Guide for the Perplexed"))
  (should (equal (cj/--reading-clean-title "The Fool's Progress_ An Honest Novel")
                 "The Fool's Progress: An Honest Novel")))

(ert-deftest test-reading-clean-title-stray-underscore-and-whitespace ()
  "Boundary: a non-colon underscore becomes a space; whitespace collapses."
  (should (equal (cj/--reading-clean-title "a_b") "a b"))
  (should (equal (cj/--reading-clean-title "  x   y  ") "x y")))

(ert-deftest test-reading-clean-title-rejects-blank-and-nonstring ()
  "Error: nil, empty, all-whitespace, or non-string yields nil."
  (should-not (cj/--reading-clean-title nil))
  (should-not (cj/--reading-clean-title ""))
  (should-not (cj/--reading-clean-title "   "))
  (should-not (cj/--reading-clean-title 42)))

;;; cj/--reading-bookmark-name-from-file

(ert-deftest test-reading-bookmark-name-real-examples ()
  "Normal: real Calibre filenames become \"Author, Title\" with colons restored."
  (should (equal (cj/--reading-bookmark-name-from-file
                  "/books/Frege_ A Guide for the Perplexed - Edward Kanterian.epub")
                 "Edward Kanterian, Frege: A Guide for the Perplexed"))
  (should (equal (cj/--reading-bookmark-name-from-file
                  "/books/The A.B.C. Murders - Agatha Christie.epub")
                 "Agatha Christie, The A.B.C. Murders"))
  (should (equal (cj/--reading-bookmark-name-from-file
                  "/books/The Fool's Progress_ An Honest Novel - Edward Abbey.epub")
                 "Edward Abbey, The Fool's Progress: An Honest Novel")))

(ert-deftest test-reading-bookmark-name-pdf-extension ()
  "Normal: a PDF filename is parsed the same way -- the parser is extension-
agnostic, so PDF bookmarks reformat like EPUB ones."
  (should (equal (cj/--reading-bookmark-name-from-file
                  "/books/Engines of Logic_ Mathematicians and the O - Martin Davis.pdf")
                 "Martin Davis, Engines of Logic: Mathematicians and the O")))

(ert-deftest test-reading-bookmark-name-splits-on-last-separator ()
  "Boundary: a title containing \" - \" splits on the LAST separator."
  (should (equal (cj/--reading-bookmark-name-from-file "/b/Title - Part Two - Some Author.epub")
                 "Some Author, Title - Part Two")))

(ert-deftest test-reading-bookmark-name-no-separator ()
  "Boundary: a filename with no \" - \" falls back to the cleaned whole name."
  (should (equal (cj/--reading-bookmark-name-from-file "/b/Untitled_ Draft.epub")
                 "Untitled: Draft")))

(ert-deftest test-reading-bookmark-name-nil-and-empty ()
  "Error: nil or empty path yields nil."
  (should-not (cj/--reading-bookmark-name-from-file nil))
  (should-not (cj/--reading-bookmark-name-from-file "")))

;;; cj/--reading-bookmark-rename-record

(ert-deftest test-reading-bookmark-rename-record-replaces-name ()
  "Normal: the record's name is rebuilt from its filename; the alist is kept."
  (let* ((record (cons "The A.B.C. Murders - Agatha Christie.epub"
                       '((filename . "/b/The A.B.C. Murders - Agatha Christie.epub")
                         (index . 0))))
         (out (cj/--reading-bookmark-rename-record record)))
    (should (equal (car out) "Agatha Christie, The A.B.C. Murders"))
    (should (equal (cdr out) (cdr record)))))

(ert-deftest test-reading-bookmark-rename-record-pdf ()
  "Normal: a PDF-shaped record (filename from `bookmark-make-record-default')
gets the same \"Author, Title\" rename."
  (let* ((record (cons "Engines of Logic_ Mathematicians and the O - Martin Davis.pdf"
                       '((filename . "/b/Engines of Logic_ Mathematicians and the O - Martin Davis.pdf")
                         (page . 12))))
         (out (cj/--reading-bookmark-rename-record record)))
    (should (equal (car out) "Martin Davis, Engines of Logic: Mathematicians and the O"))
    (should (equal (cdr out) (cdr record)))))

(ert-deftest test-reading-bookmark-rename-record-keeps-original-without-filename ()
  "Boundary: a record with no usable filename is returned unchanged."
  (let ((record (cons "whatever" '((index . 0)))))
    (should (equal (cj/--reading-bookmark-rename-record record) record))))

(provide 'test-calibredb-epub-config--bookmark-name)
;;; test-calibredb-epub-config--bookmark-name.el ends here
