;;; test-face-docs-dump.el --- ERT tests for face-docs-dump.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests the pure docstring-extraction helper (`face-docs--first-line') and the
;; syntax-category resolution (`face-docs--syntax-map') in face-docs-dump.el, the
;; asset generator behind theme-studio's element hovers.  The faces map is a
;; thin `face-list' walk over `face-docs--first-line', so testing the helper and
;; the syntax resolution covers the logic that can actually be wrong.
;;
;; Self-locating: loads face-docs-dump.el and build-theme.el (for the
;; category->face map) from this file's own directory, so the runner only needs
;; to `-l' this file.

;;; Code:

(require 'ert)

(let ((dir (file-name-directory
            (or load-file-name buffer-file-name default-directory))))
  (load (expand-file-name "face-docs-dump.el" dir) nil t)
  (load (expand-file-name "build-theme.el" dir) nil t))

;;; --- face-docs--first-line ---

(ert-deftest test-face-docs-first-line-normal-multiline ()
  "Normal: returns the first line of a multi-line docstring."
  (should (equal (face-docs--first-line "First line.\nSecond line.")
                 "First line.")))

(ert-deftest test-face-docs-first-line-single-line ()
  "Normal: a single-line docstring returns unchanged."
  (should (equal (face-docs--first-line "Just one.") "Just one.")))

(ert-deftest test-face-docs-first-line-skips-leading-blank-lines ()
  "Boundary: leading blank lines are skipped to the first real line, trimmed."
  (should (equal (face-docs--first-line "\n\n  Real line.\nrest")
                 "Real line.")))

(ert-deftest test-face-docs-first-line-collapses-internal-whitespace ()
  "Boundary: runs of spaces and tabs inside the line collapse to one space."
  (should (equal (face-docs--first-line "A  B\tC") "A B C")))

(ert-deftest test-face-docs-first-line-empty-is-nil ()
  "Boundary: an empty string yields nil."
  (should (null (face-docs--first-line ""))))

(ert-deftest test-face-docs-first-line-whitespace-only-is-nil ()
  "Boundary: a blank/whitespace-only docstring yields nil."
  (should (null (face-docs--first-line "   \n\t ")))
  (should (null (face-docs--first-line "   "))))

(ert-deftest test-face-docs-first-line-non-string-is-nil ()
  "Error: nil, a number, or the :null sentinel yields nil."
  (should (null (face-docs--first-line nil)))
  (should (null (face-docs--first-line 42)))
  (should (null (face-docs--first-line :null))))

;;; --- face-docs--syntax-map ---

(ert-deftest test-face-docs-syntax-map-resolves-category-to-face-doc ()
  "Normal: kw resolves to font-lock-keyword-face's first docstring line."
  (let ((m (face-docs--syntax-map)))
    (should (stringp (gethash "kw" m)))
    (should (string-match-p "keyword" (downcase (gethash "kw" m))))))

(ert-deftest test-face-docs-syntax-map-bg-and-p-are-default ()
  "Boundary: bg and p resolve to the default face's docstring."
  (let ((m (face-docs--syntax-map))
        (def (face-docs--first-line (face-documentation 'default))))
    (should (equal (gethash "bg" m) def))
    (should (equal (gethash "p" m) def))))

(ert-deftest test-face-docs-syntax-map-omits-faceless-category ()
  "Boundary: dec (Emacs has no dedicated decorator face) is absent."
  (should (null (gethash "dec" (face-docs--syntax-map)))))

(provide 'test-face-docs-dump)
;;; test-face-docs-dump.el ends here
