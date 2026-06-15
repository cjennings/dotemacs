;;; test-org-config-table-header.el --- In-buffer org table header fontify -*- lexical-binding: t; -*-

;;; Commentary:
;; Org has no in-buffer header-row face -- the whole table uses `org-table'.
;; cj/--org-table-header-row-p, cj/--org-table-first-hline-position, and the
;; font-lock matcher cj/--org-fontify-table-header-matcher (org-config.el) add
;; one: they identify a table's header rows (the non-hline rows above its first
;; hline) so font-lock can prepend `org-table-header' there.  These exercise the
;; detection logic directly against fixture tables, matching the tag-alignment
;; test's pure-logic style.

;;; Code:

(require 'ert)
(require 'org)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-config)

(defmacro test-org-th--in (content &rest body)
  "Run BODY in a temp org buffer holding CONTENT, hooks suppressed."
  (declare (indent 1))
  `(let ((org-mode-hook nil))
     (with-temp-buffer
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defun test-org-th--goto (substring)
  "Move point to the beginning of the line containing SUBSTRING."
  (goto-char (point-min))
  (search-forward substring)
  (beginning-of-line))

;; ----- cj/--org-table-header-row-p -----

(ert-deftest test-org-table-header-row-p-header-above-hline ()
  "Normal: a non-hline row above the first hline is a header row."
  (test-org-th--in "| Name | Age |\n|------+-----|\n| Bob | 3 |\n"
    (test-org-th--goto "Name")
    (should (cj/--org-table-header-row-p))))

(ert-deftest test-org-table-header-row-p-body-row-not-header ()
  "Normal: a row below the first hline is not a header row."
  (test-org-th--in "| Name | Age |\n|------+-----|\n| Bob | 3 |\n"
    (test-org-th--goto "Bob")
    (should-not (cj/--org-table-header-row-p))))

(ert-deftest test-org-table-header-row-p-hline-not-header ()
  "Boundary: the hline itself is not a header row."
  (test-org-th--in "| Name | Age |\n|------+-----|\n| Bob | 3 |\n"
    (test-org-th--goto "----")
    (should-not (cj/--org-table-header-row-p))))

(ert-deftest test-org-table-header-row-p-no-hline-no-header ()
  "Boundary: a table with no hline has no header rows."
  (test-org-th--in "| A | B |\n| x | y |\n"
    (test-org-th--goto "A |")
    (should-not (cj/--org-table-header-row-p))))

(ert-deftest test-org-table-header-row-p-multi-row-header ()
  "Boundary: every non-hline row above the first hline is a header row."
  (test-org-th--in "| A | B |\n| C | D |\n|---+---|\n| x | y |\n"
    (test-org-th--goto "A |")
    (should (cj/--org-table-header-row-p))
    (test-org-th--goto "C |")
    (should (cj/--org-table-header-row-p))))

(ert-deftest test-org-table-header-row-p-key-value-first-row-only ()
  "Boundary: hline-after-every-row table -- only the first row is header."
  (test-org-th--in "| Status | draft |\n|--------+-------|\n| Owner | cj |\n|--------+-------|\n"
    (test-org-th--goto "Status")
    (should (cj/--org-table-header-row-p))
    (test-org-th--goto "Owner")
    (should-not (cj/--org-table-header-row-p))))

(ert-deftest test-org-table-header-row-p-non-table-line ()
  "Error: a line that is not in a table is never a header row."
  (test-org-th--in "Just some prose.\n"
    (test-org-th--goto "prose")
    (should-not (cj/--org-table-header-row-p))))

;; ----- cj/--org-table-first-hline-position -----

(ert-deftest test-org-table-first-hline-position-found ()
  "Normal: returns the bol of the first hline in the table."
  (test-org-th--in "| Name | Age |\n|------+-----|\n| Bob | 3 |\n"
    (test-org-th--goto "Name")
    (let ((expected (save-excursion (goto-char (point-min))
                                    (forward-line 1)
                                    (line-beginning-position))))
      (should (equal (cj/--org-table-first-hline-position) expected)))))

(ert-deftest test-org-table-first-hline-position-none ()
  "Boundary: a table with no hline returns nil."
  (test-org-th--in "| A | B |\n| x | y |\n"
    (test-org-th--goto "A |")
    (should-not (cj/--org-table-first-hline-position))))

;; ----- cj/--org-fontify-table-header-matcher -----

(ert-deftest test-org-fontify-table-header-matcher-matches-header-only ()
  "Normal: the matcher sets match data to the header row, then stops."
  (test-org-th--in "| Name | Age |\n|------+-----|\n| Bob | 3 |\n"
    (should (cj/--org-fontify-table-header-matcher (point-max)))
    (should (equal (match-string 0) "| Name | Age |"))
    (should-not (cj/--org-fontify-table-header-matcher (point-max)))))

(ert-deftest test-org-fontify-table-header-matcher-no-header ()
  "Boundary: a table with no hline yields no matches."
  (test-org-th--in "| A | B |\n| x | y |\n"
    (should-not (cj/--org-fontify-table-header-matcher (point-max)))))

(provide 'test-org-config-table-header)
;;; test-org-config-table-header.el ends here
