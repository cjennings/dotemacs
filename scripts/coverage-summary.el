;;; coverage-summary.el --- Terminal summary for SimpleCov module coverage -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch helper for `make coverage' and `make coverage-summary'.
;; Reuses coverage-core's SimpleCov parser and whole-project formatter so the
;; terminal table matches the editor's whole-project coverage semantics.

;;; Code:

(require 'coverage-core)

(defun cj/coverage-summary--copy-lines (lines)
  "Return a copy of hash table LINES."
  (let ((copy (make-hash-table :test 'eql)))
	(when (hash-table-p lines)
	  (maphash (lambda (line value)
				 (puthash line value copy))
			   lines))
	copy))

(defun cj/coverage-summary--modules-only (table module-dir project-root)
  "Filter coverage TABLE to files under MODULE-DIR.

Returned keys are relative to PROJECT-ROOT for readable terminal output."
  (let ((result (make-hash-table :test 'equal))
		(module-dir (file-name-as-directory (expand-file-name module-dir)))
		(project-root (file-name-as-directory (expand-file-name project-root))))
	(maphash
	 (lambda (path lines)
	   (let ((absolute-path (expand-file-name path)))
		 (when (string-prefix-p module-dir absolute-path)
		   (puthash (file-relative-name absolute-path project-root)
					(cj/coverage-summary--copy-lines lines)
					result))))
	 table)
	result))

(defun cj/coverage-summary--module-files (module-dir project-root)
  "Return repository module files under MODULE-DIR, relative to PROJECT-ROOT.

This intentionally uses only direct =modules/*.el= files.  Compiled files,
subdirectories, and non-Elisp assets are outside this repository's module
coverage universe."
  (let ((module-dir (file-name-as-directory (expand-file-name module-dir)))
        (project-root (file-name-as-directory (expand-file-name project-root))))
    (sort
     (mapcar (lambda (path)
               (file-relative-name path project-root))
             (directory-files module-dir t "\\.el\\'"))
     #'string<)))

(defun cj/coverage-summary--missing-module-files (tracked-table module-dir project-root)
  "Return modules present on disk but absent from TRACKED-TABLE.

TRACKED-TABLE should use readable project-relative paths, such as the table
returned by `cj/coverage-summary--modules-only'."
  (let (tracked)
    (maphash (lambda (path _lines) (push path tracked)) tracked-table)
    (seq-difference
     (cj/coverage-summary--module-files module-dir project-root)
     tracked
     #'string=)))

(defun cj/coverage-summary--format-missing-modules (missing)
  "Return a report section for MISSING module files.

MISSING is a list of project-relative module paths that are absent from the
SimpleCov report."
  (with-temp-buffer
    (insert (format "\nNot in SimpleCov report: %d module%s\n"
                    (length missing)
                    (if (= 1 (length missing)) "" "s")))
    (if missing
        (progn
          (insert "These modules had no coverage entry; they count as 0% in project module coverage.\n")
          (dolist (path missing)
            (insert (format "  %s\n" path))))
      (insert "Every modules/*.el file appears in the SimpleCov report.\n"))
    (buffer-string)))

(defun cj/coverage-summary--record-module-percent (record)
  "Return RECORD's per-module coverage percentage.

RECORD is a plist from `cj/--coverage-intersect'.  A tracked module with no
executable lines contributes 100%; there is nothing executable left uncovered."
  (let ((total (length (plist-get record :changed-lines)))
        (covered (length (plist-get record :covered-lines))))
    (if (> total 0)
        (/ (* 100.0 covered) total)
      100.0)))

(defun cj/coverage-summary--format-project-module-coverage (records missing)
  "Return the project-module coverage policy section.

The existing SimpleCov total is line-weighted over files present in the report.
This section is module-weighted over all direct `modules/*.el' files: tracked
modules contribute their per-file coverage percentage, while MISSING modules
contribute 0%."
  (let* ((tracked (seq-filter (lambda (rec) (plist-get rec :tracked)) records))
         (tracked-count (length tracked))
         (missing-count (length missing))
         (total-count (+ tracked-count missing-count))
         (score (apply #'+ (mapcar #'cj/coverage-summary--record-module-percent
                                   tracked)))
         (pct (if (> total-count 0)
                  (/ score total-count)
                0.0)))
    (format (concat "\nProject module coverage: %.1f%%"
                    " (%d tracked, %d missing, %d total; missing modules count as 0%%)\n")
            pct tracked-count missing-count total-count)))

(defun cj/coverage-summary-text (report-file module-dir project-root)
  "Return a whole-project coverage summary for MODULE-DIR from REPORT-FILE."
  (let* ((covered (cj/coverage-summary--modules-only
				   (cj/--coverage-parse-simplecov report-file)
				   module-dir
				   project-root))
		 (executable (cj/coverage-summary--modules-only
					  (cj/--coverage-simplecov-executable-lines report-file)
					  module-dir
					  project-root))
		 (records (cj/--coverage-intersect covered executable))
         (missing (cj/coverage-summary--missing-module-files executable
                                                             module-dir
                                                             project-root)))
	(concat
     (cj/--coverage-format-summary records "modules/")
     (cj/coverage-summary--format-project-module-coverage records missing)
     (cj/coverage-summary--format-missing-modules missing))))

(defun cj/coverage-print-module-summary (report-file module-dir project-root)
  "Print a whole-project coverage summary for MODULE-DIR from REPORT-FILE."
  (princ "\n")
  (princ (cj/coverage-summary-text report-file module-dir project-root)))

(provide 'coverage-summary)
;;; coverage-summary.el ends here
