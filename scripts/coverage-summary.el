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
		 (records (cj/--coverage-intersect covered executable)))
	(cj/--coverage-format-summary records "modules/")))

(defun cj/coverage-print-module-summary (report-file module-dir project-root)
  "Print a whole-project coverage summary for MODULE-DIR from REPORT-FILE."
  (princ "\n")
  (princ (cj/coverage-summary-text report-file module-dir project-root)))

(provide 'coverage-summary)
;;; coverage-summary.el ends here
