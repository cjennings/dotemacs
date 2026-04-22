;;; coverage-core.el --- Coverage reporting engine and backend registry -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Language-agnostic core for diff-aware coverage reporting.
;;
;; Reads an LCOV file, shells to git diff at a selectable scope,
;; intersects the results, and displays a report buffer.  Languages
;; plug in via the backend registry (see `cj/coverage-backends').
;;
;; See docs/design/coverage.org for the design rationale.

;;; Code:

(defun cj/--coverage-parse-lcov (file)
  "Parse FILE as LCOV and return a hash table of covered lines.
Keys are source-file paths (strings).  Values are hash tables whose
keys are line numbers (integers) that had a hit count greater than
zero.  Only the SF, DA, and end_of_record fields are read; other
LCOV fields are ignored.  Malformed DA lines are skipped silently.
Signals `user-error' if FILE does not exist."
  (unless (file-exists-p file)
	(user-error "LCOV file not found: %s" file))
  (let ((result (make-hash-table :test 'equal))
		(current-file nil)
		(current-lines nil))
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (while (not (eobp))
		(let ((line (buffer-substring-no-properties
					 (line-beginning-position) (line-end-position))))
		  (cond
		   ((string-prefix-p "SF:" line)
			(setq current-file (substring line 3))
			(setq current-lines (make-hash-table :test 'eql)))
		   ((string-prefix-p "DA:" line)
			(when current-lines
			  (let* ((rest (substring line 3))
					 (parts (split-string rest ","))
					 (line-str (car parts))
					 (hits-str (cadr parts))
					 (line-num (and line-str (string-match-p "\\`[0-9]+\\'" line-str)
									(string-to-number line-str)))
					 (hits (and hits-str (string-match-p "\\`[0-9]+\\'" hits-str)
								(string-to-number hits-str))))
				(when (and line-num hits (> hits 0))
				  (puthash line-num t current-lines)))))
		   ((string= line "end_of_record")
			(when current-file
			  (puthash current-file current-lines result))
			(setq current-file nil
				  current-lines nil))))
		(forward-line 1)))
	result))

(provide 'coverage-core)
;;; coverage-core.el ends here
