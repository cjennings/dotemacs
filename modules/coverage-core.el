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

(defconst cj/--coverage-hunk-header-regexp
  "^@@ -[0-9]+\\(,[0-9]+\\)? \\+\\([0-9]+\\)\\(,\\([0-9]+\\)\\)? @@"
  "Regexp for a git unified-diff hunk header.
Captures new_start (group 2) and new_count (group 4; nil implies 1).")

(defconst cj/--coverage-file-marker-regexp
  "^\\+\\+\\+ b/\\(.+\\)$"
  "Regexp for the \"+++ b/<path>\" line of a git diff.
Captures the file path (group 1).")

(defun cj/--coverage-parse-diff-output (output)
  "Parse OUTPUT, a git unified-diff string, into a hash table.
Keys are file paths (relative to repo root, as git emits them).  Values
are hash tables whose keys are line numbers added or modified in the new
version of the file.  A file that appears with only deletions maps to an
empty hash table.  Malformed hunk headers are skipped silently."
  (let ((result (make-hash-table :test 'equal))
		(current-lines nil))
	(with-temp-buffer
	  (insert output)
	  (goto-char (point-min))
	  (while (not (eobp))
		(let ((line (buffer-substring-no-properties
					 (line-beginning-position) (line-end-position))))
		  (cond
		   ((string-match cj/--coverage-file-marker-regexp line)
			(let ((path (match-string 1 line)))
			  (setq current-lines (make-hash-table :test 'eql))
			  (puthash path current-lines result)))
		   ((string-prefix-p "+++ /dev/null" line)
			(setq current-lines nil))
		   ((and current-lines
				 (string-match cj/--coverage-hunk-header-regexp line))
			(let* ((new-start (string-to-number (match-string 2 line)))
				   (count-str (match-string 4 line))
				   (new-count (if count-str
								  (string-to-number count-str)
								1)))
			  (when (> new-count 0)
				(dotimes (i new-count)
				  (puthash (+ new-start i) t current-lines)))))))
		(forward-line 1)))
	result))

(defun cj/--coverage-changed-lines (scope &optional base)
  "Return a hash table of files to changed line numbers for SCOPE.
SCOPE is one of the symbols `working-tree', `staged', `branch-vs-main',
or `branch-vs-parent'.  For `branch-vs-parent', BASE is the ref to
compare against; if nil, falls back to the tracked upstream @{upstream}.
Signals `user-error' for any other SCOPE."
  (let ((cmd (cond
			  ((eq scope 'working-tree)
			   "git diff HEAD --unified=0")
			  ((eq scope 'staged)
			   "git diff --cached --unified=0")
			  ((eq scope 'branch-vs-main)
			   "git diff $(git merge-base HEAD main)..HEAD --unified=0")
			  ((eq scope 'branch-vs-parent)
			   (format "git diff $(git merge-base HEAD %s)..HEAD --unified=0"
					   (or base "@{upstream}")))
			  (t
			   (user-error "Unknown coverage scope: %s" scope)))))
	(cj/--coverage-parse-diff-output (shell-command-to-string cmd))))

(defun cj/--coverage-hash-keys-sorted (table)
  "Return a sorted list of TABLE's integer keys."
  (let (keys)
	(maphash (lambda (k _v) (push k keys)) table)
	(sort keys #'<)))

(defun cj/--coverage-intersect (covered changed)
  "Combine COVERED (LCOV) with CHANGED (git diff) into per-file records.
COVERED and CHANGED are each hash tables from file path to a hash table
of line numbers (as built by `cj/--coverage-parse-lcov' and
`cj/--coverage-parse-diff-output').  Either may be nil, in which case
the result is an empty list.

Return value is a list of plists, one per entry in CHANGED, sorted by
path:
  (:path PATH
   :changed-lines LIST-OF-INTS
   :covered-lines LIST-OF-INTS   ; nil when the file isn't tracked
   :uncovered-lines LIST-OF-INTS ; nil when the file isn't tracked
   :tracked BOOL)

A file that appears in CHANGED but not in COVERED is marked as
`:tracked nil'; coverage data is unavailable for it, so no lines
can be classified as covered or uncovered."
  (unless (and covered changed)
	(setq covered (or covered (make-hash-table :test 'equal)))
	(setq changed (or changed (make-hash-table :test 'equal))))
  (let (paths records)
	(maphash (lambda (path _) (push path paths)) changed)
	(setq paths (sort paths #'string<))
	(dolist (path paths)
	  (let* ((changed-set (gethash path changed))
			 (changed-lines (cj/--coverage-hash-keys-sorted changed-set))
			 (covered-set (gethash path covered))
			 (tracked (and covered-set t))
			 covered-lines
			 uncovered-lines)
		(when tracked
		  (dolist (line changed-lines)
			(if (gethash line covered-set)
				(push line covered-lines)
			  (push line uncovered-lines)))
		  (setq covered-lines (nreverse covered-lines)
				uncovered-lines (nreverse uncovered-lines)))
		(push (list :path path
					:changed-lines changed-lines
					:covered-lines covered-lines
					:uncovered-lines uncovered-lines
					:tracked tracked)
			  records)))
	(nreverse records)))

(provide 'coverage-core)
;;; coverage-core.el ends here
