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

(require 'seq)

(defvar cj/coverage-backends nil
  "Registry of coverage backends in priority order.
Each entry is a plist with at least :name, :detect, :run, and :report-path.
Use `cj/coverage-register-backend' to add or replace an entry.")

(defvar-local cj/coverage-backend nil
  "Override: name of the coverage backend to use for the current project.
When nil (the default), resolution runs each registered backend's :detect
function in registration order.  Typically set buffer-locally via
`.dir-locals.el' to pin a specific backend.")

(defun cj/coverage-register-backend (backend)
  "Register BACKEND, a plist with :name, :detect, :run, :report-path.
Appends to `cj/coverage-backends' at the end, or replaces the existing
entry with the same :name in its current position."
  (let ((name (plist-get backend :name)))
	(if (cj/--coverage-backend-by-name name)
		(setq cj/coverage-backends
			  (mapcar (lambda (b)
						(if (eq (plist-get b :name) name) backend b))
					  cj/coverage-backends))
	  (setq cj/coverage-backends
			(append cj/coverage-backends (list backend))))))

(defun cj/--coverage-backend-by-name (name)
  "Return the registered backend whose :name equals NAME, or nil."
  (seq-find (lambda (b) (eq name (plist-get b :name)))
			cj/coverage-backends))

(defun cj/--coverage-backend-for-project (root &optional override)
  "Resolve the coverage backend to use for ROOT.
OVERRIDE, if non-nil, is a backend name symbol (typically the value of
`cj/coverage-backend' from .dir-locals.el).  When given, the named
backend is returned regardless of any :detect functions.  Signals
`user-error' when OVERRIDE names a backend that isn't registered.

When OVERRIDE is nil, each backend's :detect is called in turn with
ROOT as its sole argument; the first that returns non-nil wins.
Returns the backend plist, or nil when no backend matches."
  (cond
   (override
	(or (cj/--coverage-backend-by-name override)
		(user-error
		 "Unknown coverage backend: %s (registered: %s)"
		 override
		 (mapcar (lambda (b) (plist-get b :name)) cj/coverage-backends))))
   (t
	(seq-find (lambda (backend)
				(funcall (plist-get backend :detect) root))
			  cj/coverage-backends))))

(defun cj/--coverage-parse-simplecov (file)
  "Parse FILE as a simplecov JSON report and return covered lines per file.
Keys are source-file paths (strings).  Values are hash tables whose
keys are line numbers (integers) with a hit count greater than zero.
Lines marked nil (not executable) or 0 (executable but not hit) are
excluded.

Simplecov JSON structure is:
  { <test-name>: { \"coverage\": { <path>: [null | 0 | int, ...] } } }

When the JSON contains multiple top-level test-name keys, coverage
data is unioned across them; useful for files produced with undercover's
`:merge-report t' option that accumulate runs under a shared key, and
also for defensive handling of unexpected multi-key shapes.

Signals `user-error' if FILE does not exist or contains malformed JSON."
  (unless (file-exists-p file)
	(user-error "Simplecov report not found: %s" file))
  (require 'json)
  (let* ((json-object-type 'hash-table)
		 (json-array-type 'list)
		 (json-key-type 'string)
		 (data (condition-case err
				   (json-read-file file)
				 (error (user-error "Malformed simplecov JSON in %s: %s"
									file (error-message-string err)))))
		 (result (make-hash-table :test 'equal)))
	(maphash
	 (lambda (_test-name section)
	   (when (hash-table-p section)
		 (let ((coverage (gethash "coverage" section)))
		   (when (hash-table-p coverage)
			 (maphash
			  (lambda (path hits-list)
				(let ((lines (or (gethash path result)
								 (make-hash-table :test 'eql)))
					  (line-num 1))
				  (dolist (hits hits-list)
					(when (and (numberp hits) (> hits 0))
					  (puthash line-num t lines))
					(setq line-num (1+ line-num)))
				  (puthash path lines result)))
			  coverage)))))
	 data)
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
of line numbers (as built by `cj/--coverage-parse-simplecov' and
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

(defun cj/--coverage-format-report (records scope-label)
  "Render RECORDS as a text report for SCOPE-LABEL.
RECORDS is the list of plists produced by `cj/--coverage-intersect'.
SCOPE-LABEL is the human-readable scope name (e.g. \"Staged\").
Returns a string ready to insert into a compilation-mode buffer.

Uncovered-line entries use the format \"<path>:<line>: uncovered\"
so `compilation-error-regexp-alist' picks them up for
`next-error' / `previous-error' navigation.

Files with an empty :changed-lines (deletion-only hunks) are
omitted from the display.  The summary counts only tracked files."
  (if (null records)
	  (format "Coverage Report — %s\n\nNo changes in this scope; nothing to report.\n"
			  scope-label)
	(let (partial fully-covered not-tracked
				  (total-covered 0)
				  (total-tracked 0))
	  (dolist (rec records)
		(let ((changed (plist-get rec :changed-lines))
			  (tracked (plist-get rec :tracked))
			  (uncovered (plist-get rec :uncovered-lines))
			  (covered (plist-get rec :covered-lines)))
		  (cond
		   ((null changed) nil)   ; deletion-only; skip
		   ((not tracked)
			(push rec not-tracked))
		   (uncovered
			(push rec partial)
			(setq total-covered (+ total-covered (length covered))
				  total-tracked (+ total-tracked (length changed))))
		   (t
			(push rec fully-covered)
			(setq total-covered (+ total-covered (length covered))
				  total-tracked (+ total-tracked (length changed)))))))
	  (setq partial (nreverse partial)
			fully-covered (nreverse fully-covered)
			not-tracked (nreverse not-tracked))
	  (with-temp-buffer
		(let* ((header (format "Coverage Report — %s" scope-label))
			   (pct (if (> total-tracked 0)
						(/ (* 100.0 total-covered) total-tracked)
					  0.0)))
		  (insert header "\n")
		  (insert (make-string (length header) ?=) "\n\n")
		  (insert (format "Summary: %d of %d changed lines covered (%.1f%%)\n\n"
						  total-covered total-tracked pct)))
		(when partial
		  (insert "Uncovered lines:\n")
		  (dolist (rec partial)
			(dolist (line (plist-get rec :uncovered-lines))
			  (insert (format "  %s:%d: uncovered\n"
							  (plist-get rec :path) line))))
		  (insert "\n"))
		(when not-tracked
		  (insert "Not tracked (coverage data unavailable):\n")
		  (dolist (rec not-tracked)
			(insert (format "  %s (%d lines changed)\n"
							(plist-get rec :path)
							(length (plist-get rec :changed-lines)))))
		  (insert "\n"))
		(when fully-covered
		  (insert "Fully covered:\n")
		  (dolist (rec fully-covered)
			(let ((cnt (length (plist-get rec :covered-lines))))
			  (insert (format "  %s (%d/%d)\n"
							  (plist-get rec :path) cnt cnt))))
		  (insert "\n"))
		(buffer-string)))))

;;; --- Scope selection ---

(defconst cj/--coverage-scope-alist
  '(("Working tree — all uncommitted changes" . working-tree)
	("Staged — about to commit"               . staged)
	("Branch vs parent"                       . branch-vs-parent)
	("Branch vs main"                         . branch-vs-main))
  "Alist mapping human-readable scope labels to scope symbols.
Used by `cj/--coverage-select-scope' for the `completing-read' prompt
and by the report-buffer header to show which scope was picked.")

(defun cj/--coverage-scope-from-label (label)
  "Return the scope symbol for human-readable LABEL, or nil if unknown."
  (cdr (assoc label cj/--coverage-scope-alist)))

(defun cj/--coverage-label-from-scope (scope)
  "Return the human-readable label for SCOPE symbol, or nil if unknown."
  (car (rassq scope cj/--coverage-scope-alist)))

(defun cj/--coverage-select-scope ()
  "Prompt for a coverage scope via `completing-read'.
Returns the selected scope symbol (e.g. `staged')."
  (let* ((labels (mapcar #'car cj/--coverage-scope-alist))
		 (choice (completing-read "Coverage scope: " labels nil t)))
	(cj/--coverage-scope-from-label choice)))

;;; --- User-facing command ---

(defun cj/--coverage-project-root ()
  "Return the current project's root, or `default-directory' as fallback."
  (or (and (fboundp 'projectile-project-root)
		   (projectile-project-root))
	  default-directory))

(defun cj/--coverage-render-to-buffer (records scope)
  "Render RECORDS for SCOPE into the coverage report buffer.
Does the buffer setup, the insert, and switches it into
`cj/coverage-report-mode' for compilation-mode navigation."
  (let* ((label (cj/--coverage-label-from-scope scope))
		 (text (cj/--coverage-format-report records label))
		 (buf (get-buffer-create "*Coverage Report*")))
	(with-current-buffer buf
	  (let ((inhibit-read-only t))
		(erase-buffer)
		(insert text))
	  (cj/coverage-report-mode)
	  (goto-char (point-min)))
	(display-buffer buf)
	buf))

(defun cj/--coverage-read-and-display (backend scope)
  "Parse BACKEND's report file, intersect with SCOPE, display result."
  (let* ((report-path (funcall (plist-get backend :report-path)))
		 (covered (cj/--coverage-parse-simplecov report-path))
		 (changed (cj/--coverage-changed-lines scope))
		 (records (cj/--coverage-intersect covered changed)))
	(cj/--coverage-render-to-buffer records scope)))

(defun cj/coverage-report (&optional force-rerun)
  "Show a coverage report for in-flight changes in the current project.
Prompts for a git-diff scope via `completing-read', reads the most
recent coverage data, intersects with the diff, and pops a buffer
listing covered, uncovered, and not-tracked files.

With a prefix argument (FORCE-RERUN non-nil), re-runs coverage before
displaying the report even if a recent report already exists.  Without
the prefix, a missing report triggers a y/n prompt."
  (interactive "P")
  (let* ((root (cj/--coverage-project-root))
		 (backend (cj/--coverage-backend-for-project
				   root cj/coverage-backend)))
	(unless backend
	  (user-error
	   "No coverage backend for %s.  Register one or set cj/coverage-backend"
	   root))
	(let* ((scope (cj/--coverage-select-scope))
		   (report-path (funcall (plist-get backend :report-path))))
	  (cond
	   ;; Force rerun via prefix arg
	   (force-rerun
		(funcall (plist-get backend :run)
				 (lambda (_path)
				   (cj/--coverage-read-and-display backend scope))))
	   ;; Missing report — prompt before running
	   ((not (file-exists-p report-path))
		(if (y-or-n-p (format "No coverage report at %s.  Run coverage now? "
							  report-path))
			(funcall (plist-get backend :run)
					 (lambda (_path)
					   (cj/--coverage-read-and-display backend scope)))
		  (user-error "Coverage cancelled")))
	   ;; Otherwise, use existing data
	   (t
		(cj/--coverage-read-and-display backend scope))))))

(define-derived-mode cj/coverage-report-mode compilation-mode "Coverage"
  "Major mode for the coverage report buffer.
Derives from `compilation-mode' so next-error / previous-error and
the standard navigation keys (n, p, RET, g, q) work without extra
setup.  Uncovered-line entries match compilation-mode's default
`file:line: message' error regex."
  (setq-local compilation-error-regexp-alist '(gnu))
  (setq-local compilation-search-path (list (cj/--coverage-project-root))))

;;; --- Global keybinding ---

(keymap-global-set "<f7>" #'cj/coverage-report)

(provide 'coverage-core)
;;; coverage-core.el ends here
