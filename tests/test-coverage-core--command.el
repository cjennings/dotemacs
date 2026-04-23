;;; test-coverage-core--command.el --- Tests for cj/coverage-report and scope helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for:
;;   `cj/--coverage-scope-from-label' (pure label → symbol lookup)
;;   `cj/--coverage-label-from-scope' (pure symbol → label lookup)
;;   `cj/coverage-report' (one smoke test with stubbed git + prepared report)

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

;;; Scope label <-> symbol (pure lookups)

(ert-deftest test-coverage-scope-from-label-known ()
  "Normal: each registered label maps to its scope symbol."
  (should (eq 'working-tree
			  (cj/--coverage-scope-from-label
			   "Working tree — all uncommitted changes")))
  (should (eq 'staged
			  (cj/--coverage-scope-from-label "Staged — about to commit")))
  (should (eq 'branch-vs-parent
			  (cj/--coverage-scope-from-label "Branch vs parent")))
  (should (eq 'branch-vs-main
			  (cj/--coverage-scope-from-label "Branch vs main"))))

(ert-deftest test-coverage-scope-from-label-unknown ()
  "Boundary: unknown label returns nil."
  (should-not (cj/--coverage-scope-from-label "bogus label"))
  (should-not (cj/--coverage-scope-from-label "")))

(ert-deftest test-coverage-label-from-scope-roundtrip ()
  "Normal: symbol → label → symbol is an identity."
  (dolist (sym '(working-tree staged branch-vs-parent branch-vs-main))
	(should (eq sym (cj/--coverage-scope-from-label
					 (cj/--coverage-label-from-scope sym))))))

;;; Smoke test for the interactive command

(ert-deftest test-coverage-report-smoke-happy-path ()
  "Smoke: cj/coverage-report with stubbed backend, scope, and git-diff
populates the *Coverage Report* buffer with the expected summary and
uncovered-line markers."
  (let* ((tmp-root (make-temp-file "test-coverage-smoke-" t))
		 (report-file (expand-file-name "simplecov.json" tmp-root))
		 (simplecov (concat "{\"run\":{\"timestamp\":1,\"coverage\":"
							"{\"modules/foo.el\":[null,1,0,1,0]}}}"))
		 (diff-output (concat "diff --git a/modules/foo.el b/modules/foo.el\n"
							  "--- a/modules/foo.el\n"
							  "+++ b/modules/foo.el\n"
							  "@@ -1,0 +2,4 @@\n"
							  "+added 1\n+added 2\n+added 3\n+added 4\n"))
		 (test-backend
		  (list :name 'test-backend
				:detect (lambda (_) t)
				:run (lambda (cb) (funcall cb report-file))
				:report-path (lambda (&rest _) report-file)))
		 (cj/coverage-backends nil))
	(unwind-protect
		(progn
		  (with-temp-file report-file (insert simplecov))
		  (cj/coverage-register-backend test-backend)
		  (cl-letf (((symbol-function 'completing-read)
					 (lambda (&rest _) "Staged — about to commit"))
					((symbol-function 'shell-command-to-string)
					 (lambda (_) diff-output))
					((symbol-function 'cj/--coverage-project-root)
					 (lambda () tmp-root))
					((symbol-function 'display-buffer) #'identity))
			(call-interactively #'cj/coverage-report))
		  (with-current-buffer "*Coverage Report*"
			(let ((text (buffer-substring-no-properties (point-min) (point-max))))
			  ;; Scope label appears
			  (should (string-match-p "Staged" text))
			  ;; Lines 2 and 4 hit, lines 3 and 5 uncovered (per simplecov)
			  ;; Diff covers lines 2-5
			  ;; So the summary should show 2 of 4 covered
			  (should (string-match-p "2 of 4" text))
			  ;; Uncovered line markers use compilation-friendly format
			  (should (string-match-p "modules/foo\\.el:[35]: uncovered" text)))))
	  (when (get-buffer "*Coverage Report*")
		(kill-buffer "*Coverage Report*"))
	  (delete-directory tmp-root t))))

(provide 'test-coverage-core--command)
;;; test-coverage-core--command.el ends here
