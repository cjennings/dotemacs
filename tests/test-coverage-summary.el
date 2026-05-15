;;; test-coverage-summary.el --- Tests for terminal coverage summary -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the batch helper used by `make coverage-summary'.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(require 'coverage-summary)

(defun test-coverage-summary--write-json (content)
  "Write CONTENT to a temp file; return its path."
  (let ((file (make-temp-file "test-coverage-summary-" nil ".json")))
	(with-temp-file file
	  (insert content))
	file))

(defun test-coverage-summary--touch (file)
  "Create FILE and its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert ";;; fixture\n")))

(ert-deftest test-coverage-summary-modules-only ()
  "Normal: summary includes modules files and ignores non-module files."
  (let* ((root (file-name-as-directory (make-temp-file "coverage-root-" t)))
		 (module-a (expand-file-name "modules/a.el" root))
		 (module-b (expand-file-name "modules/b.el" root))
		 (other (expand-file-name "tests/test-a.el" root))
		 (content (format
				   "{\"run\":{\"coverage\":{\"%s\":[1,0,null,1],\"%s\":[0,0],\"%s\":[1,1]}}}"
				   module-a module-b other))
		 (file (test-coverage-summary--write-json content)))
	(unwind-protect
		(progn
          (test-coverage-summary--touch module-a)
          (test-coverage-summary--touch module-b)
          (test-coverage-summary--touch other)
          (let ((output (cj/coverage-summary-text
					     file
					     (expand-file-name "modules" root)
					     root)))
		  (should (string-match-p "modules/a\\.el" output))
		  (should (string-match-p "modules/b\\.el" output))
		  (should-not (string-match-p "tests/test-a\\.el" output))
		  (should (string-match-p "2 of 5" output))
          (should (string-match-p "Project module coverage: 33\\.3%" output))
          (should (string-match-p "Not in SimpleCov report: 0 modules" output))))
	  (delete-file file)
	  (delete-directory root t))))

(ert-deftest test-coverage-summary-sorts-worst-first ()
  "Normal: module rows use the same worst-first sorting as the editor summary."
  (let* ((root (file-name-as-directory (make-temp-file "coverage-root-" t)))
		 (low (expand-file-name "modules/low.el" root))
		 (high (expand-file-name "modules/high.el" root))
		 (content (format
				   "{\"run\":{\"coverage\":{\"%s\":[0,0,1,1],\"%s\":[1,1]}}}"
				   low high))
		 (file (test-coverage-summary--write-json content)))
	(unwind-protect
		(progn
          (test-coverage-summary--touch low)
          (test-coverage-summary--touch high)
          (let* ((output (cj/coverage-summary-text
						  file
						  (expand-file-name "modules" root)
						  root))
			     (pos-low (string-match "modules/low\\.el" output))
			     (pos-high (string-match "modules/high\\.el" output)))
		  (should pos-low)
		  (should pos-high)
		  (should (< pos-low pos-high))))
	  (delete-file file)
	  (delete-directory root t))))

(ert-deftest test-coverage-summary-lists-modules-missing-from-simplecov ()
  "Normal: modules on disk but absent from SimpleCov are listed separately."
  (let* ((root (file-name-as-directory (make-temp-file "coverage-root-" t)))
         (tracked (expand-file-name "modules/tracked.el" root))
         (missing-a (expand-file-name "modules/missing-a.el" root))
         (missing-b (expand-file-name "modules/missing-b.el" root))
         (content (format
                   "{\"run\":{\"coverage\":{\"%s\":[1,0,null,1]}}}"
                   tracked))
         (file (test-coverage-summary--write-json content)))
    (unwind-protect
        (progn
          (test-coverage-summary--touch tracked)
          (test-coverage-summary--touch missing-a)
          (test-coverage-summary--touch missing-b)
          (let ((output (cj/coverage-summary-text
                         file
                         (expand-file-name "modules" root)
                         root)))
            (should (string-match-p "Total: 2 of 3 lines covered" output))
            (should (string-match-p
                     "Project module coverage: 22\\.2% (1 tracked, 2 missing, 3 total; missing modules count as 0%)"
                     output))
            (should (string-match-p "Not in SimpleCov report: 2 modules" output))
            (should (string-match-p "modules/missing-a\\.el" output))
            (should (string-match-p "modules/missing-b\\.el" output))
            (should (string-match-p "count as 0% in project module coverage" output))))
      (delete-file file)
      (delete-directory root t))))

(ert-deftest test-coverage-summary-missing-module-helper-ignores-elc-and-subdirs ()
  "Boundary: untracked module detection is exactly direct modules/*.el files."
  (let* ((root (file-name-as-directory (make-temp-file "coverage-root-" t)))
         (module-dir (expand-file-name "modules" root))
         (tracked-file (expand-file-name "modules/tracked.el" root))
         (missing-file (expand-file-name "modules/missing.el" root))
         (compiled-file (expand-file-name "modules/compiled.elc" root))
         (nested-file (expand-file-name "modules/nested/not-a-module.el" root))
         (tracked-table (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (test-coverage-summary--touch tracked-file)
          (test-coverage-summary--touch missing-file)
          (make-directory (file-name-directory compiled-file) t)
          (with-temp-file compiled-file (insert "compiled"))
          (test-coverage-summary--touch nested-file)
          (puthash "modules/tracked.el" t tracked-table)
          (should (equal (cj/coverage-summary--missing-module-files
                          tracked-table module-dir root)
                         '("modules/missing.el"))))
      (delete-directory root t))))

(provide 'test-coverage-summary)
;;; test-coverage-summary.el ends here
