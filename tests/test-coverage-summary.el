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
		(let ((output (cj/coverage-summary-text
					   file
					   (expand-file-name "modules" root)
					   root)))
		  (should (string-match-p "modules/a\\.el" output))
		  (should (string-match-p "modules/b\\.el" output))
		  (should-not (string-match-p "tests/test-a\\.el" output))
		  (should (string-match-p "2 of 5" output)))
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
		(let* ((output (cj/coverage-summary-text
						file
						(expand-file-name "modules" root)
						root))
			   (pos-low (string-match "modules/low\\.el" output))
			   (pos-high (string-match "modules/high\\.el" output)))
		  (should pos-low)
		  (should pos-high)
		  (should (< pos-low pos-high)))
	  (delete-file file)
	  (delete-directory root t))))

(provide 'test-coverage-summary)
;;; test-coverage-summary.el ends here
