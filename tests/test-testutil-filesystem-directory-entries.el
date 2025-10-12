;;; test-testutil-filesystem-directory-entries.el ---  -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; ERT tests for testutil-filesystem.el
;; Tests  cj/list-directory-recursive and it's helper function cj/get--directory-entries.
;;
;;; Code:

(require 'ert)
(require 'f)

;; load test directory
(add-to-list 'load-path (concat user-emacs-directory "tests/"))
(require 'testutil-general)    ;; helper functions
(require 'testutil-filesystem) ;; file under test

(defun cj/test--setup ()
  "Create the test base directory using `cj/create-test-base-dir'."
  (cj/create-test-base-dir))

(defun cj/test--teardown ()
  "Remove the test base directory using `cj/delete-test-base-dir'."
  (cj/delete-test-base-dir))

;;; ---------------------- CJ/GET--DIRECTORY-ENTRIES TESTS ----------------------
;;;; Normal Case Tests

(ert-deftest test-normal-one-file ()
  "Test a single file at the base directory."
  (cj/test--setup)
  (unwind-protect
	  (progn
		(cj/create-directory-or-file-ensuring-parents "file.txt" "Test file")
		(let
			;; get paths to all files
			((entries (cj/get--directory-entries cj/test-base-dir)))
		  ;; check for files of different types and in subdirectories
		  (should (cl-some (lambda (e) (string= (f-filename e) "file.txt")) entries))))
	(cj/test--teardown)))

(ert-deftest test-normal-includes-subdirectories-but-no-contents ()
  "Test that we do include subdirectories themselves."
  (cj/test--setup)
  (unwind-protect
	  (progn
		;; create yoru test assets
		(cj/create-directory-or-file-ensuring-parents "file1.org" "Test file 1" t)
		(cj/create-directory-or-file-ensuring-parents "subdir/file2.org" "Nested file")
		;; get paths to all files
		(let ((entries (cj/get--directory-entries cj/test-base-dir)))
		  (should (cl-some (lambda (e) (and (file-directory-p e)
									   (string= (f-filename e) "subdir"))) entries))
		  (should-not (cl-some (lambda (e) (string= (f-filename e) "file2.org")) entries))))
	(cj/test--teardown)))

(ert-deftest test-normal-excludes-hidden-by-default ()
  "Test that hidden files (i.e.,begin with a dot) are excluded by default.
Asserts no subdirectories or hidden files or visible files in hidden subdirectories are returned."
  (cj/test--setup)
  (unwind-protect
	  (progn
		;; create your test assets
		(cj/create-directory-or-file-ensuring-parents ".hiddenfile" "Hidden content")
		;; get paths to all files
		(let ((entries (cj/get--directory-entries cj/test-base-dir)))
		  ;; should not see hidden file
		  (should-not (cl-some (lambda (e) (string= (f-filename e) ".hiddenfile")) entries))))
	(cj/test--teardown)))

(ert-deftest test-normal-includes-hidden-with-flag ()
  "Non-nil means hidden files are included."
  (cj/test--setup)
  (unwind-protect
	  (progn
		;; create your test assets
		(cj/create-directory-or-file-ensuring-parents ".hiddenfile" "Hidden content")
		;; get paths to all files passing in t to reveal hidden files
		(let ((entries (cj/get--directory-entries cj/test-base-dir t)))
		  ;; should not see hidden file
		  (should (cl-some (lambda (e) (string= (f-filename e) ".hiddenfile")) entries))))
	(cj/test--teardown)))

;;
;;;; Boundary Cases

(ert-deftest test-boundary-empty-directory ()
  "Test an empty directory returns empty list."
  (cj/test--setup)
  (unwind-protect
	  (let ((entries (cj/get--directory-entries cj/test-base-dir)))
		(should (equal entries nil)))
	(cj/test--teardown)))

(ert-deftest test-boundary-files-with-unusual-names ()
  "Test files with unusual names."
  (cj/test--setup)
  (unwind-protect
	  (progn
		(cj/create-directory-or-file-ensuring-parents "file with spaces.org" "content")
		(cj/create-directory-or-file-ensuring-parents "unicode-ß₄©.org" "content")  ;; Direct Unicode chars
		;; Or use proper escape sequences:
		;; (cj/create-directory-or-file-ensuring-parents "unicode-\u00DF\u2074\u00A9.org" "content")
		(let ((entries (cj/get--directory-entries cj/test-base-dir)))
		  (should (cl-some (lambda (e) (string= (f-filename e) "file with spaces.org")) entries))
		  (should (cl-some (lambda (e) (string= (f-filename e) "unicode-ß₄©.org")) entries))))
	(cj/test--teardown)))

;;;; Error Cases

(ert-deftest test-error-nonexistent-directory ()
  "Test calling on nonexistent directory returns nil or error handled."
  (should-error (cj/get--directory-entries "/path/does/not/exist")))
										;
(ert-deftest test-error-not-a-directory-path ()
  "Test calling on a file path signals error."
  (cj/test--setup)
  (unwind-protect
	  (let ((filepath (cj/create-directory-or-file-ensuring-parents "file.txt" "data")))
		(should-error (cj/get--directory-entries filepath)))
	(cj/test--teardown)))

(ert-deftest test-error-permission-denied ()
  "Test directory with no permission signals error or returns nil."
  (cj/test--setup)
  (unwind-protect
	  (let ((dir (expand-file-name "noperm" cj/test-base-dir)))
		(cj/create-directory-or-file-ensuring-parents "noperm/file2.org" "Nested file")
		(let ((original-mode (file-modes dir))) ; Save original permissions
		  (set-file-modes dir #o000) ; Remove all permissions
		  (unwind-protect
			  (should-error (cj/get--directory-entries dir))
			(set-file-modes dir original-mode)))) ; Restore permissions - extra paren here
	(cj/test--teardown)))

;;; --------------------- CJ/LIST-DIRECTORY-RECURSIVE TESTS ---------------------
;;;; Normal Cases

(ert-deftest test-normal-single-file-at-root ()
  "Test the normal base case: one single file at the root."
  (cj/test--setup)
  (unwind-protect
	  (progn
		(cj/create-directory-or-file-ensuring-parents "file.txt" "Content")
		(let ((file-infos (cj/list-directory-recursive cj/test-base-dir)))
		  (should  (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file.txt")) file-infos))))
	(cj/test--teardown)))

(ert-deftest test-normal-multiple-files-at-root ()
  "Test finding multiple files at the root directory."
  (cj/test--setup)
  (unwind-protect
	  (cj/create-directory-or-file-ensuring-parents "file1.txt" "Content in File 1")
	(cj/create-directory-or-file-ensuring-parents "file2.org" "Content in File 2")
	(cj/create-directory-or-file-ensuring-parents "file3.md" "Content in File 3")
	(let ((file-infos (cj/list-directory-recursive cj/test-base-dir)))
	  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.txt")) file-infos))
	  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file2.org")) file-infos))
	  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file3.md")) file-infos)))
	(cj/test--teardown)))

(ert-deftest test-normal-multiple-files-in-subdirectories ()
  "Test finding multiple files at the root directory."
  (cj/test--setup)
  (unwind-protect
	  (cj/create-directory-or-file-ensuring-parents "one/file1.txt" "Content in File 1")
	(cj/create-directory-or-file-ensuring-parents "two/file2.org" "Content in File 2")
	(cj/create-directory-or-file-ensuring-parents "three/file3.md" "Content in File 3")
	(let ((file-infos (cj/list-directory-recursive cj/test-base-dir)))
	  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.txt")) file-infos))
	  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file2.org")) file-infos))
	  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file3.md")) file-infos)))
	(cj/test--teardown)))

(ert-deftest test-recursive-excludes-hidden-by-default ()
  "Test that hidden files are excluded by default in recursive listing.
Verify that files beginning with a dot, hidden directories, and files
within hidden directories are all excluded when include-hidden is nil."
  (cj/test--setup)
  (unwind-protect
	  (progn
		;; Create test assets including hidden files at various levels
		(cj/create-directory-or-file-ensuring-parents ".hiddenfile" "Hidden content")
		(cj/create-directory-or-file-ensuring-parents ".hiddendir/visible-in-hidden.txt" "File in hidden dir")
		(cj/create-directory-or-file-ensuring-parents "visible/normal.txt" "Normal file")
		(cj/create-directory-or-file-ensuring-parents "visible/.hidden-in-visible.txt" "Hidden in visible dir")

		;; Get all files recursively (default excludes hidden)
		(let ((file-infos (cj/list-directory-recursive cj/test-base-dir)))
		  ;; Should not see .hiddenfile at root
		  (should-not (cl-some (lambda (fi)
								 (string= (f-filename (plist-get fi :path)) ".hiddenfile"))
							   file-infos))
		  ;; Should not see .hiddendir directory
		  (should-not (cl-some (lambda (fi)
								 (string= (f-filename (plist-get fi :path)) ".hiddendir"))
							   file-infos))
		  ;; Should not see files inside hidden directory
		  (should-not (cl-some (lambda (fi)
								 (string= (f-filename (plist-get fi :path)) "visible-in-hidden.txt"))
							   file-infos))
		  ;; Should not see hidden file in visible directory
		  (should-not (cl-some (lambda (fi)
								 (string= (f-filename (plist-get fi :path)) ".hidden-in-visible.txt"))
							   file-infos))
		  ;; Should see normal visible file
		  (should (cl-some (lambda (fi)
							 (string= (f-filename (plist-get fi :path)) "normal.txt"))
						   file-infos))))
	(cj/test--teardown)))

(ert-deftest test-recursive-includes-hidden-with-flag ()
  "Non-nil means hidden files are included.
Verifies that files beginning with a dot, hidden directories, and files
within hidden directories are all included when include-hidden is t."
  (cj/test--setup)
  (unwind-protect
	  (progn
		;; Create test assets including hidden files at various levels
		(cj/create-directory-or-file-ensuring-parents ".hiddenfile" "Hidden content")
		(cj/create-directory-or-file-ensuring-parents ".hiddendir/visible-in-hidden.txt" "File in hidden dir")
		(cj/create-directory-or-file-ensuring-parents "visible/normal.txt" "Normal file")
		(cj/create-directory-or-file-ensuring-parents "visible/.hidden-in-visible.txt" "Hidden in visible dir")

		;; Get all files recursively with include-hidden = t
		(let ((file-infos (cj/list-directory-recursive cj/test-base-dir t)))
		  ;; Should see .hiddenfile at root
		  (should (cl-some (lambda (fi)
							 (string= (f-filename (plist-get fi :path)) ".hiddenfile")) file-infos))
		  ;; Should see .hiddendir directory
		  (should (cl-some (lambda (fi) (and (plist-get fi :directory)
										(string= (f-filename (plist-get fi :path)) ".hiddendir"))) file-infos))
		  ;; Should see files inside hidden directory
		  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "visible-in-hidden.txt")) file-infos))
		  ;; Should see hidden file in visible directory
		  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) ".hidden-in-visible.txt")) file-infos))
		  ;; Should still see normal visible file
		  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "normal.txt")) file-infos))))
	(cj/test--teardown)))

(ert-deftest test-normal-deeply-nested-structure ()
  "Tests with deeply nested directory trees."
  (cj/test--setup)
  (unwind-protect
	  (progn
		(cj/create-directory-or-file-ensuring-parents
		 "one/two/three/four/five/six/seven/eight/nine/ten/eleven/twelve/13.txt" "thirteen")
		(cj/create-directory-or-file-ensuring-parents
		 "1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/thirty.txt" "30")
		(let ((file-infos (cj/list-directory-recursive cj/test-base-dir)))
		  ;; validate the files
		  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "13.txt")) file-infos))
		  (should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "thirty.txt")) file-infos))))
	(cj/test--teardown)))

(ert-deftest test-normal-only-directory-entries ()
  "Tests with deeply nested directory trees without files."
  (cj/test--setup)
  (unwind-protect
	  (progn
		(cj/create-directory-or-file-ensuring-parents
		 "one/two/three/four/five/six/seven/eight/nine/ten/eleven/twelve/thirteen/")
		(cj/create-directory-or-file-ensuring-parents
		 "1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/30/")
		(let ((file-infos (cj/list-directory-recursive cj/test-base-dir)))
		  ;; validate the directories
		  (should (cl-some (lambda (fi)
							 (and (string= (f-filename (plist-get fi :path)) "thirteen")
								  (plist-get fi :directory)
								  (file-directory-p (plist-get fi :path))))
						   file-infos))

		  (should (cl-some (lambda (fi)
							 (and (string= (f-filename (plist-get fi :path)) "30")
								  (plist-get fi :directory)
								  (file-directory-p (plist-get fi :path))))
						   file-infos))))
	(cj/test--teardown)))

;; 5. =test-normal-filter-by-extension= - Filter predicate correctly filters .org files


;; 6. =test-normal-filter-by-size= - Filter predicate filters files > 1KB
;; 7. =test-normal-filter-excludes-directories= - Filter can exclude directories themselves
;; 8. =test-normal-max-depth-one= - Respects max-depth=1 (only immediate children)
;; 9. =test-normal-max-depth-three= - Respects max-depth=3 limit
;; 11. =test-normal-executable-files= - Correctly identifies executable files
;; 12. =test-normal-file-info-plist-structure= - Verifies correct plist keys/values returned

;;;; Boundary Cases
;; 1. =test-boundary-empty-directory= - Empty directory returns empty list
;; 2. =test-boundary-single-empty-subdirectory= - Directory with only empty subdirectory
;; 3. =test-boundary-unicode-filenames= - Files with unicode characters (emoji, Chinese, etc.)
;; 4. =test-boundary-spaces-in-names= - Files/dirs with spaces in names
;; 5. =test-boundary-special-characters= - Files with special chars (@#$%^&*()_+)
;; 6. =test-boundary-very-long-filename= - File with 255 character name
;; 8. =test-boundary-many-files= - Directory with 1000+ files
;; 9. =test-boundary-max-depth-zero= - max-depth=0 (unlimited) works correctly
;; 10. =test-boundary-symlinks= - How it handles symbolic links
;; 11. =test-boundary-filter-returns-all-nil= - Filter that rejects everything
;; 12. =test-boundary-filter-returns-all-true= - Filter that accepts everything

;;;; Error Cases
;; 1. =test-error-nonexistent-path= - Path that doesn't exist
;; 2. =test-error-file-not-directory= - PATH is a file, not directory
;; 3. =test-error-permission-denied= - Directory without read permissions
;; 4. =test-error-permission-denied-subdirectory= - Subdirectory without permissions
;; 5. =test-error-invalid-max-depth= - Negative max-depth value
;; 6. =test-error-filter-predicate-errors= - Filter function that throws error
;; 7. =test-error-circular-symlinks= - Circular symbolic link reference
;; 8. =test-error-path-outside-home= - Attempt to access system directories (if restricted)
;; 9. =test-error-nil-path= - PATH is nil
;; 10. =test-error-empty-string-path= - PATH is empty string

(provide 'test-testutil-filesystem-directory-entries)
;;; test-testutil-filesystem-directory-entries.el ends here.
