;;; test-theme-theme-persistence.el --- Tests theme persistence mechanism -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the persistence of the chosen theme

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'ui-theme)

;; ------------------------ Constants / Setup / Teardown -----------------------

(defvar cj/original-theme-name nil)
(defvar cj/original-newline-setting nil)

(defun cj/test-setup ()
  "Required settings and save state before each test."

  ;; save the current theme for restoration
  (setq cj/original-theme-name (symbol-name (car custom-enabled-themes)))
  (setq cj/original-newline-setting mode-require-final-newline)

  ;; unload all themes before starting test
  (mapcar #'disable-theme custom-enabled-themes)

  ;; no EOF newlines
  (custom-set-variables
   '(require-final-newline nil))
  (setq mode-require-final-newline nil))

(defun cj/test-teardown ()
  "Restore the state before each test."
  ;; restore newline setting
  (setq require-final-newline cj/original-newline-setting)

  ;; if there wasn't an original theme, remove all themes
  (if (string= cj/original-theme-name "nil")
	  (mapcar #'disable-theme custom-enabled-themes)
  ;; otherwise, restore it
	(load-theme (intern cj/original-theme-name))))

;; ----------------------------------- Tests -----------------------------------

(ert-deftest test-write-file-contents ()
  "Normal Case: Uses function to write a string, reads it back, and compares."
  (cj/test-setup)
  (let ((teststring "testing123")
		(testfilename "test-write-file-contents.txt"))
	;; call the function
	(should (equal (cj/write-file-contents teststring testfilename)
				   't))
	;; Read the file and check it's contents
	(should (equal (with-temp-buffer(insert-file-contents testfilename)
									(buffer-string))
				   teststring))
	;; clean up test file
	(delete-file testfilename))
  (cj/test-teardown))

(ert-deftest test-write-file-not-writable ()
  "Test writing to a non-writable file."
  (cl-flet ((file-writeable-p (file) nil))
	(let* ((non-writable-file (make-temp-file "test-non-writable"))
		   (should (equal (cj/write-file-contents "cowabunga" non-writable-file) 'nil)))
	  (delete-file non-writable-file))))

(ert-deftest test-read-file-contents ()
  "Normal Case: Writes string to file and reads contents using function."
  (cj/test-setup)
  (let ((teststring "testing123")
		(testfilename "test-read-file-contents.txt"))
	;; write the file
	(with-temp-buffer
	  (insert teststring)
	  (write-file testfilename))
	;; call the function
	(should (equal (cj/read-file-contents testfilename)
				   teststring))
	;; clean up test file
	(delete-file testfilename))
  (cj/test-teardown))

(ert-deftest test-read-file-nonexistent ()
  "Test reading from a non-existent file returns nil."
  (cj/test-setup)
  (let* ((filename (concat (number-to-string (random 99999999)) "nonexistent-file.txt"))
		 (result (cj/read-file-contents filename)))
	(should (equal result nil)))
  (cj/test-teardown))

(ert-deftest test-get-active-theme ()
  (cj/test-setup)
  "Normal Case: Sets theme, gets theme-name, and compares."
  (let ((expected "wombat"))
	(load-theme (intern expected))
	(should (string= (cj/get-active-theme-name) expected))
	(cj/test-teardown)))

(ert-deftest test-get-active-theme ()
  (cj/test-setup)
  "Normal Case: Sets theme, gets theme-name, and compares."
  (let ((expected "nil"))
	(mapcar #'disable-theme custom-enabled-themes)
	(should (equal (cj/get-active-theme-name) expected))
	(cj/test-teardown)))

(ert-deftest test-save-theme-to-file ()
  "Normal case: sets theme, saves it, reads from file, and compares."
  (cj/test-setup)
  (let ((expected "wombat"))
	(load-theme (intern expected))
	(cj/save-theme-to-file)
	(should (equal (cj/read-file-contents theme-file) expected))
	(cj/test-teardown)))

(ert-deftest test-load-theme-from-file ()
  "Normal case: saves new theme to file, loads it from file, and compares."
  (cj/test-setup)
  (let ((expected "wombat")) ;; the ui theme that test-setup uses.
	(cj/write-file-contents expected theme-file)
	(cj/load-theme-from-file)
	(should (equal expected (cj/get-active-theme-name))))
  (cj/test-teardown))

(ert-deftest test-load-nil-theme ()
  "Corner case: saves 'nil as theme name to file, loads it, and compares to not having a theme."
  (cj/test-setup)
  (let ((expected "nil")) ;; the ui theme that test-setup uses.
	(cj/write-file-contents expected theme-file)
	(cj/load-theme-from-file)
	(should (equal expected (cj/get-active-theme-name))))
  (cj/test-teardown))

(provide 'test-theme-theme-persistence)
;;; test-theme-theme-persistence.el ends here.
