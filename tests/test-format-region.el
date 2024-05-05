;;; test-format-region.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;  Some basic tests for the custom function cj/format-region-or-buffer in
;;  custom-functions.el

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'custom-functions)


;; ----------------------------------- Tests -----------------------------------

(defvar test-format-rob-text-data
  '(("    spaces in front\nspaces behind   " .
	 "spaces in front\nspaces behind")
	("\t    tabs and spaces in front\ntabs and spaces behind\t   " .
	 "tabs and spaces in front\ntabs and spaces behind")))

(defvar test-format-rob-elisp-data
  '(("(defun existential ()\n(if (eq (+ 3 4) 7)\n(order)\n(chaos)))" .
	  "(defun existential ()\n  (if (eq (+ 3 4) 7)\n   (order)\n (chaos)))")))


(ert-deftest test-format-rob-positive-text-region ()
  "Test cj/format-region-or-buffer on a selected region.
This tests "
  (dolist (data-pair test-format-rob-text-data)
	(let* ((testdata (car data-pair))
		   (expected (cdr data-pair))
		   (actual))
	  (with-temp-buffer
		(insert testdata)
		(goto-char (point-min))
		(set-mark (point))
		(goto-char (point-max))
		(cj/format-region-or-buffer)
		(setq actual (buffer-string))
		(should (string= actual expected))))))

(ert-deftest test-format-rob-positive-text-buffer ()
  "Test cj/format-region-or-buffer on the entire buffer.
This is the same as testing the region without setting a region in the temp
buffer."
  (dolist (data-pair test-format-rob-text-data)
	(let* ((testdata (car data-pair))
		   (expected (cdr data-pair))
		   (actual))
	  (with-temp-buffer
		(insert testdata)
		(cj/format-region-or-buffer)
		(setq actual (buffer-string))
		(should (string= actual expected))))))

(ert-deftest test-format-rob-positive-region-text-multiple-paragraphs ()
  "Test cj/format-region-or-buffer on the entire buffer."
  (dolist (data-pair test-format-rob-text-data)
	(let ((testdata (car data-pair))
		  (expected1 (cdr data-pair))
		  (expected2 (car data-pair))
		  (actual1)
		  (actual2))
	  (with-temp-buffer
		;; insert data twice with newline char in between
		(insert testdata)
		(insert"\n")
		(insert testdata)

		;; select the first set of data
		(goto-char (point-min))
		(set-mark (point))
		(forward-line 2)

		;; run format and return to top
		(cj/format-region-or-buffer)
		(message "buffer is:\n'%s'" (buffer-string))

		;; assert the first set is formatted
		(goto-char (point-min))
		(setq actual1 (buffer-substring (point-min) (line-end-position 2)))
		(should (string= actual1 expected1))

		;; assert the second set is unformatted
		(goto-char (point-min))
		(setq actual2 (buffer-substring (line-beginning-position 3) (point-max)))
		(should (string= actual2 expected2))))))

(ert-deftest test-format-rob-positive-elisp-region ()
  "Test cj/format-region-or-buffer on a selected region.
This tests that emacs-lisp specific formatting is applied."
  (ws-butler-mode nil)
  (dolist (data-pair test-format-rob-elisp-data)
	(let* ((testdata (car data-pair))
		   (expected (cdr data-pair))
		   (actual))
	  (with-temp-buffer
		(emacs-lisp-mode)
		(insert testdata)
		(goto-char (point-min))
		(set-mark (point))
		(goto-char (point-max))
		(message "buffer before:\n'%s'" (buffer-string))
		(cj/format-region-or-buffer)
		(message "buffer after:\n'%s'" (buffer-string))
		(setq actual (buffer-string))
		(should (string= actual expected))))))

(provide 'test-format-region)
;;; test-format-region.el ends here.
