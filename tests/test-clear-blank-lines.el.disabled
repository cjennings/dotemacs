;;; test-clear-blank-lines.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'ert)
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'custom-functions)

(ert-deftest test-cj/clear-blank-lines-region ()
  (let ((testdata "Some\n\n\n\nText")
		(expected  "Some\nText")
		(actual))
	(with-temp-buffer
	  (insert testdata)
	  (cj/clear-blank-lines (point-min) (point-max))
	  (setq actual (buffer-string))
	  (message "buffer is:\n'%s'" actual)
	  (should (string= actual expected)))))

(ert-deftest test-cj/clear-blank-lines-region-multiple-lines ()
  (let ((testdata "Some\n\n\n\nText")
		(expected "Some\n\n\n\nText")
		(midpoint)
		(actual))
	(with-temp-buffer
	  (insert testdata)
	  (insert "\n")
	  (setq midpoint (point))
	  (insert testdata)
      (cj/clear-blank-lines (point-min) midpoint)
      (setq actual (buffer-substring (- (point-max)
                                        (length testdata)) (point-max)))
      (message "buffer is:\n'%s'" (buffer-string))
	  (should (string= actual expected)))))

(ert-deftest test-cj/clear-blank-lines-negative ()
  (with-temp-buffer
	(insert "Some\nText")
	(cj/clear-blank-lines (point-min) (point-max))
	(should (equal (buffer-string) "Some\nText"))))


(provide 'test-clear-blank-lines)
;;; test-clear-blank-lines.el ends here.
