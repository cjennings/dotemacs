;;; test-join-line-or-region.el --- Test Join Line/Region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the join-line-or-region function

;;; Code:x

;; (require 'custom-functions)

(require 'ert)

(defun cj/join-line-or-region (beg end)
  "Apply 'join-line' over the region (identified by BEG and END).
If the region's not active, join line with previous line above."
  (interactive "r")
  ;; when in region
  (if mark-active
	  (message "mark is active")
	  (let ((beg (region-beginning))
			(end (copy-marker (region-end))))
		(goto-char beg)
		;; apply join lines until point => end
		(while (< (point) end)
		  (join-line 1))
		(goto-char end)
		(newline)))
  ;; outside of region; just join line
  (join-line)(newline))


(ert-deftest test-cj/join-line-or-region ()
  (let ((given  "Line1\nLine2\nLine3\n")
		(expected "Line1Line2Line3")
		(beg nil)
		(end nil))
	(with-temp-buffer
	  (insert given)
	  ;; select the whole buffer as region
	  (setq beg (copy-marker (goto-char (point-min))))
	  (setq end (copy-marker (goto-char (point-max))))

	  ;; Call the function being tested
	  (cj/join-line-or-region beg end)

	  ;; Perform assertions to check the expected result
	  (should (equal (buffer-substring-no-properties (point-min)
													 (point-max))
					 expected)))))


(provide 'test-join-line-or-region)
;;; test-join-line-or-region.el ends here.
