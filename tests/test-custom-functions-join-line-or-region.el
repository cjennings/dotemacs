;;; test-custom-functions-join-line-or-region.el --- Test cj/join-line-or-region -*- lexical-binding: t; -*-
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Tests for the cj/join-line-or-region function in custom-functions.el

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'ert)
(require 'custom-functions)


(ert-deftest test-cj/join-line-or-region-normal-case ()
  (let* ((given  "Line1\nLine2\nLine3\n")
		 (expected "Line1 Line2 Line3\n"))  ; Note: join-line adds newline.
	(with-temp-buffer
	  (insert given)

	  ;; Properly set and activate the region
	  (push-mark (point-min) t t)  ; Set mark, no message, activate
	  (goto-char (point-max))      ; This creates active region from min to max

	  ;; Call the function being tested
	  (cj/join-line-or-region)

	  ;; Perform assertions to check the expected result
	  (should (equal (buffer-substring-no-properties (point-min) (point-max))
					 expected)))))

(ert-deftest test-cj/join-line-or-region-multiple-spaces ()
  (let* ((given  "Line1\n\n\n\n\nLine2\nLine3\n")
		 (expected "Line1 Line2 Line3\n"))  ; Note: join-line adds newline.
	(with-temp-buffer
	  (insert given)

	  ;; Properly set and activate the region
	  (push-mark (point-min) t t)
	  (goto-char (point-max))

	  ;; Call the function being tested
	  (cj/join-line-or-region)

	  ;; Perform assertions to check the expected result
	  (should (equal (buffer-substring-no-properties (point-min) (point-max))
					 expected)))))


(ert-deftest test-cj/join-line-or-region-single-line ()
  (let* ((given  "Line1\n")
		 (expected "Line1\n"))  ; Note: join-line adds newline.
	(with-temp-buffer
	  (insert given)

	  ;; push the mark mid-way on the line
	  (goto-char (/ (point-max) 2))

	  ;; Call the function being tested
	  (cj/join-line-or-region)

	  ;; Perform assertions to check the expected result
	  (should (equal (buffer-substring-no-properties (point-min) (point-max))
					 expected)))))

(ert-deftest test-cj/join-line-or-region-nothing ()
  (let* ((given  "")
		 (expected "\n"))  ; Note: join-line adds newline.
	(with-temp-buffer
	  (insert given)

	  ;; Properly set and activate the region
	  (push-mark (point-min) t t)
	  (goto-char (point-max))

	  ;; Call the function being tested
	  (cj/join-line-or-region)

	  ;; Perform assertions to check the expected result
	  (should (equal (buffer-substring-no-properties (point-min) (point-max))
					 expected)))))


(provide 'test-custom-functions.el-join-line-or-region)
;;; test-custom-functions-join-line-or-region.el ends here.
