;;; test-system-utils--file-from-context.el --- Tests for cj/--file-from-context -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--file-from-context' in system-utils.el.  The
;; helper returns a file path from the current context, resolving in
;; priority order: explicit argument, `buffer-file-name', dired file
;; at point.  Returns nil when none of these yield a file.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-utils)

(defmacro test-ffc--with-context (buffer-file in-dired dired-file &rest body)
  "Run BODY with context stubbed for testing `cj/--file-from-context'.
BUFFER-FILE becomes the value of `buffer-file-name'.
IN-DIRED controls `derived-mode-p' (t to simulate dired-mode).
DIRED-FILE becomes the return value of `dired-file-name-at-point'."
  (declare (indent 3))
  `(let ((buffer-file-name ,buffer-file))
	 (cl-letf (((symbol-function 'derived-mode-p)
				(lambda (&rest modes)
				  (and ,in-dired (memq 'dired-mode modes))))
			   ((symbol-function 'dired-file-name-at-point)
				(lambda () ,dired-file)))
	   ,@body)))

;;; Normal cases

(ert-deftest test-ffc-explicit-wins-over-buffer-file ()
  "Normal: an explicit filename argument wins over `buffer-file-name'."
  (test-ffc--with-context "/from-buffer.el" nil nil
	(should (string= "/explicit.el"
					 (cj/--file-from-context "/explicit.el")))))

(ert-deftest test-ffc-explicit-wins-over-dired ()
  "Normal: an explicit filename argument wins over dired file at point."
  (test-ffc--with-context nil t "/from-dired.el"
	(should (string= "/explicit.el"
					 (cj/--file-from-context "/explicit.el")))))

(ert-deftest test-ffc-buffer-file-used-when-no-explicit ()
  "Normal: falls back to `buffer-file-name' when no explicit arg."
  (test-ffc--with-context "/from-buffer.el" nil nil
	(should (string= "/from-buffer.el"
					 (cj/--file-from-context)))))

(ert-deftest test-ffc-dired-used-when-no-explicit-no-buffer-file ()
  "Normal: in a dired buffer, falls back to dired file at point."
  (test-ffc--with-context nil t "/from-dired.el"
	(should (string= "/from-dired.el"
					 (cj/--file-from-context)))))

;;; Boundary cases

(ert-deftest test-ffc-all-sources-nil-returns-nil ()
  "Boundary: no explicit, no buffer-file, not in dired → nil."
  (test-ffc--with-context nil nil nil
	(should-not (cj/--file-from-context))))

(ert-deftest test-ffc-explicit-nil-uses-fallback-chain ()
  "Boundary: explicitly passing nil as the arg still uses the fallback chain."
  (test-ffc--with-context "/from-buffer.el" nil nil
	(should (string= "/from-buffer.el"
					 (cj/--file-from-context nil)))))

(ert-deftest test-ffc-dired-mode-but-no-file-at-point ()
  "Boundary: in dired but nothing at point returns nil (buffer-file also nil)."
  (test-ffc--with-context nil t nil
	(should-not (cj/--file-from-context))))

(provide 'test-system-utils--file-from-context)
;;; test-system-utils--file-from-context.el ends here
