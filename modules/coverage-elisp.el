;;; coverage-elisp.el --- Elisp coverage backend for coverage-core -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Registers the `elisp' coverage backend with `coverage-core'.
;;
;; Detection: a project root with a Makefile / Eask / Cask plus any
;; .el files (either at root or under modules/).  Loose on purpose —
;; `.dir-locals.el' can pin the backend explicitly when the heuristic
;; guesses wrong.
;;
;; :run invokes `make coverage' in a compilation buffer.  On success,
;; the callback is invoked with the LCOV path; on failure, the buffer
;; stays visible for the user to inspect.
;;
;; :lcov-path resolves to `<project-root>/.coverage/lcov.info', which
;; matches the path the Makefile's coverage target writes to.

;;; Code:

(require 'coverage-core)

(use-package undercover
  :defer t)

(defconst cj/--coverage-elisp-lcov-relative-path
  ".coverage/lcov.info"
  "Project-relative path to the LCOV file produced by `make coverage'.")

(defun cj/--coverage-elisp-project-root (&optional root)
  "Return ROOT or fall back to projectile's root or `default-directory'."
  (or root
	  (and (fboundp 'projectile-project-root)
		   (projectile-project-root))
	  default-directory))

(defun cj/--coverage-elisp-detect (root)
  "Return non-nil if ROOT looks like an Elisp project.
The heuristic needs both (a) a Makefile, Eask, or Cask at ROOT and
\(b) any .el files at ROOT or under modules/."
  (and (or (file-exists-p (expand-file-name "Makefile" root))
		   (file-exists-p (expand-file-name "Eask" root))
		   (file-exists-p (expand-file-name "Cask" root)))
	   (or (file-expand-wildcards (expand-file-name "modules/*.el" root))
		   (file-expand-wildcards (expand-file-name "*.el" root)))))

(defun cj/--coverage-elisp-lcov-path (&optional root)
  "Return the absolute path to the LCOV file for ROOT."
  (expand-file-name cj/--coverage-elisp-lcov-relative-path
					(cj/--coverage-elisp-project-root root)))

(defun cj/--coverage-elisp-run (callback)
  "Run `make coverage' asynchronously.
CALLBACK is invoked with the LCOV path when the build finishes
successfully.  On failure, no callback is invoked and the compilation
buffer stays visible so the user can read the error."
  (let* ((default-directory (cj/--coverage-elisp-project-root))
		 (buffer (compilation-start "make coverage" nil
									(lambda (_mode) "*coverage-run*"))))
	(with-current-buffer buffer
	  (add-hook 'compilation-finish-functions
				(lambda (_buf status)
				  (when (string-match-p "^finished" status)
					(funcall callback (cj/--coverage-elisp-lcov-path))))
				nil t))))

(cj/coverage-register-backend
 (list :name 'elisp
	   :detect #'cj/--coverage-elisp-detect
	   :run #'cj/--coverage-elisp-run
	   :lcov-path #'cj/--coverage-elisp-lcov-path))

(provide 'coverage-elisp)
;;; coverage-elisp.el ends here
