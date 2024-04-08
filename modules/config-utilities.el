;;; config-utilities  --- Config Hacking Utilities -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Convenience utilities for working on Emacs configuration.

;;; Code:

;; ------------------------------ Reload Init File -----------------------------
;; it does what it says it does.

(defun cj/reload-init-file ()
  "Reload the init file.  Useful when modifying Emacs config."
  (interactive)
  (load-file user-init-file))

;; ---------------------------- Recompile Emacs Home ---------------------------
;; deletes all .elc and .eln files in user-emacs-directory, then compiles
;; all emacs-lisp files natively if supported, or byte-compiles them if not.

(defun cj/recompile-emacs-home()
  "Delete all compiled files in Emacs home recursively before recompilation.
Will recompile natively if supported, or byte-compiled if not."
  (interactive)
  (let* ((native-comp-supported (boundp 'native-compile-async))
		 (elt-dir (expand-file-name (if native-comp-supported "eln" "elc") user-emacs-directory))
		 (message-format (format "Please confirm recursive %s recompilation of %%s: " (if native-comp-supported "native" "byte")))
		 (compile-message (format "%scompiling all emacs-lisp files in %%s" (if native-comp-supported "Natively " "Byte-"))))
	(if (yes-or-no-p (format message-format user-emacs-directory))
		(progn
		  (message "Deleting all compiled files in %s" user-emacs-directory)
		  (dolist (file (directory-files-recursively user-emacs-directory "\\(\\.elc\\|\\.eln\\)$"))
			(delete-file file))
		  (when (file-directory-p elt-dir)
			(delete-directory elt-dir t t))
		  (message compile-message user-emacs-directory)
		  (if native-comp-supported
			  (let ((comp-async-report-warnings-errors nil))
				(native-compile-async user-emacs-directory 'recursively))
			(byte-recompile-directory user-emacs-directory 0)))
	  (message "Cancelled recompilation of %s" user-emacs-directory))))


;; ---------------------- Delete Emacs Home Compiled Files ---------------------
;; removes all compiled files and deletes the eln directory

(defun cj/delete-emacs-home-compiled-files ()
  "Delete all compiled files recursively in \='user-emacs-directory\='."
  (interactive)
  (message "Deleting compiled files under %s. This may take a while." user-emacs-directory)
  (require 'find-lisp)    ;; make sure the package is required
  (mapc (lambda (path)
		  (when (or (string-suffix-p ".elc" path)
					(string-suffix-p ".eln" path))
			(delete-file path)))
        (find-lisp-find-files user-emacs-directory ""))
  (message "Done. Compiled files removed under %s" user-emacs-directory))


;; ---------------------- List Loaded Packages ---------------------
;; you don't really need an explanation for this function, do you?

(defvar cj--loaded-file-paths nil
  "All file paths that are loaded.")
(defvar cj--loaded-packages-buffer "*loaded-packages*"
  "Buffer name for data about loaded packages.")
(defvar cj--loaded-features-buffer "*loaded-features*"
  "Buffer name for data about loaded features.")

(defun cj/list-loaded-packages()
  "List all currently loaded packages."
  (interactive)
  (with-current-buffer (get-buffer-create cj--loaded-packages-buffer)
	(erase-buffer)
	(pop-to-buffer (current-buffer))

	(insert "* Live Packages Exploration\n\n")
	(insert (format "%s total packages currently loaded\n"
					(length cj--loaded-file-paths)))

	;; Extract data from builtin variable `load-history'.
	(setq cj--loaded-file-paths
		  (seq-filter #'stringp
					  (mapcar #'car load-history)))
	(cl-sort cj--loaded-file-paths 'string-lessp)
	(cl-loop for file in cj--loaded-file-paths
			 do (insert "\n" file))

	(goto-char (point-min))))

;; ---------------------------- List Loaded Features ---------------------------
;; this function's also self-explanatory

(defun cj/list-loaded-features()
  "List all currently loaded features."
  (interactive)
  (with-current-buffer (get-buffer-create cj--loaded-features-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))

    (insert (format "\n** %d features currently loaded\n"
                    (length features)))

    (let ((features-vec (apply 'vector features)))
      (cl-sort features-vec 'string-lessp)
      (cl-loop for x across features-vec
               do (insert (format "  - %-25s: %s\n" x
                                  (locate-library (symbol-name x))))))
    (goto-char (point-min))))

(provide 'config-utilities)
;;; config-utilities.el ends here
