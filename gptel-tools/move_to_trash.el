;;; move_to_trash.el --- Move files/directories to trash for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-tool-writer
;; Keywords: convenience, tools, files

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This file provides a gptel tool for moving files and directories to the trash.
;; Files are moved to ~/.local/share/Trash/files with automatic timestamping for
;; name conflicts. The tool operates only within the home directory and /tmp
;; for security reasons.

;;; Code:

(require 'gptel)
(require 'subr-x)

(defun gptel--move-to-trash-generate-unique-name (original-name trash-dir)
  "Generate a unique name for ORIGINAL-NAME in TRASH-DIR.
If a file with the same name exists, append a timestamp in the format
YYYY-MM-DD-HH-MM-SS."
  (let* ((base-name (file-name-nondirectory original-name))
		 (target-path (expand-file-name base-name trash-dir)))
	(if (not (file-exists-p target-path))
		target-path
	  ;; Name conflict: add timestamp
	  (let* ((extension (file-name-extension base-name t))
			 (name-sans-ext (file-name-sans-extension base-name))
			 (timestamp (format-time-string "%Y-%m-%d-%H-%M-%S"))
			 (new-name (if extension
						  (concat name-sans-ext "-" timestamp extension)
						(concat base-name "-" timestamp))))
		(expand-file-name new-name trash-dir)))))

(defun gptel--move-to-trash-validate-path (path)
  "Validate that PATH is safe to trash.
Returns the expanded path if valid, signals an error otherwise.
Ensures path is within home directory or /tmp, and prevents
trashing of critical system directories."
  (let ((expanded-path (expand-file-name path))
		(home-dir (expand-file-name "~"))
		(critical-dirs (list (expand-file-name "~")
							(expand-file-name "~/.emacs.d")
							(expand-file-name "~/.config")
							"/tmp")))
	;; Security check: must be within allowed directories
	(unless (or (string-prefix-p home-dir expanded-path)
				(string-prefix-p "/tmp" expanded-path))
	  (error "Path must be within home directory or /tmp: %s" path))

	;; Prevent trashing critical directories
	(when (member expanded-path critical-dirs)
	  (error "Cannot trash critical directory: %s" path))

	;; Existence check
	(unless (file-exists-p expanded-path)
	  (error "File or directory does not exist: %s" path))

	expanded-path))

(defun gptel--move-to-trash-perform (expanded-path trash-dir)
  "Move EXPANDED-PATH to TRASH-DIR with unique naming.
Returns a formatted message describing the operation."
  (let* ((is-directory (file-directory-p expanded-path))
		 (is-symlink (file-symlink-p expanded-path))
		 (trash-path (gptel--move-to-trash-generate-unique-name
					 expanded-path trash-dir))
		 (item-type (cond
					 (is-symlink "Symlink")
					 (is-directory "Directory")
					 (t "File"))))

	;; Perform the move
	(condition-case move-err
		(progn
		  (rename-file expanded-path trash-path)

		  ;; Verify success
		  (cond
		   ((file-exists-p expanded-path)
			(error "Failed to move %s to trash - file still exists at original location"
				   expanded-path))
		   ((not (file-exists-p trash-path))
			(error "Move operation failed - file not found in trash"))
		   (t
			(format "%s moved to trash: %s → %s"
					item-type
					(abbreviate-file-name expanded-path)
					(file-name-nondirectory trash-path)))))
	  (permission-denied
	   (error "Permission denied: cannot move %s to trash" expanded-path))
	  (error
	   (error "Failed to move %s to trash: %s"
			  expanded-path (error-message-string move-err))))))

;; Main tool definition
(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "move_to_trash"
   :function (lambda (path)
			   "Move PATH to the trash directory.
Creates the trash directory if needed, handles naming conflicts,
and provides detailed error messages."
			   (condition-case err
				   (let* ((trash-dir (expand-file-name "~/.local/share/Trash/files"))
						  (expanded-path (gptel--move-to-trash-validate-path path)))

					 ;; Ensure trash directory exists
					 (unless (file-exists-p trash-dir)
					   (make-directory trash-dir t))

					 ;; Move and return status message
					 (gptel--move-to-trash-perform expanded-path trash-dir))
				 (error
				  (error "Tool error: %s" (error-message-string err)))))
   :description "Move a file or directory to the trash (~/.local/share/Trash/files). Works recursively for directories. Handles name conflicts with timestamps. Operates only within home directory and /tmp. Does not follow symlinks. Synonyms: delete, remove, trash file/directory."
   :args (list '(:name "path"
					   :type string
					   :description "Path to the file or directory to move to trash. Must be within home directory or /tmp."))
   :category "filesystem"
   :confirm nil  ; No confirmation needed
   :include t))

;; Automatically add to gptel-tools on load
(add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "move_to_trash")))

(provide 'move_to_trash)
;;; move_to_trash.el ends here
