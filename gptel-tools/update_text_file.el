;;; update_text_file.el --- Update text files for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-tool-writer
;; Keywords: convenience, tools

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

;; This file provides a gptel tool for updating text files with various
;; operations including replace, append, prepend, insert-at-line, and
;; delete-lines. The tool creates timestamped backups and shows diffs
;; before applying changes.

;;; Code:

(require 'gptel)
(require 'subr-x)

;; Helper function for building sed commands
(defun cj/build-sed-command (operation pattern replacement line-num temp-file)
  "Build appropriate sed/shell command for OPERATION."
  (pcase operation
	("replace"
	 (unless (and pattern replacement)
	   (error "Replace operation requires pattern and replacement"))
	 (format "sed -i 's|%s|%s|g' '%s'"
			 (replace-regexp-in-string "|" "\\\\\\\\|" pattern)
			 (replace-regexp-in-string "|" "\\\\\\\\|" replacement)
			 temp-file))
	("append"
	 (unless pattern
	   (error "Append operation requires text to append"))
	 (format "printf '%%s\\\\n' %s >> '%s'"
			 (shell-quote-argument pattern)
			 temp-file))
	("prepend"
	 (unless pattern
	   (error "Prepend operation requires text to prepend"))
	 (format "(printf '%%s\\\\n' %s; cat '%s') > '%s.new' && mv '%s.new' '%s'"
			 (shell-quote-argument pattern)
			 temp-file temp-file temp-file temp-file))
	("insert-at-line"
	 (unless (and pattern line-num)
	   (error "Insert-at-line requires text and line number"))
	 (format "sed -i '%di\\\\%s' '%s'"
			 line-num
			 (replace-regexp-in-string "/" "\\\\\\\\/" pattern)
			 temp-file))
	("delete-lines"
	 (unless pattern
	   (error "Delete-lines requires pattern"))
	 (format "sed -i '/%s/d' '%s'"
			 (replace-regexp-in-string "/" "\\\\\\\\/" pattern)
			 temp-file))
	(_
	 (error "Unknown operation: %s" operation))))

;; Main tool definition
(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "update_text_file"
   :function (lambda (path operation &optional pattern replacement line-num)
			   (let* ((full-path (expand-file-name path "~"))
					  (temp-file (make-temp-file "gptel-update-" nil ".tmp"))
					  (backup-name (format "%s-%s.bak"
										   full-path
										   (format-time-string "%Y-%m-%d-%H%M%S"))))
					 (unwind-protect
						 (progn
						   ;; Validate path
						   (unless (string-prefix-p (expand-file-name "~") full-path)
							 (error "Path must be within home directory"))
						   (unless (file-exists-p full-path)
							 (error "File not found: %s" full-path))
						   (unless (file-readable-p full-path)
							 (error "No read permission for file: %s" full-path))
						   ;; Check file size
						   (let ((size (file-attribute-size (file-attributes full-path))))
							 (when (> size (* 10 1024 1024))
							   (error "File too large (%s): exceeds 10MB limit"
									  (file-size-human-readable size))))
						   ;; Create backup
						   (copy-file full-path backup-name t)
						   ;; Copy to temp file for operations
						   (copy-file full-path temp-file t)
						   ;; Execute operation and check diff
						   (let* ((sed-cmd (cj/build-sed-command operation pattern replacement line-num temp-file))
								  (result (shell-command-to-string sed-cmd))
								  (diff-output (shell-command-to-string
												(format "diff -u '%s' '%s' 2>/dev/null" full-path temp-file))))
								 (if (string-empty-p diff-output)
									 (progn
									   (delete-file backup-name)
									   (format "No changes made to %s" full-path))
								   (if (y-or-n-p (format "Apply these changes to %s?\\n\\n%s\\n"
														 full-path diff-output))
									   (progn
										 (copy-file temp-file full-path t)
										 (format "Updated %s (backup: %s)"
												 full-path (file-name-nondirectory backup-name)))
									 (progn
									   (delete-file backup-name)
									   (error "Update cancelled by user"))))))
					   ;; Cleanup temp file
					   (when (file-exists-p temp-file)
						 (delete-file temp-file)))))
   :description "Update a text file with various operations: replace, append, prepend, insert-at-line, or delete-lines. Shows diff before applying changes and creates timestamped backups."
   :args (list '(:name "path"
					   :type string
					   :description "File path relative to home directory, e.g., 'documents/myfile.txt' or '~/documents/myfile.txt'")
			   '(:name "operation"
					   :type string
					   :enum ["replace" "append" "prepend" "insert-at-line" "delete-lines"]
					   :description "The type of update operation to perform")
			   '(:name "pattern"
					   :type string
					   :description "For replace/delete: pattern to match. For append/prepend/insert: text to add"
					   :optional t)
			   '(:name "replacement"
					   :type string
					   :description "For replace operation: the replacement text"
					   :optional t)
			   '(:name "line_num"
					   :type integer
					   :description "For insert-at-line operation: the line number where to insert"
					   :optional t))
   :category "filesystem"
   :confirm t
   :include t))

;; Automatically add to gptel-tools on load
(add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "update_text_file")))


(provide 'update_text_file)
;;; update_text_file.el ends here"
