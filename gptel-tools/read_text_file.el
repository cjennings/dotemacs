;;; read_text_file.el --- Read text files for GPTel -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-tool-writer
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))

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

;;; Code:

;; Helper functions for read_text_file tool
(defun cj/validate-file-path (path)
  "Validate PATH is within home directory and exists."
  (let ((full-path (expand-file-name path "~")))
	(unless (string-prefix-p (expand-file-name "~") full-path)
	  (error "Path must be within home directory"))
	(unless (file-exists-p full-path)
	  (error "File not found: %s" full-path))
	(when (file-directory-p full-path)
	  (error "Path is a directory, not a file: %s" full-path))
	(unless (file-readable-p full-path)
	  (error "No read permission for file: %s" full-path))
	;; Follow symlinks
	(if (file-symlink-p full-path)
		(file-truename full-path)
	  full-path)))

(defun cj/get-file-metadata (path)
  "Return formatted metadata string for file at PATH."
  (let* ((attributes (file-attributes path))
		 (size (file-attribute-size attributes))
		 (modes (file-attribute-modes attributes))
		 (modtime (format-time-string "%Y-%m-%d"
									  (file-attribute-modification-time attributes))))
	(list :size size
		  :string (format "File: %s (%s, %s, modified %s)"
						  path modes
						  (file-size-human-readable size)
						  modtime))))

(defun cj/check-file-size-limits (size no-confirm)
  "Check file SIZE against limits, prompting user unless NO-CONFIRM."
  (let ((size-warning-limit (* 10 1024 1024))  ; 10MB
		(size-hard-limit (* 100 1024 1024)))   ; 100MB
	(when (> size size-hard-limit)
	  (error "File too large (%s): exceeds 100MB limit"
			 (file-size-human-readable size)))
	(when (and (> size size-warning-limit)
			   (not no-confirm))
	  (unless (y-or-n-p (format "File is large (%s). Continue? "
								(file-size-human-readable size)))
		(error "File read cancelled: size exceeds 10MB")))))

(defun cj/detect-binary-file (path)
  "Check if file at PATH appears to be binary."
  (with-temp-buffer
	(insert-file-contents path nil 0 1024)
	(goto-char (point-min))
	(search-forward "\0" nil t)))

(defun cj/handle-special-file-types (path no-confirm)
  "Handle PDF, EPUB, and other binary files at PATH."
  (cond
   ((string-match-p "\\.pdf\\'" path)
	(when (and (not no-confirm)
			   (not (y-or-n-p "This is a PDF file. Extract text for LLM (y) or cancel (n)? ")))
	  (error "PDF file read cancelled"))
	;; Extract text from PDF
	(let ((text (shell-command-to-string
				 (format "pdftotext '%s' -" path))))
	  (if (string-empty-p text)
		  (error "Could not extract text from PDF: %s" path)
		text)))
   ((string-match-p "\\.epub\\'" path)
	(when (and (not no-confirm)
			   (not (y-or-n-p "This is an EPUB file. Extract text for LLM (y) or cancel (n)? ")))
	  (error "EPUB file read cancelled"))
	(error "EPUB text extraction not yet implemented"))
   (t
	(when (and (not no-confirm)
			   (not (y-or-n-p "This appears to be a binary file. Read anyway? ")))
	  (error "Binary file read cancelled"))
	nil)))  ; Return nil to indicate normal read

;; Main tool function using the helpers
(gptel-make-tool
 :name "read_text_file"
 :function (lambda (path &optional no-confirm)
			 (let* ((full-path (cj/validate-file-path path))
					(metadata (cj/get-file-metadata full-path))
					(size (plist-get metadata :size))
					(metadata-string (plist-get metadata :string)))
			   ;; Show metadata and confirm
			   (unless no-confirm
				 (unless (y-or-n-p (format "%s\nRead this file? " metadata-string))
				   (error "File read cancelled by user")))
			   ;; Check size limits
			   (cj/check-file-size-limits size no-confirm)
			   ;; Handle binary/special files
			   (let ((content
					  (if (cj/detect-binary-file full-path)
						  (or (cj/handle-special-file-types full-path no-confirm)
							  ;; If not a special type or user wants to read anyway
							  (with-temp-buffer
								(insert-file-contents full-path)
								(buffer-string)))
						;; Normal text file
						(with-temp-buffer
						  (insert-file-contents full-path)
						  (buffer-string)))))
				 (format "Read %d bytes from %s\n\n%s"
						 (length content) full-path content))))
 :description "Read text content from a file within the user's home directory. Shows file metadata and requests confirmation before reading. Handles large files, binary detection, and PDF text extraction."
 :args (list '(:name "path"
					 :type string
					 :description "File path relative to home directory, e.g., 'documents/myfile.txt' or '~/documents/myfile.txt'")
			 '(:name "no_confirm"
					 :type boolean
					 :description "If true, skip confirmation prompts and read immediately"
					 :optional t))
 :category "filesystem"
 :confirm t
 :include t)

;; Automatically add to gptel-tools on load
(add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "read_text_file")))


(provide 'read_text_file)
;;; read_text_file.el ends here.
