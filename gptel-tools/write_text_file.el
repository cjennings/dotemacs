;;; write_text_file.el --- Write text files for gptel  -*- lexical-binding: t; -*-

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

;; This file provides a gptel tool for writing text files to the filesystem.
;; The tool includes safety features like backup creation, size limits,
;; and restriction to the user's home directory.

;;; Code:

(require 'gptel)

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "write_text_file"
   :function (lambda (path content &optional overwrite)
               (let* ((full-path (expand-file-name path "~"))
                      (content (or content ""))
                      (content-size (length content))
                      (size-limit (* 1024 1024 1024))) ; 1 GB
                 ;; Check if path is within home directory
                 (unless (string-prefix-p (expand-file-name "~") full-path)
                   (error "Path must be within home directory"))
                 ;; Check size limit
                 (when (> content-size size-limit)
                   (unless (y-or-n-p (format "File is %s. Write anyway? "
                                            (file-size-human-readable content-size)))
                     (error "File write cancelled: size exceeds 1GB limit")))
                 ;; Check write permission on parent directory
                 (let ((parent-dir (file-name-directory full-path)))
                   (when parent-dir
                     ;; Create parent directories if needed
                     (unless (file-exists-p parent-dir)
                       (condition-case err
                           (make-directory parent-dir t)
                         (error (error "Cannot create directory %s: %s" 
                                      parent-dir (error-message-string err)))))
                     ;; Check write permission
                     (unless (file-writable-p parent-dir)
                       (error "No write permission for directory %s" parent-dir))))
                 ;; Handle existing file
                 (when (file-exists-p full-path)
                   (if overwrite
                       ;; Create backup with timestamp
                       (let* ((backup-name 
                              (format "%s-%s.bak" 
                                     full-path
                                     (format-time-string "%Y-%m-%d-%H%M%S"))))
                         (copy-file full-path backup-name t)
                         (message "Backed up existing file to %s" backup-name))
                     (error "File %s already exists. Set overwrite to true to replace it" full-path)))
                 ;; Write the file atomically
                 (with-temp-file full-path
                   (insert content))
                 (format "Successfully wrote %d bytes to %s" 
                         content-size full-path)))
   :description "Write text content to a file within the user's home directory. Creates parent directories if needed. Backs up existing files with timestamp when overwriting."
   :args (list '(:name "path"
                 :type string
                 :description "File path relative to home directory, e.g., 'documents/myfile.txt' or '~/documents/myfile.txt'")
               '(:name "content"
                 :type string
                 :description "The text content to write to the file")
               '(:name "overwrite"
                 :type boolean
                 :description "If true, backup and overwrite existing file. If false or omitted, error if file exists"
                 :optional t))
   :category "filesystem"
   :confirm t
   :include t)
  
  ;; Automatically add to gptel-tools on load
  (add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "write_text_file"))))

(provide 'write_text_file)
;;; write_text_file.el ends here