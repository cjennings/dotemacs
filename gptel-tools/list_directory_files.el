;;; list_directory_files.el --- List directory files for GPTel -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-tool-writer
;; Keywords: convenience, tools
;; Version: 2.0.0

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

;; This tool provides a comprehensive directory listing function for use within gptel.
;; It lists files and directories with detailed attributes such as size, last modification time,
;; permissions, and executable status, supporting optional recursive traversal and filtering
;; by file extension.
;;
;; Features:
;; - Lists files with Unix-style permissions, size, and modification date
;; - Optional recursive directory traversal
;; - Filter files by extension
;; - Graceful error handling and reporting
;; - Human-readable file sizes and dates

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; Helper Functions

(defun list-directory-files--mode-to-permissions (mode)
  "Convert numeric MODE to symbolic Unix style permissions string."
  (concat
   (if (eq (logand #o40000 mode) #o40000) "d" "-")
   (mapconcat
    (lambda (bit)
      (cond ((eq bit ?r) (if (> (logand mode #o400) 0) "r" "-"))
            ((eq bit ?w) (if (> (logand mode #o200) 0) "w" "-"))
            ((eq bit ?x) (if (> (logand mode #o100) 0) "x" "-"))))
    '(?r ?w ?x) "")
   (mapconcat
    (lambda (bit)
      (cond ((eq bit ?r) (if (> (logand mode #o040) 0) "r" "-"))
            ((eq bit ?w) (if (> (logand mode #o020) 0) "w" "-"))
            ((eq bit ?x) (if (> (logand mode #o010) 0) "x" "-"))))
    '(?r ?w ?x) "")
   (mapconcat
    (lambda (bit)
      (cond ((eq bit ?r) (if (> (logand mode #o004) 0) "r" "-"))
            ((eq bit ?w) (if (> (logand mode #o002) 0) "w" "-"))
            ((eq bit ?x) (if (> (logand mode #o001) 0) "x" "-"))))
    '(?r ?w ?x) "")))

(defun list-directory-files--get-file-info (filepath)
  "Get file information for FILEPATH as a plist."
  (condition-case err
      (let* ((attrs (file-attributes filepath 'string))
             (size (file-attribute-size attrs))
             (last-mod (file-attribute-modification-time attrs))
             (dirp (eq t (file-attribute-type attrs)))
             (mode (file-modes filepath))
             (perm (list-directory-files--mode-to-permissions mode))
             (execp (file-executable-p filepath)))
        (list :success t
              :path filepath
              :size size
              :last-modified last-mod
              :is-directory dirp
              :permissions perm
              :executable execp))
    (error
     (list :success nil
           :path filepath
           :error (error-message-string err)))))

(defun list-directory-files--filter-by-extension (extension)
  "Create a filter function for files with EXTENSION."
  (when extension
    (lambda (file-info)
      (or (plist-get file-info :is-directory)  ; Always include directories
          (and (plist-get file-info :success)
               (string-suffix-p (concat "." extension)
                              (file-name-nondirectory (plist-get file-info :path))
                              t))))))

(defun list-directory-files--format-file-entry (file-info base-path)
  "Format a single FILE-INFO entry relative to BASE-PATH."
  (format "  %s%s %10s %s %s"
          (plist-get file-info :permissions)
          (if (plist-get file-info :executable) "*" " ")
          (file-size-human-readable (or (plist-get file-info :size) 0))
          (format-time-string "%Y-%m-%d %H:%M" (plist-get file-info :last-modified))
          (file-relative-name (plist-get file-info :path) base-path)))

;;; Core Implementation

(defun list-directory-files--list-directory (path &optional recursive filter max-depth current-depth)
  "List files in PATH directory.
RECURSIVE enables subdirectory traversal.
FILTER is a predicate function for filtering files.
MAX-DEPTH limits recursion depth (nil for unlimited).
CURRENT-DEPTH tracks the current recursion level."
  (let ((files '())
        (errors '())
        (current-depth (or current-depth 0))
        (expanded-path (expand-file-name (or path ".") "~")))
    
    (if (not (file-directory-p expanded-path))
        ;; Return error if not a directory
        (list :files nil 
              :errors (list (format "Not a directory: %s" expanded-path)))
      ;; Process directory
      (condition-case err
          (dolist (entry (directory-files expanded-path t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
            (let ((info (list-directory-files--get-file-info entry)))
              (if (plist-get info :success)
                  (progn
                    ;; Add file if it passes the filter
                    (when (or (not filter) (funcall filter info))
                      (push info files))
                    ;; Recurse into directories if needed
                    (when (and recursive
                             (plist-get info :is-directory)
                             (or (not max-depth) (< current-depth max-depth)))
                      (let ((subdir-result (list-directory-files--list-directory
                                          entry recursive filter max-depth (1+ current-depth))))
                        (setq files (nconc files (plist-get subdir-result :files)))
                        (setq errors (nconc errors (plist-get subdir-result :errors))))))
                ;; Handle file access error
                (push (format "%s: %s" (plist-get info :path) (plist-get info :error)) errors))))
        (error
         (push (format "Error accessing directory %s: %s" expanded-path (error-message-string err)) errors)))
      
      (list :files (nreverse files) :errors (nreverse errors)))))

(defun list-directory-files--format-output (path result)
  "Format the directory listing RESULT for PATH as a string."
  (let ((files (plist-get result :files))
        (errors (plist-get result :errors))
        (base-path (expand-file-name "~")))
    (concat
     (when files
       (format "Found %d file%s in %s:\n%s"
               (length files)
               (if (= (length files) 1) "" "s")
               path
               (mapconcat (lambda (f) (list-directory-files--format-file-entry f base-path))
                        files "\n")))
     (when (and files errors) "\n\n")
     (when errors
       (format "Errors encountered:\n%s"
               (mapconcat (lambda (e) (format "  - %s" e)) errors "\n")))
     ;; Handle case where there are no files and no errors
     (when (and (not files) (not errors))
       (format "No files found in %s" path)))))

;;; Tool Registration

(gptel-make-tool
 :name "list_directory_files"
 :function (lambda (path &optional recursive filter-extension)
             "List files in directory PATH.
RECURSIVE enables subdirectory listing.
FILTER-EXTENSION limits results to files with the specified extension."
             (let* ((filter (list-directory-files--filter-by-extension filter-extension))
                    (result (list-directory-files--list-directory path recursive filter)))
               (list-directory-files--format-output (or path ".") result)))
 :description "List files in a directory with detailed attributes. Returns formatted listing with permissions, size, modification time."
 :args (list '(:name "path"
                    :type string
                    :description "Directory path to list (relative to home directory)")
            '(:name "recursive"
                    :type boolean
                    :description "Recursively list subdirectories"
                    :optional t)
            '(:name "filter-extension"
                    :type string
                    :description "Only include files with this extension"
                    :optional t))
 :category "filesystem"
 :confirm nil
 :include t)

;; Automatically add to gptel-tools on load
(add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "list_directory_files")))

(provide 'list_directory_files)
;;; list_directory_files.el ends here