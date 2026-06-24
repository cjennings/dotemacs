;;; testutil-filesystem.el ---  -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; This library provides reusable helper functions for GPTel filesystem tools.
;;
;; It uses f.el and core Emacs libraries for path manipulation, directory listing,
;; file info retrieval, filtering, and recursive traversal.
;;
;; Designed to be used by multiple tools that operate on the filesystem.
;;
;;; Code:

(require 'f)
(require 'cl-lib)
(require 'subr-x)

;; Get directory entries in PATH. Returns list of absolute paths.
;; Default excludes hidden files and directories (name begins with dot).
;; Optional INCLUDE-HIDDEN to include hidden entries.
;; Optional FILTER-PREDICATE is a function called on each absolute path to filter.
(defun cj/get--directory-entries (path &optional include-hidden filter-predicate)
  "Return a list of entries (absolute paths) in directory PATH.
Entries exclude '.' and '..'.
By default, hidden entries (starting with '.') are excluded unless
INCLUDE-HIDDEN is non-nil. FILTER-PREDICATE, if non-nil, is a predicate
function called on each entry's absolute path; only entries where it returns
non-nil are included."
  ;; Convert 'path' to an absolute filename string
  (let* ((expanded-path (expand-file-name path))
		 ;; get absolute paths in expanded directory
		 (entries (directory-files expanded-path t nil t))
		 ;; remove "." ".." entries
         (filtered-entries
          (cl-remove-if
           (lambda (entry)
			 (or (member (f-filename entry) '("." ".."))
				 ;; and hidden files include-hidden is non-nil.
                 (and (not include-hidden)
                      (string-prefix-p "." (f-filename entry)))))
           entries)))
	;; apply filtered predicate if provided
    (if filter-predicate
		(seq-filter filter-predicate filtered-entries)
	  ;; retun filtered-entries
      filtered-entries)))

(defun cj/get-file-info (path)
  "Get file information for PATH.
Returned plist keys:
:success       t or nil
:error         string error message if :success is nil
:path          absolute file path (string)
:size          file size (integer)
:last-modified last modification time (time value)
:directory     boolean: t if a directory
:permissions   string with symbolic permissions, e.g. \"drwxr-xr-x\"
:executable    boolean: t if executable file
:owner         string: owner name or UID if name unavailable
:group         string: group name or GID if name unavailable"
  ;; handle errors during evaluation
  (condition-case err
	  (let* ((expanded-path (expand-file-name path)))
		(if (not (file-readable-p expanded-path))
			;; Explicit permission denied check
			(list :success nil :path expanded-path :error
				  (format "Permission denied: %s" expanded-path))
		  (let*
			  ;; t = return string names for uid/gid
			  ((attrs (file-attributes expanded-path t))
			   (size (file-attribute-size attrs))
			   (mod (file-attribute-modification-time attrs))
			   (dirp (eq t (file-attribute-type attrs)))
			   (modes (file-modes expanded-path))
			   (perm (cj/-mode-to-permissions modes))
			   (execp (file-executable-p expanded-path))
			   (owner (file-attribute-user-id attrs))    ; Get owner
			   (group (file-attribute-group-id attrs)))  ; Get group
			(list :success t :path expanded-path :size size :last-modified mod
				  :directory dirp :permissions perm :executable execp
				  :owner (or owner "unknown")
				  :group (or group "unknown")))))
	;; if error, return failure plist with error info
	(error (list :success nil :path path :error (error-message-string err)))))

(defun cj/format-file-info (file-info base-path)
  "Format FILE-INFO plist relative to BASE-PATH as a string.
Handles missing keys gracefully by supplying default values."
  (let ((permissions (or (plist-get file-info :permissions) ""))
		(executable (if (plist-get file-info :executable) "*" " "))
		(size (file-size-human-readable (or (plist-get file-info :size) 0)))
		(last-modified (or (plist-get file-info :last-modified) (current-time)))
		(path (or (plist-get file-info :path) base-path)))
	(format "  %s%s %10s %s %s"
			permissions
			executable
			size
			(format-time-string "%Y-%m-%d %H:%M" last-modified)
			(file-relative-name path base-path))))

;; Convert file mode bits integer to string like ls -l, e.g. drwxr-xr-x
(defun cj/-mode-to-permissions (mode)
  "Convert file MODE (returned by `file-modes') to symbolic permission string."
  (concat
   (if (eq (logand #o40000 mode) #o40000) "d" "-")
   (mapconcat
    (lambda (bits)
      (concat (if (/= 0 (logand bits 4)) "r" "-")
              (if (/= 0 (logand bits 2)) "w" "-")
              (if (/= 0 (logand bits 1)) "x" "-")))
    (list (logand (/ mode 64) 7)
          (logand (/ mode 8) 7)
          (logand mode 7))
    "")))

;; Filter a list of file info plists by extension (case insensitive).
;; Always includes directories.
(defun cj/filter-by-extension (file-info-list extension)
  "Keep only directories and files with EXTENSION from FILE-INFO-LIST.
EXTENSION should not include leading dot, e.g. \"org\"."
  ;; return full list if no extension
  (if (not extension)
	  file-info-list
    (cl-remove-if-not
	 (lambda (fi)
	   ;; always keep directories
	   (or (plist-get fi :directory)
		   ;; and successful file entries
		   (and (plist-get fi :success)
				;; and file extensions that match case-insensitively
                (string-suffix-p (concat "." extension)
                                 (f-filename (plist-get fi :path))
                                 t))))
     file-info-list)))

(defun cj/list-directory-recursive (path &optional include-hidden filter-predicate max-depth)
  "Recursively list files under PATH applying FILTER-PREDICATE.
PATH is the directory to list.
INCLUDE-HIDDEN if non-nil, includes hidden files (those starting with '.').
FILTER-PREDICATE, if non-nil, is a function called on file info plist and
returns non-nil to include file.
MAX-DEPTH limits recursion depth (nil or 0 = unlimited)."
  ;; set up cl-recursive function with path and current depth
  (cl-labels ((recurse (path depth)
				(let ((expanded-path (expand-file-name path))
					  ;; empty list to accumulate file info plists
					  (file-info-list '()))
				  ;; ensure we're working with directories only
				  (when (not (file-directory-p expanded-path))
					(error "Not a directory: %s" expanded-path))

				  ;; loop over each file in the path
				  (dolist (file-entry
						   (cj/get--directory-entries expanded-path include-hidden))
					;; get the metadata for the file
					(let ((file-metadata (cj/get-file-info file-entry)))
					  ;; if retrieving metadata was successful
					  (when (and file-metadata (plist-get file-metadata :success))
						;; if there's no custom filter or it matches, add it to the list
						(when (or (not filter-predicate)
								  (funcall filter-predicate file-metadata))
						  (push file-metadata file-info-list))
						;; if it's a directory and we're not at the max-depth
						(when (and (plist-get file-metadata :directory)
								   (or (not max-depth) (< depth (1- max-depth))))
						  ;; gather all the files and recurse with that file
						  (setq file-info-list
								(nconc file-info-list (recurse file-entry (1+ depth)))))
						;; warn if recursion returned received both a success and error
						(when (and (plist-get file-metadata :success)
								   (plist-get file-metadata :error))
						  (message "Warning: %s" (plist-get file-metadata :error))))))
				  ;; restore the file order (as they were pushed into reverse order)
				  (nreverse file-info-list))))
	;; start recursion at the top level
	(recurse path 0)))

(provide 'testutil-filesystem)
;;; testutil-filesystem.el ends here.
