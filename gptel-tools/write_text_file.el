;;; write_text_file.el --- Write text files for gptel  -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Gptel tool for writing a text file under the user's home directory.
;; Creates parent directories as needed, optionally overwrites an
;; existing file (with a timestamped backup), and rejects writes
;; larger than 1 GB unless the user confirms.

;;; Code:

(require 'gptel)

(defconst cj/write-text-file--size-limit (* 1024 1024 1024)
  "Soft cap for new-file writes (1 GB).  Above this size a confirm is required.")

(defun cj/write-text-file--validate-path (path)
  "Validate PATH for write.  Return the expanded path on success.
PATH must resolve inside the user's home directory."
  (let ((full (expand-file-name path "~")))
    (unless (string-prefix-p (expand-file-name "~") full)
      (error "Path must be within home directory: %s" path))
    full))

(defun cj/write-text-file--backup-name (path)
  "Return a timestamped backup filename for PATH."
  (format "%s-%s.bak"
          path
          (format-time-string "%Y-%m-%d-%H%M%S")))

(defun cj/write-text-file--ensure-parent (path)
  "Ensure the parent directory of PATH exists and is writable.
Create missing parents.  Signal on failure."
  (let ((parent (file-name-directory path)))
    (when parent
      (unless (file-exists-p parent)
        (condition-case err
            (make-directory parent t)
          (error (error "Cannot create directory %s: %s"
                        parent (error-message-string err)))))
      (unless (file-writable-p parent)
        (error "No write permission for directory %s" parent)))))

(defun cj/write-text-file--run (path content &optional overwrite)
  "Write CONTENT to PATH.  Return a status string.
PATH must be inside the user's home directory.  If the file exists
and OVERWRITE is non-nil, make a timestamped backup before writing;
otherwise signal."
  (let* ((full (cj/write-text-file--validate-path path))
         (content (or content ""))
         (size (length content)))
    (when (> size cj/write-text-file--size-limit)
      (unless (y-or-n-p (format "File is %s. Write anyway? "
                                (file-size-human-readable size)))
        (error "File write cancelled: size exceeds 1GB limit")))
    (cj/write-text-file--ensure-parent full)
    (when (file-exists-p full)
      (if overwrite
          (let ((backup (cj/write-text-file--backup-name full)))
            (copy-file full backup t)
            (message "Backed up existing file to %s" backup))
        (error "File %s already exists. Set overwrite to true to replace it" full)))
    (with-temp-file full (insert content))
    (format "Successfully wrote %d bytes to %s" size full)))

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "write_text_file"
   :function (lambda (path content &optional overwrite)
               (cj/write-text-file--run path content overwrite))
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

  (add-to-list 'gptel-tools (gptel-get-tool '("filesystem" "write_text_file"))))

(provide 'write_text_file)
;;; write_text_file.el ends here
