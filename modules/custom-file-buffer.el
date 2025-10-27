;;; custom-file-buffer.el --- Custom Buffer and File Operations -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;; This module provides custom buffer and file operations including PostScript
;; printing capabilities.
;;
;; Functions include:
;; - printing buffers or regions as PostScript to the default printer (with color support)
;; - moving/renaming/deleting buffer files
;; - copying file paths and file:// links to the kill ring
;; - copying entire buffer contents
;; - clearing buffer contents from point to top or bottom.
;;
;; The PostScript printing auto-detects the system print spooler (lpr or lp)
;; and prints with face/syntax highlighting. Bound to keymap prefix ~C-; b~.
;;
;;; Code:

;; cj/custom-keymap defined in keybindings.el
(eval-when-compile (defvar cj/custom-keymap))
(eval-when-compile (require 'ps-print)) ;; for ps-print variables
(declare-function ps-print-buffer-with-faces "ps-print")
(declare-function ps-print-region-with-faces "ps-print")

;; ------------------------- Print Buffer As Postscript ------------------------

(defvar cj/print-spooler-command 'auto
  "Command used to send PostScript to the system print spooler.
Set to a string to force a specific command (e.g., lpr or lp). Set to `auto' to
auto-detect once per session.")

(defvar cj/print--spooler-cache nil
  "Cached spooler command detected for the current Emacs session.")

(defun cj/print--resolve-spooler ()
  "Return the spooler command to use, auto-detecting and caching if needed."
  (cond
   ;; User-specified command
   ((and (stringp cj/print-spooler-command)
         (> (length cj/print-spooler-command) 0))
    (or (executable-find cj/print-spooler-command)
        (user-error "Cannot print: spooler command '%s' not found in PATH"
                    cj/print-spooler-command))
    cj/print-spooler-command)
   ;; Auto-detect once per session
   ((eq cj/print-spooler-command 'auto)
    (or cj/print--spooler-cache
        (let ((cmd (or (and (executable-find "lpr") "lpr")
                       (and (executable-find "lp")  "lp"))))
          (unless cmd
            (user-error "Cannot print: neither 'lpr' nor 'lp' found in PATH"))
          (setq cj/print--spooler-cache cmd)
          cmd)))
   (t
    (user-error "Invalid value for cj/print-spooler-command: %S"
                cj/print-spooler-command))))

(defun cj/print-buffer-ps (&optional color)
  "Print the buffer (or active region) as PostScript to the default printer.
With prefix argument COLOR, print in color; otherwise print in monochrome.
Sends directly to the system spooler with no header."
  (interactive "P")
  (unless (require 'ps-print nil t)
    (user-error "Cannot print: ps-print library not found"))
  (let* ((spooler (cj/print--resolve-spooler))
         (want-color (not (null color)))
         (have-region (use-region-p)))
    (let ((ps-lpr-command spooler)
          (ps-printer-name nil)      ; default system printer
          (ps-lpr-switches nil)
          (ps-print-color-p want-color)
          (ps-use-face-background want-color)
          (ps-print-header nil))     ; no headers
      (if have-region
          (ps-print-region-with-faces (region-beginning) (region-end))
        (ps-print-buffer-with-faces)))
    (message "Sent %s to default printer via %s (%s)"
             (if have-region "region" "buffer")
             spooler
             (if want-color "color" "monochrome"))))

;; ------------------------- Buffer And File Operations ------------------------

(defun cj/--move-buffer-and-file (dir &optional ok-if-exists)
  "Internal implementation: Move buffer and file to DIR.
If OK-IF-EXISTS is nil and target exists, signal an error.
If OK-IF-EXISTS is non-nil, overwrite existing file.
Returns t on success, nil if buffer not visiting a file."
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (expand-file-name dir))
         (dir
          (if (string-match "[/\\\\]$" dir)
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (progn
          (message "Buffer '%s' is not visiting a file!" name)
          nil)
      (progn  (copy-file filename newname ok-if-exists)
              (delete-file filename)
              (set-visited-file-name newname)
              (set-buffer-modified-p nil)
              t))))

(defun cj/move-buffer-and-file (dir)
  "Move both current buffer and the file it visits to DIR.
When called interactively, prompts for confirmation if target file exists."
  (interactive (list (read-directory-name "Move buffer and file (to new directory): ")))
  (let* ((target (expand-file-name (buffer-name) (expand-file-name dir))))
    (condition-case _
        (cj/--move-buffer-and-file dir nil)
      (file-already-exists
       (if (yes-or-no-p (format "File %s exists; overwrite? " target))
           (cj/--move-buffer-and-file dir t)
         (message "File not moved"))))))

(defun cj/--rename-buffer-and-file (new-name &optional ok-if-exists)
  "Internal implementation: Rename buffer and file to NEW-NAME.
NEW-NAME can be just a basename or a full path to move to different directory.
If OK-IF-EXISTS is nil and target exists, signal an error.
If OK-IF-EXISTS is non-nil, overwrite existing file.
Returns t on success, nil if buffer not visiting a file."
  (let ((filename (buffer-file-name))
        (new-basename (file-name-nondirectory new-name)))
    (if (not filename)
        (progn
          (message "Buffer '%s' is not visiting a file!" (buffer-name))
          nil)
      ;; Check if a buffer with the new name already exists
      (when (and (get-buffer new-basename)
                 (not (eq (get-buffer new-basename) (current-buffer))))
        (error "A buffer named '%s' already exists" new-basename))
      ;; Expand new-name to absolute path (preserves directory if just basename)
      (let ((expanded-name (expand-file-name new-name
                                              (file-name-directory filename))))
        (rename-file filename expanded-name ok-if-exists)
        (rename-buffer new-basename)
        (set-visited-file-name expanded-name)
        (set-buffer-modified-p nil)
        t))))

(defun cj/rename-buffer-and-file (new-name)
  "Rename both current buffer and the file it visits to NEW-NAME.
When called interactively, prompts for confirmation if target file exists."
  (interactive
   (list (if (not (buffer-file-name))
             (user-error "Buffer '%s' is not visiting a file!" (buffer-name))
           (read-string "Rename buffer and file (to new name): "
                        (file-name-nondirectory (buffer-file-name))))))
  (condition-case err
      (cj/--rename-buffer-and-file new-name nil)
    (file-already-exists
     (if (yes-or-no-p (format "File %s exists; overwrite? " new-name))
         (cj/--rename-buffer-and-file new-name t)
       (message "File not renamed")))
    (error
     ;; Handle buffer-already-exists and other errors
     (message "%s" (error-message-string err)))))

(defun cj/delete-buffer-and-file ()
  "Kill the current buffer and delete the file it visits."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename t)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun cj/copy-link-to-buffer-file ()
  "Copy the full file:// path of the current buffer's source file to the kill ring."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (setq file-path (concat "file://" file-path))
      (kill-new file-path)
      (message "Copied file link to kill ring: %s" file-path))))

(defun cj/copy-path-to-buffer-file-as-kill ()
  "Copy the full path of the current buffer's file to the kill ring.
Signal an error if the buffer is not visiting a file."
  (interactive)
  (let ((path (buffer-file-name)))
    (if (not path)
        (user-error "Current buffer is not visiting a file")
      (kill-new path)
      (message "Copied file path: %s" path)
      path)))

(defun cj/copy-whole-buffer ()
  "Copy the entire contents of the current buffer to the kill ring.
Point and mark are left exactly where they were.  No transient region
is created.  A message is displayed when done."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
    (kill-new contents)
    (message "Buffer contents copied to kill ring")))

(defun cj/clear-to-bottom-of-buffer ()
  "Delete all text from point to the end of the current buffer.
This does not save the deleted text in the kill ring."
  (interactive)
  (delete-region (point) (point-max))
  (message "Buffer contents removed to the end of the buffer."))

(defun cj/clear-to-top-of-buffer ()
  "Delete all text from point to the beginning of the current buffer.
Do not save the deleted text in the kill ring."
  (interactive)
  (delete-region (point) (point-min))
  (message "Buffer contents removed to the beginning of the buffer."))

(defun cj/copy-buffer-name ()
  "Copy current buffer name to kill ring."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied: %s" (buffer-name)))

;; --------------------------- Buffer And File Keymap --------------------------

;; Buffer & file operations prefix and keymap
(defvar-keymap cj/buffer-and-file-map
  :doc "Keymap for buffer and file operations."
  "m" #'cj/move-buffer-and-file
  "r" #'cj/rename-buffer-and-file
  "p" #'cj/print-buffer-ps
  "d" #'cj/delete-buffer-and-file
  "c" #'cj/copy-whole-buffer
  "n" #'cj/copy-buffer-name
  "t" #'cj/clear-to-top-of-buffer
  "b" #'cj/clear-to-bottom-of-buffer
  "x" #'erase-buffer
  "s" #'write-file ;; save as

  "l" #'cj/copy-link-to-buffer-file
  "P" #'cj/copy-path-to-buffer-file-as-kill)
(keymap-set cj/custom-keymap "b" cj/buffer-and-file-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; b" "buffer and file menu"
    "C-; b m" "move file"
    "C-; b r" "rename file"
    "C-; b p" "print to PS"
    "C-; b d" "delete file"
    "C-; b c" "copy buffer"
    "C-; b n" "copy buffer name"
    "C-; b t" "clear to top"
    "C-; b b" "clear to bottom"
    "C-; b x" "erase buffer"
    "C-; b s" "save as"
    "C-; b l" "copy file link"
    "C-; b P" "copy file path"))


(provide 'custom-file-buffer)
;;; custom-file-buffer.el ends here.
