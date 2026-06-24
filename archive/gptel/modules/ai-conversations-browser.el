;;; ai-conversations-browser.el --- Browse saved GPTel conversations -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Provides `cj/gptel-browse-conversations': a dired-style buffer
;; listing saved conversations in `cj/gptel-conversations-directory'.
;; Each row shows date, time, topic, and a short preview of the most
;; recent message.  Single-key bindings load / delete / rename a
;; conversation in place.
;;
;;   RET, l   Load the conversation under point
;;   d        Delete the conversation under point
;;   r        Rename the conversation under point (renames the file)
;;   g        Refresh the listing
;;   n / p    Move to next / previous row
;;   q        Quit the browser window

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function cj/gptel-load-conversation "ai-conversations" ())
(declare-function cj/gptel--slugify-topic "ai-conversations" (s))
(declare-function cj/gptel--timestamp-from-filename "ai-conversations" (filename))

(defcustom cj/gptel-browser-preview-length 60
  "Number of preview characters shown per row in the browser."
  :type 'integer
  :group 'cj/ai-conversations)

(defconst cj/gptel-browser--buffer-name "*GPTel-Conversations*"
  "Buffer name for the saved-conversations browser.")

(defvar-keymap cj/gptel-browser-mode-map
  :doc "Keymap for `cj/gptel-browser-mode'."
  "RET" #'cj/gptel-browser-load
  "l"   #'cj/gptel-browser-load
  "d"   #'cj/gptel-browser-delete
  "r"   #'cj/gptel-browser-rename
  "g"   #'cj/gptel-browser-refresh
  "n"   #'next-line
  "p"   #'previous-line
  "q"   #'quit-window)

(define-derived-mode cj/gptel-browser-mode special-mode "GPTel-Browser"
  "Major mode for browsing saved GPTel conversations."
  (setq-local truncate-lines t))

;; -------------------------- helpers (pure where possible)

(defun cj/gptel-browser--topic-from-filename (filename)
  "Return the topic slug from FILENAME, or nil if it isn't a gptel file."
  (when (string-match "\\`\\(.+\\)_[0-9]\\{8\\}-[0-9]\\{6\\}\\.gptel\\'" filename)
    (match-string 1 filename)))

(defun cj/gptel-browser--strip-headers (text)
  "Drop the org #+STARTUP / #+VISIBILITY headers from TEXT and return the rest."
  (let ((s text))
    (while (string-match "\\`#\\+\\(STARTUP\\|VISIBILITY\\):.*\n" s)
      (setq s (substring s (match-end 0))))
    (while (and (> (length s) 0) (eq (aref s 0) ?\n))
      (setq s (substring s 1)))
    s))

(defun cj/gptel-browser--last-message (text)
  "Return a short preview of the last user/AI message in TEXT.
Returns the empty string when no message body is present."
  (let* ((stripped (cj/gptel-browser--strip-headers text))
         ;; Last org-mode top-level heading body, or the whole text if
         ;; there isn't one.
         (body (if (string-match "\\`\\*+[^\n]*\n\\(\\(?:.\\|\n\\)*\\)\\'" stripped)
                   (let* ((all-text stripped)
                          ;; Walk backward to find the last '* ' or '** ' heading
                          (idx (or (cl-loop for i from (1- (length all-text)) downto 0
                                            when (and (or (zerop i)
                                                          (eq (aref all-text (1- i)) ?\n))
                                                      (eq (aref all-text i) ?*))
                                            return i)
                                   0)))
                     (substring all-text idx))
                 stripped)))
    ;; Drop the heading line itself, then collapse whitespace.
    (when (string-match "\\`\\*+[^\n]*\n" body)
      (setq body (substring body (match-end 0))))
    (setq body (replace-regexp-in-string "[\n\t ]+" " " body))
    (string-trim body)))

(defun cj/gptel-browser--preview (text length)
  "Return a LENGTH-char preview from TEXT, ellipsized when truncated."
  (let* ((line (cj/gptel-browser--last-message text))
         (max-len (max 1 length)))
    (cond
     ((string-empty-p line) "")
     ((> (length line) max-len)
      (concat (substring line 0 (1- max-len)) "…"))
     (t line))))

(defun cj/gptel-browser--row-for-file (file dir)
  "Return a propertized row string for FILE under DIR, or nil."
  (let* ((filename (file-name-nondirectory file))
         (topic (cj/gptel-browser--topic-from-filename filename))
         (ts (and topic (cj/gptel--timestamp-from-filename filename))))
    (when (and topic ts)
      (let* ((preview (with-temp-buffer
                        (ignore-errors (insert-file-contents file))
                        (cj/gptel-browser--preview
                         (buffer-string) cj/gptel-browser-preview-length)))
             (row (format "%s  %-22s  %s"
                          (format-time-string "%Y-%m-%d %H:%M" ts)
                          topic preview)))
        (propertize row
                    'cj/gptel-browser-file filename
                    'cj/gptel-browser-topic topic)))))

(defun cj/gptel-browser--rows ()
  "Return propertized row strings for every conversation in the directory."
  (when (and (boundp 'cj/gptel-conversations-directory)
             (file-directory-p cj/gptel-conversations-directory))
    (let ((dir cj/gptel-conversations-directory))
      (delq nil
            (mapcar (lambda (f) (cj/gptel-browser--row-for-file f dir))
                    (directory-files dir t "\\.gptel\\'"))))))

(defun cj/gptel-browser--render ()
  "Replace the current buffer's contents with the conversation listing.
Sort newest first."
  (let ((inhibit-read-only t)
        (rows (sort (cj/gptel-browser--rows)
                    (lambda (a b)
                      (string> (substring-no-properties a 0 16)
                               (substring-no-properties b 0 16))))))
    (erase-buffer)
    (insert (propertize
             "Saved GPTel conversations -- RET/l load  d delete  r rename  g refresh  q quit\n\n"
             'face 'header-line))
    (cond
     ((null rows)
      (insert "  (no saved conversations)\n"))
     (t
      (dolist (row rows)
        (insert row "\n"))))
    (goto-char (point-min))
    (forward-line 2)))

;; -------------------------- entry point

;;;###autoload
(defun cj/gptel-browse-conversations ()
  "Open the saved GPTel conversations browser."
  (interactive)
  (let ((buf (get-buffer-create cj/gptel-browser--buffer-name)))
    (with-current-buffer buf
      (cj/gptel-browser-mode)
      (cj/gptel-browser--render))
    (pop-to-buffer buf)))

(defun cj/gptel-browser-refresh ()
  "Re-read the conversations directory and refresh the browser."
  (interactive)
  (cj/gptel-browser--render))

;; -------------------------- row-level actions

(defun cj/gptel-browser--filename-at-point ()
  "Return the conversation filename on the current line, or nil."
  (get-text-property (line-beginning-position) 'cj/gptel-browser-file))

(defun cj/gptel-browser--filepath-at-point ()
  "Return the absolute filepath for the row at point, or nil."
  (when-let ((filename (cj/gptel-browser--filename-at-point)))
    (expand-file-name filename cj/gptel-conversations-directory)))

(defun cj/gptel-browser-load ()
  "Load the conversation on the current row via `cj/gptel-load-conversation'.
The browser is buried after the load fires."
  (interactive)
  (let ((filepath (cj/gptel-browser--filepath-at-point)))
    (unless filepath
      (user-error "No conversation on this line"))
    (let ((filename (file-name-nondirectory filepath)))
      ;; Stand in for cj/gptel-load-conversation's completing-read so
      ;; the user doesn't get prompted twice.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p cands &rest _)
                   (or (car (cl-find filename cands
                                     :key (lambda (c) (cdr c))
                                     :test #'equal))
                       (caar cands))))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (cj/gptel-load-conversation)))
    (quit-window)))

(defun cj/gptel-browser-delete ()
  "Delete the conversation file on the current row, after confirmation."
  (interactive)
  (let ((filepath (cj/gptel-browser--filepath-at-point)))
    (unless filepath
      (user-error "No conversation on this line"))
    (let ((filename (file-name-nondirectory filepath)))
      (when (y-or-n-p (format "Delete %s? " filename))
        (delete-file filepath)
        (message "Deleted %s" filename)
        (cj/gptel-browser--render)))))

(defun cj/gptel-browser--rename-target (filepath new-topic)
  "Compute the renamed FILEPATH for NEW-TOPIC, preserving the timestamp.
NEW-TOPIC is slugified.  Returns the new absolute filepath."
  (let* ((dir (file-name-directory filepath))
         (filename (file-name-nondirectory filepath))
         (timestamp (and (string-match "_\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)\\.gptel\\'"
                                       filename)
                         (match-string 1 filename)))
         (slug (cj/gptel--slugify-topic new-topic)))
    (unless timestamp
      (error "Cannot extract timestamp from filename: %s" filename))
    (expand-file-name (format "%s_%s.gptel" slug timestamp) dir)))

(defun cj/gptel-browser-rename ()
  "Rename the conversation file on the current row, preserving its timestamp."
  (interactive)
  (let ((filepath (cj/gptel-browser--filepath-at-point)))
    (unless filepath
      (user-error "No conversation on this line"))
    (let* ((old (file-name-nondirectory filepath))
           (current-topic (cj/gptel-browser--topic-from-filename old))
           (new-topic (read-string
                       (format "New topic (was %s): " current-topic)
                       current-topic))
           (target (cj/gptel-browser--rename-target filepath new-topic)))
      (when (equal target filepath)
        (user-error "Topic unchanged"))
      (when (file-exists-p target)
        (user-error "Target already exists: %s" (file-name-nondirectory target)))
      (rename-file filepath target)
      (message "Renamed to %s" (file-name-nondirectory target))
      (cj/gptel-browser--render))))

(provide 'ai-conversations-browser)
;;; ai-conversations-browser.el ends here
