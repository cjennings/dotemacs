;;; mu4e-attachments.el --- Save attachments from mu4e view messages -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Project-owned commands for saving attachments out of a mu4e message view.
;;
;; - `cj/mu4e-save-all-attachments' saves every attachment.
;; - `cj/mu4e-save-attachment-here' prompts for one attachment and saves it.
;; - `cj/mu4e-save-some-attachments' opens a selection buffer: RET toggles a
;;   row, a marks all, u unmarks all, s saves the marked rows, q quits.
;; All three prompt for a destination directory.
;;
;; The keybindings live in `mail-config' under `cj/email-map' (C-; e).

;;; Code:

(require 'seq)
(require 'system-lib)  ;; cj/completion-table-annotated

(defvar mu4e-uniquify-save-file-name-function)
(defvar-local cj/mu4e-attachment-selection-directory nil
  "Destination directory for the current attachment selection buffer.")
(defvar-local cj/mu4e-attachment-selection-entries nil
  "Attachment selection entries for the current selection buffer.")

(declare-function mm-save-part-to-file "mm-decode" (handle filename))
(declare-function mu4e-join-paths "mu4e-helpers" (directory &rest components))
(declare-function mu4e-view-mime-parts "mu4e-mime-parts" ())

;; --------------------------- Attachment Saving ------------------------------

(defun cj/mu4e--attachment-parts (&optional parts)
  "Return attachment-like MIME PARTS for the current mu4e view message.
When PARTS is nil, read parts from `mu4e-view-mime-parts'."
  (seq-filter (lambda (part) (plist-get part :attachment-like))
              (or parts
                  (progn
                    (unless (fboundp 'mu4e-view-mime-parts)
                      (require 'mu4e-mime-parts))
                    (mu4e-view-mime-parts)))))

(defun cj/mu4e--attachment-duplicate-filenames (parts)
  "Return filenames that appear more than once in PARTS."
  (let ((counts (make-hash-table :test 'equal))
        duplicates)
    (dolist (part parts)
      (let ((filename (plist-get part :filename)))
        (puthash filename (1+ (gethash filename counts 0)) counts)))
    (maphash (lambda (filename count)
               (when (> count 1)
                 (push filename duplicates)))
             counts)
    duplicates))

(defun cj/mu4e--attachment-label (part duplicate-filenames)
  "Return a completion label for PART.
DUPLICATE-FILENAMES is a list of filenames that need part-index disambiguation."
  (let ((filename (or (plist-get part :filename) "unnamed-attachment")))
    (if (member filename duplicate-filenames)
        (format "%s <part %s>" filename (plist-get part :part-index))
      filename)))

(defun cj/mu4e--attachment-candidates (parts)
  "Return completion candidates for attachment PARTS.
The result is an alist of display labels to MIME part plists."
  (let ((duplicates (cj/mu4e--attachment-duplicate-filenames parts)))
    (mapcar (lambda (part)
              (cons (cj/mu4e--attachment-label part duplicates) part))
            parts)))

(defun cj/mu4e--attachment-annotator (candidates)
  "Return an annotation function over attachment CANDIDATES.
CANDIDATES is the label->part alist from `cj/mu4e--attachment-candidates'.
The annotation shows the part's MIME type and human-readable decoded
size; an unknown candidate annotates as nil so marginalia shows nothing."
  (lambda (cand)
    (when-let* ((part (cdr (assoc cand candidates))))
      (let ((mime (or (plist-get part :mime-type) ""))
            (size (if-let* ((bytes (plist-get part :decoded-size-approx)))
                      (file-size-human-readable bytes)
                    "")))
        (format "  %-24s %s" mime size)))))

(defun cj/mu4e--attachment-default-directory (parts)
  "Return a sensible default save directory for attachment PARTS."
  (file-name-as-directory
   (or (plist-get (car parts) :target-dir)
       (expand-file-name "~/Downloads/"))))

(defun cj/mu4e--read-attachment-directory (parts)
  "Prompt for a destination directory for attachment PARTS."
  (file-name-as-directory
   (read-directory-name "Save attachments to: "
                        (cj/mu4e--attachment-default-directory parts))))

(defun cj/mu4e--ensure-attachment-save-functions ()
  "Load mu4e MIME support when attachment save helpers need it."
  (unless (and (boundp 'mu4e-uniquify-save-file-name-function)
               (fboundp 'mu4e-join-paths))
    (require 'mu4e-mime-parts)))

(defun cj/mu4e--save-attachment-part (part directory)
  "Save attachment PART to DIRECTORY and return the final path.
Signals a `user-error' when PART's MIME handle is stale: a handle's car
is the buffer holding the part's bytes, and viewing another message kills
it, so saving through it would error deep in mm-decode or write another
message's content.  The staleness check runs before
`cj/mu4e--ensure-attachment-save-functions', like the no-handle check."
  (let ((handle (plist-get part :handle)))
    (unless handle
      (user-error "Attachment has no MIME handle: %s"
                  (or (plist-get part :filename) "<unnamed>")))
    (when (and (consp handle)
               (bufferp (car handle))
               (not (buffer-live-p (car handle))))
      (user-error "Attachment %s is stale (the message view changed) -- reopen the message and save again"
                  (or (plist-get part :filename) "<unnamed>")))
    (cj/mu4e--ensure-attachment-save-functions)
    (let* ((path (funcall mu4e-uniquify-save-file-name-function
                          (mu4e-join-paths directory
                                           (plist-get part :filename)))))
      (mm-save-part-to-file handle path)
      path)))

(defun cj/mu4e--save-attachment-parts (parts directory)
  "Save attachment PARTS to DIRECTORY and return the saved paths."
  (mapcar (lambda (part)
            (cj/mu4e--save-attachment-part part directory))
          parts))

(defun cj/mu4e-save-all-attachments ()
  "Prompt for a directory and save all attachments in the current mu4e message."
  (interactive)
  (let ((parts (cj/mu4e--attachment-parts)))
    (unless parts
      (user-error "No attachments for this message"))
    (let* ((directory (cj/mu4e--read-attachment-directory parts))
           (paths (cj/mu4e--save-attachment-parts parts directory)))
      (message "Saved %d attachment%s to %s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               directory)
      paths)))

(defun cj/mu4e-save-attachment-here ()
  "Prompt for one attachment and a directory, then save that attachment."
  (interactive)
  (let ((parts (cj/mu4e--attachment-parts)))
    (unless parts
      (user-error "No attachments for this message"))
    (let* ((directory (cj/mu4e--read-attachment-directory parts))
           (candidates (cj/mu4e--attachment-candidates parts))
           (choice (completing-read
                    "Save attachment: "
                    (cj/completion-table-annotated
                     'mu4e-attachment
                     (cj/mu4e--attachment-annotator candidates)
                     candidates)
                    nil t))
           (part (cdr (assoc choice candidates)))
           (path (cj/mu4e--save-attachment-part part directory)))
      (message "Saved attachment to %s" path)
      path)))

(defvar cj/mu4e-attachment-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'cj/mu4e-attachment-selection-toggle)
    (define-key map (kbd "a") #'cj/mu4e-attachment-selection-mark-all)
    (define-key map (kbd "u") #'cj/mu4e-attachment-selection-unmark-all)
    (define-key map (kbd "s") #'cj/mu4e-attachment-selection-save-marked)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `cj/mu4e-attachment-selection-mode'.")

(define-derived-mode cj/mu4e-attachment-selection-mode special-mode "Mail Attachments"
  "Mode for selecting mu4e attachments to save.")

(defun cj/mu4e--attachment-selection-entry-at-point ()
  "Return the attachment selection entry at point."
  (or (get-text-property (point) 'cj/mu4e-attachment-entry)
      (get-text-property (line-beginning-position) 'cj/mu4e-attachment-entry)
      (user-error "No attachment on this line")))

(defun cj/mu4e--attachment-selection-render ()
  "Render the current attachment selection buffer."
  (let ((inhibit-read-only t)
        (point-line (line-number-at-pos)))
    (erase-buffer)
    (insert (format "Save attachments to: %s\n\n"
                    cj/mu4e-attachment-selection-directory))
    (insert "RET toggle   a mark all   u unmark all   s save marked   q quit\n\n")
    (dolist (entry cj/mu4e-attachment-selection-entries)
      (let* ((part (plist-get entry :part))
             (mark (if (plist-get entry :selected) "[x]" "[ ]"))
             (label (plist-get entry :label))
             (mime-type (or (plist-get part :mime-type) ""))
             (size (if-let ((bytes (plist-get part :decoded-size-approx)))
                       (file-size-human-readable bytes)
                     "")))
        (insert
         (propertize
          (format "%s %-40s %-24s %s\n" mark label mime-type size)
          'cj/mu4e-attachment-entry entry))))
    (goto-char (point-min))
    (forward-line (max 0 (1- point-line)))))

(defun cj/mu4e--attachment-selection-setup (parts directory)
  "Populate the current selection buffer with attachment PARTS and DIRECTORY."
  (setq cj/mu4e-attachment-selection-directory directory)
  (setq cj/mu4e-attachment-selection-entries
        (mapcar (lambda (candidate)
                  (list :label (car candidate)
                        :part (cdr candidate)
                        :selected nil))
                (cj/mu4e--attachment-candidates parts)))
  (cj/mu4e--attachment-selection-render))

(defun cj/mu4e-attachment-selection-toggle ()
  "Toggle the attachment entry at point."
  (interactive)
  (let ((entry (cj/mu4e--attachment-selection-entry-at-point)))
    (setf (plist-get entry :selected)
          (not (plist-get entry :selected)))
    (cj/mu4e--attachment-selection-render)))

(defun cj/mu4e-attachment-selection-mark-all ()
  "Mark all attachments in the selection buffer."
  (interactive)
  (dolist (entry cj/mu4e-attachment-selection-entries)
    (setf (plist-get entry :selected) t))
  (cj/mu4e--attachment-selection-render))

(defun cj/mu4e-attachment-selection-unmark-all ()
  "Unmark all attachments in the selection buffer."
  (interactive)
  (dolist (entry cj/mu4e-attachment-selection-entries)
    (setf (plist-get entry :selected) nil))
  (cj/mu4e--attachment-selection-render))

(defun cj/mu4e-attachment-selection-save-marked ()
  "Save the marked attachments, then clear the marks.
Clearing the marks keeps a second `s' from silently re-saving the same set;
quit the buffer with `q' or RET when done.  With no marks set, this is a
`user-error'."
  (interactive)
  (let ((parts (mapcar (lambda (entry) (plist-get entry :part))
                       (seq-filter (lambda (entry)
                                     (plist-get entry :selected))
                                   cj/mu4e-attachment-selection-entries))))
    (unless parts
      (user-error "No attachments selected"))
    (let ((paths (cj/mu4e--save-attachment-parts
                  parts cj/mu4e-attachment-selection-directory)))
      (dolist (entry cj/mu4e-attachment-selection-entries)
        (setf (plist-get entry :selected) nil))
      (cj/mu4e--attachment-selection-render)
      (message "Saved %d attachment%s to %s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               cj/mu4e-attachment-selection-directory)
      paths)))

(defun cj/mu4e--open-attachment-selection-buffer (parts directory)
  "Open an attachment selection buffer for PARTS and DIRECTORY."
  (let ((buffer (get-buffer-create "*mu4e attachments*")))
    (with-current-buffer buffer
      (cj/mu4e-attachment-selection-mode)
      (cj/mu4e--attachment-selection-setup parts directory))
    (pop-to-buffer buffer)))

(defun cj/mu4e-save-some-attachments ()
  "Prompt for a directory and open a buffer to select attachments to save."
  (interactive)
  (let ((parts (cj/mu4e--attachment-parts)))
    (unless parts
      (user-error "No attachments for this message"))
    (let ((directory (cj/mu4e--read-attachment-directory parts)))
      (cj/mu4e--open-attachment-selection-buffer parts directory))))

(provide 'mu4e-attachments)
;;; mu4e-attachments.el ends here
