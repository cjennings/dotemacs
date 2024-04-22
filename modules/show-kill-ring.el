;;; show-kill-ring --- Displays Previous Kill Ring Entries -*- lexical-binding: t; -*-;; Show Kill Ring
;; Stolen from Steve Yegge when he wasn't looking

;;; Commentary:
;; Browse items you've previously killed.

;;; Code:


(require 'cl-lib)

(defvar show-kill-max-item-size 1000
  "This represents the size of a \='kill ring\=' entry.
A positive number means to limit the display of \='kill-ring\=' items to
that number of characters.")

(defun show-kill-ring-exit ()
  "Exit the show-kill-ring buffer."
  (interactive)
  (quit-window t))

(defun show-kill-ring ()
  "Show the current contents of the kill ring in a separate buffer.
This makes it easy to figure out which prefix to pass to yank."
  (interactive)
  ;; kill existing one, since erasing it doesn't work
  (let ((buf (get-buffer "*Kill Ring*")))
    (and buf (kill-buffer buf)))

  (let* ((buf (get-buffer-create "*Kill Ring*"))
         (temp kill-ring)
         (count 1)
         (bar (make-string 32 ?=))
         (bar2 (concat " " bar))
         (item "  Item ")
         (yptr nil) (ynum 1))
    (set-buffer buf)
    (erase-buffer)

    (show-kill-insert-header)

    ;; show each of the items in the kill ring, in order
    (while temp
      ;; insert our little divider
      (insert (concat "\n" bar item (prin1-to-string count) "  "
                      (if (< count 10) bar2 bar) "\n"))

      ;; if this is the yank pointer target, grab it
      (when (equal temp kill-ring-yank-pointer)
        (setq yptr (car temp) ynum count))

      ;; insert the item and loop
      (show-kill-insert-item (car temp))
      (cl-incf count)
      (setq temp (cdr temp)))

    ;; show info about yank item
    (show-kill-insert-footer yptr ynum)

    (use-local-map (make-sparse-keymap))
    (local-set-key "q" 'show-kill-ring-exit)

    ;; show it
    (goto-char (point-min))
    (setq buffer-read-only t)
	(set-buffer-modified-p nil)
	;; display-buffer rather than pop-to-buffer
	;; easier for user to C-u (item#) C-y
	;; while the point is where they want to yank
	(display-buffer buf)))

(defun show-kill-insert-item (item)
  "Insert an ITEM from the kill ring into the current buffer.
If it's too long, truncate it first."
  (let ((max show-kill-max-item-size))
    (cond
     ((or (not (numberp max))
          (< max 0)
          (< (length item) max))
      (insert item))
     (t
      ;; put ellipsis on its own line if item is longer than 1 line
      (let ((preview (substring item 0 max)))
        (if (< (length item) (- (frame-width) 5))
            (insert (concat preview "..." ))
          (insert (concat preview "\n..."))))))))

(defun show-kill-insert-header ()
  "Insert the show-kill-ring header or a notice if the kill ring is empty."
  (if kill-ring
      (insert "Contents of the kill ring:\n")
    (insert "The kill ring is empty")))

(defun show-kill-insert-footer (yptr ynum)
  "Insert final divider and the yank-pointer (YPTR YNUM) info."
  (when kill-ring
    (save-excursion
      (re-search-backward "^\\(=+  Item [0-9]+\\ +=+\\)$"))
    (insert "\n")
    (insert (make-string (length (match-string 1)) ?=))
    (insert (concat "\n\nItem " (int-to-string ynum)
                    " is the next to be yanked:\n\n"))
    (show-kill-insert-item yptr)
    (insert "\n\nThe prefix arg will yank relative to this item.")))

(defun empty-kill-ring ()
  "Force garbage collection of huge kill ring entries that I don't care about."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(global-set-key (kbd "M-K") 'show-kill-ring)

(provide 'show-kill-ring)
;;; show-kill-ring.el ends here

