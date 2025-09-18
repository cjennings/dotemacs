;;; show-kill-ring --- Displays Previous Kill Ring Entries -*- lexical-binding: t; coding: utf-8; -*-
;; Show Kill Ring
;; Stolen from Steve Yegge when he wasn't looking
;; enhancements and bugs added by Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Browse items you've previously killed.
;; Yank text  using C-u, the index, then C-y.
;; Yank media using C-u, the index, then M-Y.
;;
;; I've lovingly kept the nice 1970s aesthetic, complete with wood paneling.
;; Maybe I'll give it a makeover at some point.
;;
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
  ;; sync clipboard to kill ring before showing (with error handling)
  (when (fboundp 'cj/sync-clipboard-to-kill-ring)
	(ignore-errors (cj/sync-clipboard-to-kill-ring)))

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

	;; use define-key instead of local-set-key
	(use-local-map (make-sparse-keymap))
	(define-key (current-local-map) "q" 'show-kill-ring-exit)

	;; show it
	(goto-char (point-min))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	;; display-buffer rather than pop-to-buffer
	;; easier for user to C-u (item#) C-y
	;; while the point is where they want to yank
	(display-buffer buf)))

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
	(insert (concat "\n\nItem " (number-to-string ynum)
					" is the next to be yanked:\n\n"))
	(show-kill-insert-item yptr)
	(insert "\n\nThe prefix arg will yank relative to this item.")))

(defun empty-kill-ring ()
  "Force garbage collection of huge kill ring entries that I don't care about."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defun show-kill-get-media-type (item)
  "Extract media type from ITEM if it does contain media data.
Returns a string like \=image/png\=' or nil if not media."
  (when (stringp item)
	(let ((yank-media-types (get-text-property 0 'yank-media item)))
	  (when yank-media-types
		;; yank-media-types is typically an alist of (mime-type . data)
		(caar yank-media-types)))))

(defun show-kill-format-item (item)
  "Format ITEM for display, handling media entries specially.
Returns a string representation suitable for display."
  (cond
   ;; Check if it's a media item
   ((and (stringp item) (get-text-property 0 'yank-media item))
	(let ((media-type (show-kill-get-media-type item)))
	  (format "[Media: %s]" (or media-type "unknown"))))
   ;; Regular text item
   ((stringp item)
	item)
   ;; Other non-string items (shouldn't normally happen)
   (t
	(format "[%s]" (type-of item)))))

(defun show-kill-insert-item (item)
  "Insert an ITEM from the kill ring into the current buffer.
If it's too long, truncate it first. Handle media items specially."
  (let* ((formatted-item (show-kill-format-item item))
		 (max show-kill-max-item-size))
	(cond
	 ((or (not (numberp max))
		  (< max 0)
		  (< (length formatted-item) max))
	  (insert formatted-item))
	 (t
	  ;; put ellipsis on its own line if item is longer than 1 line
	  (let ((preview (substring formatted-item 0 max)))
		(if (< (length formatted-item) (- (frame-width) 5))
			(insert (concat preview "..." ))
		  (insert (concat preview "\n..."))))))))

(defun yank-media-from-kill-ring (n)
  "Yank media from the Nth position in the kill ring.
With prefix argument N, yank the Nth item from the kill ring if
it does contain media. Without prefix argument, behaves like
regular `yank-media'."
  (interactive "P")
  (if n
	  (let* ((n (prefix-numeric-value n))
			 ;; Save original kill ring state
			 (original-kill-ring kill-ring)
			 (original-pointer kill-ring-yank-pointer))
		(unwind-protect
			(progn
			  ;; Rotate kill ring to position N
			  (when (> n 1)
				(setq kill-ring-yank-pointer
					  (nthcdr (1- n) kill-ring)))
			  ;; Check if the item at position N has media
			  (let ((item (nth (1- n) kill-ring)))
				(if (and (stringp item)
						 (get-text-property 0 'yank-media item))
					;; Temporarily make it the first item and yank
					(progn
					  (setq kill-ring (cons item (delete item kill-ring)))
					  (yank-media))
				  (user-error "Item %d in kill ring does not contain media" n))))
		  ;; Restore original state
		  (setq kill-ring original-kill-ring
				kill-ring-yank-pointer original-pointer)))
	;; No prefix arg - just call yank-media normally
	(yank-media)))

;; keybindings
(global-set-key (kbd "M-Y") 'yank-media-from-kill-ring)
(global-set-key (kbd "M-K") 'show-kill-ring)

(provide 'show-kill-ring)
;;; show-kill-ring.el ends here
