;;; custom-functions.el --- My Custom Functions and Keymaps -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; These are custom utility functions I use frequently.
;; For convenience, they are bound to a custom keymap with a prefix of "C-;".
;; Additional keymaps are created on top of this prefix to collect similar operations.
;;
;; C-; --- Custom Key Map
;;   C-; )    → jump to matching parenthesis
;;   C-; f    → re-formats region or buffer (delete trailing whitespace, reindent, and untabify).
;;   C-; W    → counts words in region or buffer displaying results in echo area.
;;   C-; /    → replace common glyph fractions (½) to text (1/2) (text to glyph with C-u).
;;   C-; r    → align text by regexp with spaces
;;   C-; |    → toggle visibility of the fill-column indicator
;;
;; C-; b --- Buffer & File Operations
;;   C-; b m  → move buffer and file to another directory
;;   C-; b r  → rename buffer and its file simultaneously
;;   C-; b d  → delete buffer and its file simultaneously
;;   C-; b l  → copy file:// link of buffer’s source file
;;   C-; b c  → copy entire buffer to the kill rung
;;   C-; b b  → clear contents of buffer from point to beginnning
;;   C-; b e  → clear contents of buffer from point to end
;;
;; C-; w --- Whitespace Operations
;;   C-; w r  → remove leading/trailing whitespace from line or region (buffer with C-u).
;;   C-; w c  → collapses runs of whitespace to one space.
;;   C-; w l  → delete all blank lines in region or buffer
;;   C-; w h  → hyphenate all whitespace in region
;;
;; C-; s --- Surround, Append & Prepend
;;   C-; s s  → surround word or region with string
;;   C-; s a  → append a string to each line
;;   C-; s p  → prepend a string to each line
;;
;; C-; d --- Date/Time Insertion
;;   C-; d r  → readable date and time : Sunday, August 31, 2025 at 04:07:02 PM CDT
;;   C-; d s  → sortable date and time : 2025-08-31 Sun @ 16:07:30 -0500
;;   C-; d t  → sortable time only     : 04:07:50 PM CDT
;;   C-; d D  → readable time only     : 4:08 PM
;;   C-; d T  → readable date only     : Sunday, August 31, 2025
;;   C-; d d  → sortable date only     : 2025-08-31 Sun
;;
;; C-; l --- Line & Paragraph Operations
;;   C-; l j  → join lines (or selected region of lines)
;;   C-; l J  → join entire paragraph. guesses at the lines that constitute paragraph.
;;   C-; l d  → duplicates the line or region
;;   C-; l r  → remove duplicate lines from the buffer, keepinf the first occurrence.
;;   C-; l u  → "underline" current line: repeat a chosen character to same length on line below.
;;
;; C-; m --- Comment Styling & Removal
;;   C-; m r  → reformats selecton into a commented paragraph re-wrapping at fill column width.
;;   C-; m c  → insert centered comment
;;   C-; m -  → insert hyphen-style comment
;;   C-; m b  → draw a comment box
;;   C-; m D  → delete all comments in buffer
;;
;; C-; o --- Ordering & Sorting
;;   C-; o a  → arrayify lines into quoted list
;;   C-; o u  → unarrayify list into lines
;;   C-; o A  → alphabetize items in region
;;   C-; o l  → split comma-separated text onto lines
;;
;; C-; c --- Case-Change Operations
;;   C-; c t  → Change selected text to Title Case  : This is the Title of a Movie
;;   C-; c u  → Change word or region to Upper Case : THIS IS THE TITLE OF A MOVIE
;;   C-; c d  → Change word or region to Lower Case : this is the title of a movie

;;; Code:

(require 'subr-x)

(use-package expand-region
  :demand t) ;; used w/in join paragraph

;;; ----------------- Miscellaneous Functions And Custom Keymap -----------------

(defun cj/jump-to-matching-paren ()
  "If point is on a parenthesis, jump to it's match.
Otherwise, complain."
  (interactive)
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
		 (forward-list))
		((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
		 (backward-list))
		(t (message "Cursor doesn't follow parenthesis, so there's no match."))))

(defun cj/format-region-or-buffer ()
  "Reformat the region or the entire buffer.
Deletes trailing whitespace, reindents the region, and replaces tabs with spaces."
  (interactive)
  (let ((start-pos (if (use-region-p) (region-beginning) (point-min)))
		(end-pos (if (use-region-p) (region-end) (point-max))))
	(save-excursion
	  (save-restriction
		(narrow-to-region start-pos end-pos)
		(delete-trailing-whitespace)
		(indent-region (point-min) (point-max))
		(untabify (point-min) (point-max))))))

(defun cj/count-words-buffer-or-region ()
  "Count the number of words in buffer or region.
Displays result as a message in the minibuffer and *Messasges* buffer."
  (interactive)
  (let ((begin (point-min))
		(end (point-max))
		(area_type "the buffer"))
	(when mark-active
	  (setq begin (region-beginning)
			end (region-end)
			area_type "the region"))
	(message (format "There are %d words in %s." (count-words begin end) area_type))))

(defun cj/replace-fraction-glyphs (start end)
  "Replace common fraction glyphs (½) with their text format (1/2).
Operates in the buffer or region (as identified with START and END) if selected.
Replaces the text versions with the glyphs if function prefaced by 'C-u'."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((replacements (if current-prefix-arg
                          '(("1/4" . "¼")
                            ("1/2" . "½")
                            ("3/4" . "¾")
                            ("1/3" .  "⅓")
                            ("2/3" . "⅔"))
                        '(("¼" . "1/4")
                          ("½" . "1/2")
                          ("¾" . "3/4")
                          ("⅓" . "1/3")
                          ("⅔" . "2/3")))))
    (save-excursion
      (dolist (r replacements)
        (goto-char start)
        (while (search-forward (car r) end t)
          (replace-match (cdr r)))))))

(defun cj/align-regexp-with-spaces (orig-fun &rest args)
  "Around-advice for =align-regexp' to disable tabs during alignment.
ORIG-FUN is the original =align-regexp'; ARGS are its arguments."
  (let ((indent-tabs-mode nil))
	(apply orig-fun args)))

(advice-remove 'align-regexp #'align-regexp-with-spaces) ; in case this is reloaded
(advice-add    'align-regexp :around #'cj/align-regexp-with-spaces)

;; Must unbind Flyspell's  'C-;' keybinding before it's assigned to cj/custom-keymap
(global-unset-key (kbd "C-;"))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))

(defvar cj/custom-keymap
  (let ((map (make-sparse-keymap)))
	(define-key map ")" 'cj/jump-to-matching-paren)
    (define-key map "f" 'cj/format-region-or-buffer)
	(define-key map "W" 'cj/count-words-buffer-or-region)
    (define-key map "/" 'cj/replace-fraction-glyphs)
	(define-key map "r" 'align-regexp)
    (define-key map "|" 'display-fill-column-indicator-mode)
    map)
  "The base key map for custom elisp functions holding miscellaneous functions.
Other key maps extend from this key map to hold categorized functions.")
(global-set-key (kbd "C-;") cj/custom-keymap)

;;; ------------------- Buffer And File Operations And Keymap -------------------

(defun cj/move-buffer-and-file (dir)
  "Move both current buffer and the file it visits to DIR."
  (interactive "DMove buffer and file (to new directory): ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn  (copy-file filename newname 1)  (delete-file filename)
              (set-visited-file-name newname)  (set-buffer-modified-p nil)  t))))

(defun cj/rename-buffer-and-file (new-name)
  "Rename both current buffer and the file it visits to NEW-NAME."
  (interactive
   (list (read-string "Rename buffer and file (to new name): "
                      (file-name-nondirectory (buffer-file-name)))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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

(defun cj/copy-whole-buffer ()
  "Copy the entire contents of the current buffer to the kill ring.
Point and mark are left exactly where they were.  No transient region
is created.  A message is displayed when done."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
	(kill-new contents)
    (message "Buffer contents copied to kill ring")))

(defun cj/clear-to-end-of-buffer ()
  "Delete all text from point to the end of the current buffer.
This does not save the deleted text in the kill ring."
  (interactive)
  (delete-region (point) (point-max))
  (message "Buffer contents removed to the end of the buffer."))

(defun cj/clear-to-beginning-of-buffer ()
  "Delete all text from point to the end of the current buffer.
This does not save the deleted text in the kill ring."
  (interactive)
  (delete-region (point) (point-min))
  (message "Buffer contents removed to the beginning of the buffer."))

;; Buffer & file operations prefix and keymap
(define-prefix-command 'cj/buffer-and-file-map nil
					   "Keymap for buffer-and-file operations.")
(define-key cj/custom-keymap "b" 'cj/buffer-and-file-map)
(define-key cj/buffer-and-file-map "m" 'cj/move-buffer-and-file)
(define-key cj/buffer-and-file-map "r" 'cj/rename-buffer-and-file)
(define-key cj/buffer-and-file-map "d" 'cj/delete-buffer-and-file)
(define-key cj/buffer-and-file-map "c" 'cj/copy-whole-buffer)
(define-key cj/buffer-and-file-map "e" 'cj/clear-to-end-of-buffer)
(define-key cj/buffer-and-file-map "b" 'cj/clear-to-beginning-of-buffer)
(define-key cj/buffer-and-file-map "l" 'cj/copy-link-to-buffer-file)

;;; ---------------------- Whitespace Operations And Keymap ---------------------

(defun cj/remove-leading-trailing-whitespace ()
  "Remove leading and trailing whitespace in a region, line, or buffer.
When called interactively:
- If a region is active, operate on the region
- If called with C-u prefix, operate on entire buffer
- Otherwise, operate on current line."
  (interactive)
  (let ((start (cond (current-prefix-arg (point-min))
                     ((use-region-p) (region-beginning))
                     (t (line-beginning-position))))
        (end (cond (current-prefix-arg (point-max))
                   ((use-region-p) (region-end))
                   (t (line-end-position)))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t) (replace-match ""))))))

(defun cj/collapse-whitespace-line-or-region ()
  "Collapse whitespace to one space in the current line, or region if selected.
Ensure there is exactly one space between words, and remove leading and trailing
whitespace."
  (interactive)
  (save-excursion
	(let* ((region-active (use-region-p))
		   (beg (if region-active (region-beginning) (line-beginning-position)))
		   (end (if region-active (region-end) (line-end-position))))
	  (save-restriction
		(narrow-to-region beg end)
		;; Replace all tabs with space
		(goto-char (point-min))
		(while (search-forward "\t" nil t)
		  (replace-match " " nil t))
		;; Remove leading and trailing spaces
		(goto-char (point-min))
		(while (re-search-forward "^\\s-+\\|\\s-+$" nil t)
		  (replace-match "" nil nil))
		;; Ensure only one space between words/symbols
		(goto-char (point-min))
		(while (re-search-forward "\\s-\\{2,\\}" nil t)
		  (replace-match " " nil nil))))))

(defun cj/delete-blank-lines-region-or-buffer (start end)
  "Delete all blank lines in the region between START and END.
Blank lines contain nothing or only whitespace (spaces or tabs).
If called with an active region, operate on that region.
If no region is selected, prompt before operating on the whole buffer.
Otherwise signal a user-error and do nothing. The point is restored
to its original position after deletion."
  (interactive
   (if (use-region-p)
       ;; grab its boundaries if there's a region
       (list (region-beginning) (region-end))
     ;; or ask if user intended operating on whole buffer
     (if (yes-or-no-p "Delete blank lines in entire buffer? ")
         (list (point-min) (point-max))
       (user-error "Aborted"))))
  (save-excursion
    (save-restriction
      (widen)
      ;; Regexp "^[[:space:]]*$" matches lines of zero or more spaces/tabs.
      (flush-lines "^[[:space:]]*$" start end)))
  ;; Return nil (Emacs conventions). Point is already restored.
  nil)

(defun cj/hyphenate-whitespace-in-region (start end)
  "Hyphenate all continuous whitespace in the region.
START and END represent the region selected."
  (interactive "*r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (re-search-forward "[ \t\n\r]+" nil t)
            (replace-match "-"))))
    (message "No region; nothing to hyphenate.")))


;; Whitespace operations prefix and keymap
(define-prefix-command 'cj/whitespace-map nil
					   "Keymap for whitespace operations.")
(define-key cj/custom-keymap "w" 'cj/whitespace-map)
(define-key cj/whitespace-map "r" 'cj/remove-leading-trailing-whitespace)
(define-key cj/whitespace-map "c" 'cj/collapse-whitespace-line-or-region)
(define-key cj/whitespace-map "l" 'cj/delete-blank-lines-region-or-buffer)
(define-key cj/whitespace-map "h" 'cj/hyphenate-whitespace-in-region)

;;; ------------------------- Surround, Append, Prepend -------------------------

(defun cj/surround-word-or-region ()
  "Prompt for a string, insert it before and after the word at point or selected region."
  (interactive)
  (let ((str (read-string "Surround with: "))
        (regionp (use-region-p)))
    (save-excursion
      (if regionp
          (let ((beg (region-beginning))
                (end (region-end)))
            (goto-char end)
            (insert str)
            (goto-char beg)
            (insert str))
        (if (thing-at-point 'word)
            (let ((bounds (bounds-of-thing-at-point 'word)))
              (goto-char (cdr bounds))
              (insert str)
              (goto-char (car bounds))
              (insert str))
          (message "Can't insert around. No word at point and no region selected."))))))

(defun cj/append-to-lines-in-region-or-buffer (str)
  "Prompt for STR and append it to the end of each line in region or buffer."
  (interactive "sEnter string to append: ")
  (let ((start-pos (if (use-region-p)
                       (region-beginning)
                     (point-min)))
        (end-pos (if (use-region-p)
                     (region-end)
                   (point-max))))
    (save-excursion
      (goto-char start-pos)
      (while (< (point) end-pos)
        (move-end-of-line 1)
        (insert str)
        (forward-line 1)))))

(defun cj/prepend-to-lines-in-region-or-buffer (str)
  "Prompt for STR and prepend it to the start of each line in region or buffer."
  (interactive "sEnter string to prepend: ")
  (let ((start-pos (if (use-region-p)
                       (region-beginning)
                     (point-min)))
        (end-pos (if (use-region-p)
                     (region-end)
                   (point-max))))
    (save-excursion
      (goto-char start-pos)
      (while (< (point) end-pos)
        (beginning-of-line 1)
        (insert str)
        (forward-line 1)))))

;; Surround, append, prepend prefix keymap
(define-prefix-command 'cj/surround-map nil
					   "Keymap for surrounding, appending, and prepending operations.")
(define-key cj/custom-keymap "s" 'cj/surround-map)
(define-key cj/surround-map "s" 'cj/surround-word-or-region)
(define-key cj/surround-map "a" 'cj/append-to-lines-in-region-or-buffer)
(define-key cj/surround-map "p" 'cj/prepend-to-lines-in-region-or-buffer)

;;; -------------------------- Date And Time Insertion --------------------------

(defvar readable-date-time-format "%A, %B %d, %Y at %I:%M:%S %p %Z "
  "Format of date to insert with `insert-readable-date-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-readable-date-time ()
  "Insert the current date and time into current buffer.
Uses `readable-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string readable-date-time-format (current-time))))

(defvar sortable-date-time-format "%Y-%m-%d %a @ %H:%M:%S %z "
  "Format of date to insert with `insert-current-date-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-sortable-date-time ()
  "Insert the current date and time into current buffer.
Uses `sortable-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string sortable-date-time-format (current-time))))

(defvar sortable-time-format "%I:%M:%S %p %Z "
  "Time format to insert with `insert-current-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-sortable-time ()
  "Insert the current time into current buffer.
Uses `sortable-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string sortable-time-format (current-time))))

(defvar readable-time-format  "%-I:%M %p "
  "Time format to insert with `insert-readable-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-readable-time ()
  "Insert the current time into current buffer.
Uses `readable-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string readable-time-format (current-time))))

(defvar sortable-date-format "%Y-%m-%d %a"
  "Time format to insert with `insert-current-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-sortable-date ()
  "Insert the current time into current buffer.
Uses `sortable-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string sortable-date-format (current-time))))

(defvar readable-date-format "%A, %B %d, %Y"
  "Time format to insert with `insert-readable-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-readable-date ()
  "Insert the current date into current buffer.
Uses `readable-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string readable-date-format (current-time))))

;; Date/time insertion prefix and keymap
(define-prefix-command 'cj/datetime-map nil
					   "Keymap for inserting various date/time formats.")
(define-key cj/custom-keymap "d" 'cj/datetime-map)
(define-key cj/datetime-map "r" 'cj/insert-readable-date-time)
(define-key cj/datetime-map "s" 'cj/insert-sortable-date-time)
(define-key cj/datetime-map "t" 'cj/insert-sortable-time)
(define-key cj/datetime-map "T" 'cj/insert-readable-time)
(define-key cj/datetime-map "d" 'cj/insert-sortable-date)
(define-key cj/datetime-map "D" 'cj/insert-readable-date)

;;; ----------------------- Line And Paragraph Operations -----------------------

(defun cj/join-line-or-region ()
  "Apply 'join-line' over the marked region or join with previous line."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1))
        (goto-char end)
        (newline))
    ;; No region - just join with previous line
    (join-line)
    (newline)))


(defun cj/join-paragraph ()
  "Mark all text in a paragraph then run cj/join-line-or-region."
  (interactive)
  (er/mark-paragraph) ;; from package expand region
  (cj/join-line-or-region (region-beginning)(region-end))
  (forward-line))

(defun cj/duplicate-line-or-region (&optional comment)
  "Duplicate the line or region below.
Comment the duplicated line if prefix argument COMMENT is passed."
  (interactive "P")
  (let* ((b (if (region-active-p) (region-beginning) (line-beginning-position)))
         (e (if (region-active-p) (region-end) (line-end-position)))
         (lines (split-string (buffer-substring-no-properties b e) "\n")))
    (save-excursion
      (goto-char e)
      (dolist (line lines)
        (open-line 1)
        (forward-line 1)
        (insert line)
        ;; If the COMMENT prefix argument is non-nil, comment the inserted text
        (when comment
		  (comment-region (line-beginning-position) (line-end-position)))))))

(defun cj/remove-duplicate-lines-region-or-buffer ()
  "Find duplicate lines in region or buffer keeping the first occurrence.
If a region is selected, operate on the region. Otherwise, operate on the whole buffer."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (let ((end-marker (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end-marker t))
          (replace-match "\\1\n\\2"))))))

(defun cj/underscore-line ()
  "Underline the current line by inserting a row of characters below it.
If the line is empty or contains only whitespace, abort with a message."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (if (string-match-p "^[[:space:]]*$" line)
        (message "Line empty or only whitespace. Aborting.")
      (let* ((char (read-char "Enter character for underlining: "))
             (len  (save-excursion
                     (goto-char (line-end-position))
                     (current-column))))
        (save-excursion
          (end-of-line)
          (insert "\n" (make-string len char)))))))


;; Line & paragraph operations prefix and keymap
(define-prefix-command 'cj/line-and-paragraph-map nil
					   "Keymap for line and paragraph manipulation.")
(define-key cj/custom-keymap "l" 'cj/line-and-paragraph-map)
(define-key cj/line-and-paragraph-map "j" 'cj/join-line-or-region)
(define-key cj/line-and-paragraph-map "J" 'cj/join-paragraph)
(define-key cj/line-and-paragraph-map "d" 'cj/duplicate-line-or-region)
(define-key cj/line-and-paragraph-map "r" 'cj/remove-duplicate-lines-region-or-buffer)
(define-key cj/line-and-paragraph-map "u" 'cj/underscore-line)

;;; ---------------------------------- Comments ---------------------------------

(defun cj/comment-reformat ()
  "Reformats commented text into a single paragraph."
  (interactive)

  (if mark-active
	  (let ((beg (region-beginning))
			(end (copy-marker (region-end)))
			(orig-fill-column fill-column))
		(uncomment-region beg end)
		(setq fill-column (- fill-column 3))
		(cj/join-line-or-region beg end)
		(comment-region beg end)
		(setq fill-column orig-fill-column )))
  ;; if no region
  (message "No region was selected. Select the comment lines to reformat."))

(defun cj/comment-centered (&optional comment-char)
  "Insert comment text centered around the COMMENT-CHAR character.
Will default to the '#' character if called with no arguments. Uses the value of
fill-column or 80 (whichever is less) to calculate the comment length. Will
begin and end the line with the appropriate comment symbols based on programming mode."
  (interactive)
  (if (not (char-or-string-p comment-char))
	  (setq comment-char "#"))
  (let* ((comment (capitalize (string-trim (read-from-minibuffer "Comment: "))))
		 (fill-column (min fill-column 80))
		 (comment-length (length comment))
		 (comment-start-length (length comment-start))
		 (comment-end-length (length comment-end))
		 (current-column-pos (current-column))
		 (space-on-each-side (/ (- fill-column
								   current-column-pos
								   comment-length
								   (length comment-start)
								   (length comment-end)
								   ;; Single space on each side of comment
								   (if (> comment-length 0) 2 0)
								   ;; Single space after comment syntax sting
								   1)
								2)))
    (if (< space-on-each-side 2)
		(message "Comment string is too big to fit in one line")
	  (progn
		(insert comment-start)
		(when (equal comment-start ";") ; emacs-lisp line comments are ;;
		  (insert comment-start))       ; so insert comment-char again
		(insert " ")
		(dotimes (_ space-on-each-side) (insert comment-char))
		(when (> comment-length 0) (insert " "))
		(insert comment)
		(when (> comment-length 0) (insert " "))
		(dotimes (_ (if (= (% comment-length 2) 0)
						(- space-on-each-side 1)
					  space-on-each-side))
		  (insert comment-char))
		;; Only insert trailing space and comment-end if comment-end is not empty
		(when (not (string-empty-p comment-end))
		  (insert " ")
		  (insert comment-end))))))

(defun cj/comment-box ()
  "Insert a comment box around text that the user inputs.
The box extends to the fill column, centers the text, and uses the current
mode's comment syntax at both the beginning and end of each line. The box
respects the current indentation level and avoids trailing whitespace."
  (interactive)
  (let* ((comment-char (if (equal comment-start ";") ";;"
						 (string-trim comment-start)))
		 (comment-end-char (if (string-empty-p comment-end)
							   comment-char
							 (string-trim comment-end)))
		 (line-char (if (equal comment-char ";;") "-" "#"))
		 (comment (capitalize (string-trim (read-from-minibuffer "Comment: "))))
		 (comment-length (length comment))
		 (current-column-pos (current-column))
		 (max-width (min fill-column 80))
		 ;; Calculate available width between comment markers
		 (available-width (- max-width
							 current-column-pos
							 (length comment-char)
							 (length comment-end-char)))
		 ;; Inner width is the width without the spaces after comment start and before comment end
		 (inner-width (- available-width 2))
		 ;; Calculate padding for each side of the centered text
		 (padding-each-side (max 1 (/ (- inner-width comment-length) 2)))
		 ;; Adjust for odd-length comments
		 (right-padding (if (= (% (- inner-width comment-length) 2) 0)
							padding-each-side
						  (1+ padding-each-side))))

	;; Check if we have enough space
	(if (< inner-width (+ comment-length 4)) ; minimum sensible width
		(message "Comment string is too big to fit in one line")
	  (progn
		;; Top line - fill entirely with line characters except for space after comment start
		(insert comment-char)
        (insert " ")
        (insert (make-string inner-width (string-to-char line-char)))
		(insert " ")
		(insert comment-end-char)
		(newline)

		;; Add indentation on the new line to match current column
		(dotimes (_ current-column-pos) (insert " "))

		;; Middle line with centered text
		(insert comment-char)
		(insert " ")
		;; Left padding
		(dotimes (_ padding-each-side) (insert " "))
		;; The comment text
		(insert comment)
		;; Right padding
		(dotimes (_ right-padding) (insert " "))
		(insert " ")
		(insert comment-end-char)
		(newline)

		;; Add indentation on the new line to match current column
		(dotimes (_ current-column-pos) (insert " "))

		;; Bottom line - same as top line
		(insert comment-char)
		(insert " ")
		(dotimes (_ inner-width) (insert line-char))
		(insert " ")
		(insert comment-end-char)
		(newline)))))

(defun cj/comment-hyphen()
  "Insert a centered comment with '-' (hyphens) on each side."
  (interactive)
  (cj/comment-centered "-"))

(defun cj/delete-buffer-comments ()
  "Delete all comments within the current buffer."
  (interactive)
  (goto-char (point-min))
  (let (kill-ring)
    (comment-kill (count-lines (point-min) (point-max)))))

;; Comment styles & removal prefix and keymap
(define-prefix-command 'cj/comment-map nil
                       "Keymap for comment styling and removal.")
(define-key cj/custom-keymap "m" 'cj/comment-map)
(define-key cj/comment-map "r" 'cj/comment-reformat)
(define-key cj/comment-map "c" 'cj/comment-centered)
(define-key cj/comment-map "-" 'cj/comment-hyphen)
(define-key cj/comment-map "b" 'cj/comment-box)
(define-key cj/comment-map "D" 'cj/delete-buffer-comments)

;;; ---------------------- Ordering And Sorting Operations ----------------------

(defun cj/arrayify (start end quote)
  "Turn unquoted text on newlines into quoted comma-separated strings.
START and END indicate the region selected.
QUOTE is the characters used for quotations (i.e, \=' or \")"
  (interactive "r\nMQuotation character to use for array element: ")
  (let ((insertion
		 (mapconcat
		  (lambda (x) (format "%s%s%s" quote x quote))
		  (split-string (buffer-substring start end)) ", ")))
	(delete-region start end)
	(insert insertion)))

(defun cj/unarrayify (start end)
  "Turn quoted comma-separated strings into unquoted text on newlines.
START and END indicate the region selected."
  (interactive "r")
  (let ((insertion
		 (mapconcat
		  (lambda (x) (replace-regexp-in-string "[\"']" "" x))
		  (split-string (buffer-substring start end) ", ") "\n")))
	(delete-region start end)
	(insert insertion)))

(defun cj/alphabetize-region ()
  "Alphabetize strings (words/tokens) in region replacing the original region.
The result will be comma separated."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let ((start (region-beginning))
        (end (region-end))
        (string (buffer-substring-no-properties (region-beginning) (region-end))))
	(delete-region start end)
	(goto-char start)
	(insert
	 (mapconcat #'identity
				(sort (split-string string "[[:space:],]+" t)
					  #'string-lessp)
				", "))))

(defun cj/comma-separated-text-to-lines ()
  "Breaks up text between commas in a region and places each text on its own line."
  (interactive)
  (if (not (region-active-p))
	  (error "No region selected"))

  (let ((beg (region-beginning))
		(end (region-end))
		(text (buffer-substring-no-properties (region-beginning) (region-end))))
	(with-temp-buffer
	  (insert text)
	  (goto-char (point-min))
	  (while (search-forward "," nil t)
		(replace-match "\n" nil t))
	  (delete-trailing-whitespace)
	  (setq text (buffer-string)))

	(delete-region beg end)
	(goto-char beg)
	(insert text)))


;; Ordering & sorting prefix and keymap
(define-prefix-command 'cj/ordering-map nil
					   "Keymap for text ordering and sorting operations.")
(define-key cj/custom-keymap "o" 'cj/ordering-map)
(define-key cj/ordering-map "a" 'cj/arrayify)
(define-key cj/ordering-map "u" 'cj/unarrayify)
(define-key cj/ordering-map "A" 'cj/alphabetize-region)
(define-key cj/ordering-map "l" 'cj/comma-separated-text-to-lines)

;;; --------------------------- Case Change Operations --------------------------

(defun cj/title-case-region ()
  "Capitalize the region in title case format.
Title case is a capitalization convention where major words
are capitalized,and most minor words are lowercase.  Nouns,
verbs (including linking verbs), adjectives, adverbs,pronouns,
and all words of four letters or more are considered major words.
Short (i.e., three letters or fewer) conjunctions, short prepositions,
and all articles are considered minor words."
  (interactive)
  (let ((beg nil)
		(end nil)
		(prev-word-end nil)
		;; Allow capitals for skip characters after this, so:
		;;   Warning: An Example
		;; Capitalizes the `An'.
		(chars-skip-reset '(?: ?! ??))
		;; Don't capitalize characters directly after these. e.g.
		;; "Foo-bar" or "Foo\bar" or "Foo's".
		(chars-separator '(?\\ ?- ?' ?.))

		(word-chars "[:alnum:]")
		(word-skip
		 (list "a" "an" "and" "as" "at" "but" "by"
			   "for" "if" "in" "is" "nor" "of"
			   "on" "or" "so" "the" "to" "yet"))
		(is-first t))
	(cond
	 ((region-active-p)
	  (setq beg (region-beginning))
	  (setq end (region-end)))
	 (t
	  (setq beg (line-beginning-position))
	  (setq end (line-end-position))))
	(save-excursion
	  ;; work on uppercased text (e.g., headlines) by downcasing first
      (downcase-region beg end)
	  (goto-char beg)

	  (while (< (point) end)
		(setq prev-word-end (point))
		(skip-chars-forward (concat "^" word-chars) end)
		(let ((word-end
			   (save-excursion
				 (skip-chars-forward word-chars end)
				 (point))))

		  (unless (memq (char-before (point)) chars-separator)
			(let* ((c-orig (char-to-string (char-after (point))))
				   (c-up (capitalize c-orig)))
			  (unless (string-equal c-orig c-up)
				(let ((word (buffer-substring-no-properties (point) word-end)))
				  (when
					  (or
					   ;; Always allow capitalization.
					   is-first
					   ;; If it's not a skip word, allow.
					   (not (member word word-skip))
					   ;; Check the beginning of the previous word doesn't reset first.
					   (save-excursion
						 (and
						  (not (zerop
								(skip-chars-backward "[:blank:]" prev-word-end)))
						  (memq (char-before (point)) chars-skip-reset))))
					(delete-region (point) (1+ (point)))
					(insert c-up))))))
		  (goto-char word-end)
		  (setq is-first nil))))))

;; replace the capitalize-region keybinding to call title-case
(global-set-key [remap capitalize-region] 'cj/title-case-region)

(defun cj/upcase-dwim ()
  "Upcase the active region, or upcase the symbol at point if no region."
  (interactive)
  (if (use-region-p)
	  (upcase-region (region-beginning) (region-end))
	(let ((bounds (bounds-of-thing-at-point 'symbol)))
	  (if bounds
		  (upcase-region (car bounds) (cdr bounds))
		(user-error "No symbol at point")))))

(defun cj/downcase-dwim ()
  "Downcase the active region, or downcase the symbol at point if no region."
  (interactive)
  (if (use-region-p)
	  (downcase-region (region-beginning) (region-end))
	(let ((bounds (bounds-of-thing-at-point 'symbol)))
	  (if bounds
		  (downcase-region (car bounds) (cdr bounds))
		(user-error "No symbol at point")))))

;; Case-change operations prefix and keymap
(define-prefix-command 'cj/case-map nil
					   "Keymap for case-change operations.")
(define-key cj/custom-keymap "c" 'cj/case-map)
(define-key cj/case-map "t" 'cj/title-case-region)
(define-key cj/case-map "u" 'cj/upcase-dwim)
(define-key cj/case-map "l" 'cj/downcase-dwim) ;; for "lower" case

(provide 'custom-functions)
;;; custom-functions.el ends here.
