;;; custom-functions.el --- My Custom Functions and Keymaps -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;These are custom utility functions which I use frequently. They are bound to a
;;personal keymap with a prefix of "C-;" created at the end of this file.

;;; Code:

(use-package subr-x
  :ensure nil) ;; built-in
(use-package expand-region
  :demand t)

;; ------------------------ Jump To Matching Parentheses -----------------------
;; shows you the other matching parenthesis by jumping to it.

(defun cj/jump-to-matching-paren ()
  "If on a parenthesis, jump to it's match.  Otherwise, complain."
  (interactive)
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t (message "Cursor doesn't follow parenthesis, so there's no match."))))

;; ---------------------------- Join Line Or Region ----------------------------
;; joins all selected lines	and fixes up the whitespace.

(defun cj/join-line-or-region (beg end)
  "Apply \='join-line\=' over the marked region or join with previous line.
Region indicated with BEG and END."
  (interactive "r")
  ;; in region
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        ;; apply join lines until point => end
        (while (< (point) end)
          (join-line 1))
        (goto-char end)
        (newline)))
  ;; outside region
  (join-line)(newline))

;; ------------------------------- Join Paragraph ------------------------------
;; expands the region to the paragraph, then joins lines and fixes whitespace.

(defun cj/join-paragraph ()
  "Mark all text in a paragraph then run cj/join-line-or-region."
  (interactive)
  (er/mark-paragraph) ;; from package expand region
  (cj/join-line-or-region)
  (forward-line))

;; ---------------------- Count Words In Buffer Or Region ----------------------
;; minibuffer messages the number of words in the buffer (or region if selected).

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

;; -------------------------- Duplicate Line Or Region -------------------------
;; duplicates the current line on a new line below. With "C-u" the new line's
;; commented. when a region is selected, the whole region is duplicated.

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

;; ---------------- Remove Duplicate Lines From Region Or Buffer ---------------
;; removes all duplicate lines from the region or buffer

(defun cj/remove-duplicate-lines-from-region-or-buffer (start end)
  "Find duplicate lines in region START to END keeping the first occurrence.
If no region is selected, operate on the whole buffer."
  (interactive "*r\nP")
  (save-excursion
    (unless (region-active-p)
      (setq start (point-min) end (point-max)))
    (setq end (copy-marker end))
    (while
        (progn
          (goto-char start)
          (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
      (replace-match "\\1\n\\2"))))

;; -------------------------- Format Region Or Buffer --------------------------
;; reindent, untabify, and delete trailing whitespace across region or buffer

(defun cj/format-region-or-buffer ()
  "Reformat the region or the entire buffer.
If a region is selected, delete trailing whitespace, then indent and untabify
the region. If no region is selected, perform the same actions across the
buffer."

  (interactive)
  (let (start-pos end-pos)
    (if (use-region-p)
        (progn
          (setq start-pos (region-beginning))
          (setq end-pos (region-end)))
      (setq start-pos (point-min))
      (setq end-pos (point-max)))
    (save-excursion
      (delete-trailing-whitespace start-pos end-pos)
      (indent-region start-pos end-pos nil)
      (untabify start-pos end-pos))))

;; ------------------- Remove Leading And Trailing Whitespace ------------------
;; removes leading and trailing whitespace on line, region, or buffer.

(defun cj/remove-leading-trailing-whitespace (start end)
  "Remove leading and trailing whitespace in a region or buffer.
When called interactively, if a region is active, remove leading
and trailing spaces in the region. Else, remove from the current line.
If called with a prefix argument (C-u), remove throughout the entire buffer.
START and END define region."
  (interactive "r")
  (let (deactivate-mark)
	(if (or (use-region-p) current-prefix-arg)
		(save-restriction
		  (if current-prefix-arg
			  (progn (widen) (setq start (point-min) end (point-max)))
			(narrow-to-region start end))
		  (goto-char (point-min))
		  (while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
		  (goto-char (point-min))
		  (while (re-search-forward "[ \t]+$" nil t) (replace-match "")))
	  (beginning-of-line)
	  (while (looking-at "^[ \t]+") (replace-match ""))
	  (end-of-line)
	  (while (re-search-backward "[ \t]+$" (line-beginning-position) t)
		(replace-match "")))))

;; --------------------------- Arrayify / Unarrayify ---------------------------
;; unquoted text on newlines to quoted comma separated strings (and vice-versa).

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

;; ----------------------- Comma Separated Text To Lines -----------------------
;; like arrayify, just without the quotes

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

;; ----------------------- Alphabetize And Replace Region ----------------------
;; sorts selected words into alphabetical order, then replaces the region.

(defun cj/alphabetize-and-replace-region ()
  "Alphabetize strings (words/tokens) in region replacing the original region.
The result will be comma separated."
  (interactive)
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

;; --------------------- Wrap Region As Markdown Code Block --------------------
;; wrap the selection in triple backslash and indicate the language for markdown

(defun cj/wrap-region-as-code-span (start end)
  "Wraps the region between START and END with triple backticks and descriptor.
Triple backicks are often used to indicate a code-span block in markdown.
User is prompted for the optional descriptor."
  (interactive "r")
  (let ((lang (read-string "Descriptor (e.g., code, bash, python): ")))
    (save-excursion
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert "```\n")
      (goto-char start)
	  (insert (concat "```" lang "\n")))))

;; ------------------------ Insert Around Word Or Region -----------------------

(defun cj/insert-around-word-or-region ()
  "Prompt for a string, insert it before and after the word at point or selected region."
  (interactive)
  (let ((str (read-string "Enter a string: "))
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

(global-set-key (kbd "C-; i a") 'cj/insert-around-word-or-region)
;; ------------------------ Insert Around Word Or Region -----------------------

(defun cj/insert-around-word-or-region ()
  "Prompt for a string, insert it before and after the word at point or selected region."
  (interactive)
  (let ((str (read-string "Enter a string: "))
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

;; -------------------- Append To Lines In Region Or Buffer --------------------
;; append characters to the end of all lines in the region or the whole buffer.

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

;; ------------------------------ Hyphenate Region -----------------------------
;; hyphenates any empty space in a region; complains if there's no Region

(defun cj/hyphenate-region (start end)
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

;; ----------------------------- Title Case Region -----------------------------
;; a literate version of capitalize region for prose text.

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
                          (not (zerop (skip-chars-backward "[:blank:]" prev-word-end)))
                          (memq (char-before (point)) chars-skip-reset))))
                    (delete-region (point) (1+ (point)))
                    (insert c-up))))))
          (goto-char word-end)
          (setq is-first nil))))))
;; replace the capitalize-region keybinding to call title-case
(global-set-key [remap capitalize-region] 'cj/title-case-region)

;; --------------------------- Buffer Strip Control M --------------------------
;; remove windows carriage return control characters from the buffer

(defun buffer-strip-ctrl-m ()
  "Remove ^M from the current buffer."
  (interactive)
  (save-excursion
	(goto-char (point-min))
    (while (search-forward "^M" nil t)
      (replace-match "" nil t))))

;; ------------------------------ Insert Date Time -----------------------------
;; insert a sortable or a readable datestamp or timestamp

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

(defvar sortable-date-format "%Y-%m-%d "
  "Time format to insert with `insert-current-time' func.
See help of `format-time-string' for possible replacements")

(defun cj/insert-sortable-date ()
  "Insert the current time into current buffer.
Uses `sortable-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string sortable-date-format (current-time))))

;; -------------------------- Copy Link To Source File -------------------------
;; find the source file for the current buffer and place it's URL in the clipboard

(defun cj/copy-link-to-source-file ()
  "Copy the full file:// path of the underlying source file to the kill ring."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (setq file-path (concat "file://" file-path))
      (kill-new file-path)
      (message "Copied file link to kill ring: %s" file-path))))

;; ------------------------- Buffer And File Operations ------------------------
;; move, rename, or delete the underlying source file for the current buffer.

;; MOVE BUFFER + FILE
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
(global-set-key (kbd "C-x x m") 'cj/move-buffer-and-file)

;; RENAME BUFFER + FILE
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
(global-set-key (kbd "C-x x r") 'cj/rename-buffer-and-file)

;; DELETE BUFFER + FILE
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
(global-set-key (kbd "C-x x d") 'cj/delete-buffer-and-file)

;; ------------------------------- Ordinal Suffix ------------------------------
;; add the proper ordinal to a number (e.g., 1st, 2nd, 3rd, 4th).
;; Stolen from `diary.el' (`diary-ordinal-suffix').

(defun ordinal-suffix (n)
  "Ordinal suffix for N.  That is, `st', `nd', `rd', or `th', as appropriate."
  (if (or (memq (% n 100) '(11 12 13)) (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

;; -------------------------------- Align-Regexp -------------------------------
;; the built-in align regexp shouldn't use tabs

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Avoid tabs when aligning text."
  (let ((indent-tabs-mode nil))
	ad-do-it))

;; ----------------------------- Merge List To List ----------------------------
;; Convenience method for merging two lists together
;; https://emacs.stackexchange.com/questions/38008/adding-many-items-to-a-list/68048#68048

(defun cj/merge-list-to-list (dst src)
  "Merge content of the 2nd list SRC with the 1st one DST."
  (set dst
	   (append (eval dst) src)))

;; ------------------------------ Personal Keymap ------------------------------
;; a keymap to use the above functions. prefix key: "C-;"

(global-unset-key (kbd "C-;"))
(defvar personal-keymap
  (let ((map (make-sparse-keymap)))
    ;; un/arrayify
    (define-key map "a" 'cj/arrayify)
    (define-key map "A" 'cj/unarrayify)
    ;; de/duplicate lines
    (define-key map "d" 'cj/duplicate-line-or-region)
    (define-key map "D" 'cj/remove-duplicate-lines-from-region-or-buffer)

	(define-key map ")" #'cj/jump-to-matching-paren)
	(define-key map "-" #'cj/hyphenate-region)
	(define-key map "U" 'upcase-region)
	(define-key map "w" 'cj/remove-leading-trailing-whitespace)
	(define-key map "#" 'cj/count-words-buffer-or-region)
	(define-key map "1" 'cj/alphabetize-and-replace-region)
	(define-key map "C" 'display-fill-column-indicator-mode)
	(define-key map "J" 'cj/join-paragraph)
	(define-key map "f" 'cj/format-region-or-buffer)
	(define-key map "j" 'cj/join-line-or-region)
	(define-key map "l" 'downcase-dwim)
	(define-key map "p" 'cj/append-to-lines-in-region-or-buffer)
	(define-key map "r" 'align-regexp)
	(define-key map "u" 'cj/title-case-region)
	(define-key map "c" 'cj/wrap-region-as-code-span)
    map)
  "My personal key map.")
(global-set-key (kbd "C-;") personal-keymap)

;; timestamp insertion
(global-set-key (kbd "C-; i h") 'cj/insert-readable-date-time)
(global-set-key (kbd "C-; i s") 'cj/insert-sortable-date-time)
(global-set-key (kbd "C-; i t") 'cj/insert-sortable-time)
(global-set-key (kbd "C-; i d") 'cj/insert-sortable-date)
;; buffer and file operations
(global-set-key (kbd "C-; b r") 'cj/rename-buffer-and-file)
(global-set-key (kbd "C-; b d") 'cj/delete-buffer-and-file)
(global-set-key (kbd "C-; b m") 'cj/move-buffer-and-file)
;; copy link to source file
(global-set-key (kbd "C-; b l") 'cj/copy-link-to-source-file)
;; insert around
(global-set-key (kbd "C-; i a") 'cj/insert-around-word-or-region)

(provide 'custom-functions)
;;; custom-functions.el ends here.
