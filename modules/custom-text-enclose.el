;;; custom-text-enclose.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Text enclosure utilities for wrapping and line manipulation.
;;
;; Wrapping functions:
;; - surround-word-or-region - wrap text with same delimiter on both sides
;; - wrap-word-or-region - wrap with different opening/closing delimiters
;; - unwrap-word-or-region - remove surrounding delimiters
;;
;; Line manipulation:
;; - append-to-lines - add suffix to each line
;; - prepend-to-lines - add prefix to each line
;; - indent-lines - add leading whitespace (spaces or tabs)
;; - dedent-lines - remove leading whitespace
;;
;; Most functions work on region or entire buffer when no region is active.
;;
;; Bound to keymap prefix C-; s

;;; Code:

;; cj/custom-keymap defined in keybindings.el
(eval-when-compile (defvar cj/custom-keymap))

(defun cj/--surround (text surround-string)
  "Internal implementation: Surround TEXT with SURROUND-STRING.
TEXT is the string to be surrounded.
SURROUND-STRING is prepended and appended to TEXT.
Returns the surrounded text without modifying the buffer."
  (concat surround-string text surround-string))

(defun cj/--wrap (text opening closing)
  "Internal implementation: Wrap TEXT with OPENING and CLOSING strings.
TEXT is the string to be wrapped.
OPENING is prepended to TEXT.
CLOSING is appended to TEXT.
Returns the wrapped text without modifying the buffer."
  (concat opening text closing))

(defun cj/surround-word-or-region ()
  "Surround the word at point or active region with a string.
The surround string is read from the minibuffer."
  (interactive)
  (let ((str (read-string "Surround with: "))
        (regionp (use-region-p)))
    (if regionp
        (let ((beg (region-beginning))
              (end (region-end))
              (text (buffer-substring (region-beginning) (region-end))))
          (delete-region beg end)
          (goto-char beg)
          (insert (cj/--surround text str)))
      (if (thing-at-point 'word)
          (let* ((bounds (bounds-of-thing-at-point 'word))
                 (text (buffer-substring (car bounds) (cdr bounds))))
            (delete-region (car bounds) (cdr bounds))
            (goto-char (car bounds))
            (insert (cj/--surround text str)))
        (message "Can't insert around. No word at point and no region selected.")))))

(defun cj/wrap-word-or-region ()
  "Wrap the word at point or active region with different opening/closing strings.
The opening and closing strings are read from the minibuffer."
  (interactive)
  (let ((opening (read-string "Opening: "))
        (closing (read-string "Closing: "))
        (regionp (use-region-p)))
    (if regionp
        (let ((beg (region-beginning))
              (end (region-end))
              (text (buffer-substring (region-beginning) (region-end))))
          (delete-region beg end)
          (goto-char beg)
          (insert (cj/--wrap text opening closing)))
      (if (thing-at-point 'word)
          (let* ((bounds (bounds-of-thing-at-point 'word))
                 (text (buffer-substring (car bounds) (cdr bounds))))
            (delete-region (car bounds) (cdr bounds))
            (goto-char (car bounds))
            (insert (cj/--wrap text opening closing)))
        (message "Can't wrap. No word at point and no region selected.")))))

(defun cj/--unwrap (text opening closing)
  "Internal implementation: Remove OPENING and CLOSING from TEXT if present.
TEXT is the string to unwrap.
OPENING is checked at the start of TEXT.
CLOSING is checked at the end of TEXT.
Returns the unwrapped text if both delimiters present, otherwise unchanged."
  (if (and (string-prefix-p opening text)
           (string-suffix-p closing text)
           (>= (length text) (+ (length opening) (length closing))))
      (substring text (length opening) (- (length text) (length closing)))
    text))

(defun cj/unwrap-word-or-region ()
  "Remove surrounding delimiters from word at point or active region.
The opening and closing strings are read from the minibuffer."
  (interactive)
  (let ((opening (read-string "Opening to remove: "))
        (closing (read-string "Closing to remove: "))
        (regionp (use-region-p)))
    (if regionp
        (let ((beg (region-beginning))
              (end (region-end))
              (text (buffer-substring (region-beginning) (region-end))))
          (delete-region beg end)
          (goto-char beg)
          (insert (cj/--unwrap text opening closing)))
      (if (thing-at-point 'word)
          (let* ((bounds (bounds-of-thing-at-point 'word))
                 (text (buffer-substring (car bounds) (cdr bounds))))
            (delete-region (car bounds) (cdr bounds))
            (goto-char (car bounds))
            (insert (cj/--unwrap text opening closing)))
        (message "Can't unwrap. No word at point and no region selected.")))))

(defun cj/--append-to-lines (text suffix)
  "Internal implementation: Append SUFFIX to each line in TEXT.
TEXT is the string containing one or more lines.
SUFFIX is appended to the end of each line.
Returns the transformed string without modifying the buffer."
  (let* ((lines (split-string text "\n"))
         (has-trailing-newline (string-suffix-p "\n" text))
         ;; If has trailing newline, last element will be empty string - exclude it
         (lines-to-process (if (and has-trailing-newline
                                    (not (null lines))
                                    (string-empty-p (car (last lines))))
                               (butlast lines)
                             lines)))
    (concat
     (mapconcat (lambda (line) (concat line suffix)) lines-to-process "\n")
     (if has-trailing-newline "\n" ""))))

(defun cj/append-to-lines-in-region-or-buffer (str)
  "Append STR to the end of each line in the region or entire buffer."
  (interactive "sEnter string to append: ")
  (let* ((start-pos (if (use-region-p)
                        (region-beginning)
                      (point-min)))
         (end-pos (if (use-region-p)
                      (region-end)
                    (point-max)))
         (text (buffer-substring start-pos end-pos))
         (insertion (cj/--append-to-lines text str)))
    (delete-region start-pos end-pos)
    (goto-char start-pos)
    (insert insertion)))

(defun cj/--prepend-to-lines (text prefix)
  "Internal implementation: Prepend PREFIX to each line in TEXT.
TEXT is the string containing one or more lines.
PREFIX is prepended to the beginning of each line.
Returns the transformed string without modifying the buffer."
  (let* ((lines (split-string text "\n"))
         (has-trailing-newline (string-suffix-p "\n" text))
         ;; If has trailing newline, last element will be empty string - exclude it
         (lines-to-process (if (and has-trailing-newline
                                    (not (null lines))
                                    (string-empty-p (car (last lines))))
                               (butlast lines)
                             lines)))
    (concat
     (mapconcat (lambda (line) (concat prefix line)) lines-to-process "\n")
     (if has-trailing-newline "\n" ""))))

(defun cj/prepend-to-lines-in-region-or-buffer (str)
  "Prepend STR to the beginning of each line in the region or entire buffer."
  (interactive "sEnter string to prepend: ")
  (let* ((start-pos (if (use-region-p)
                        (region-beginning)
                      (point-min)))
         (end-pos (if (use-region-p)
                      (region-end)
                    (point-max)))
         (text (buffer-substring start-pos end-pos))
         (insertion (cj/--prepend-to-lines text str)))
    (delete-region start-pos end-pos)
    (goto-char start-pos)
    (insert insertion)))

(defun cj/--indent-lines (text count use-tabs)
  "Internal implementation: Indent each line in TEXT by COUNT characters.
TEXT is the string containing one or more lines.
COUNT is the number of indentation characters to add.
USE-TABS when non-nil uses tabs instead of spaces for indentation.
Returns the indented text without modifying the buffer."
  (let ((indent-string (if use-tabs
                           (make-string count ?\t)
                         (make-string count ?\s))))
    (cj/--prepend-to-lines text indent-string)))

(defun cj/indent-lines-in-region-or-buffer (count use-tabs)
  "Indent each line in region or buffer by COUNT characters.
COUNT is the number of characters to indent (default 4).
USE-TABS when non-nil (prefix argument) uses tabs instead of spaces."
  (interactive "p\nP")
  (let* ((start-pos (if (use-region-p)
                        (region-beginning)
                      (point-min)))
         (end-pos (if (use-region-p)
                      (region-end)
                    (point-max)))
         (text (buffer-substring start-pos end-pos))
         (insertion (cj/--indent-lines text count use-tabs)))
    (delete-region start-pos end-pos)
    (goto-char start-pos)
    (insert insertion)))

(defun cj/--dedent-lines (text count)
  "Internal implementation: Remove up to COUNT leading characters from each line.
TEXT is the string containing one or more lines.
COUNT is the maximum number of leading whitespace characters to remove.
Removes spaces and tabs, but only up to COUNT characters per line.
Returns the dedented text without modifying the buffer."
  (let* ((lines (split-string text "\n"))
         (has-trailing-newline (string-suffix-p "\n" text))
         (lines-to-process (if (and has-trailing-newline
                                    (not (null lines))
                                    (string-empty-p (car (last lines))))
                               (butlast lines)
                             lines))
         (dedented-lines
          (mapcar
           (lambda (line)
             (let ((removed 0)
                   (pos 0)
                   (len (length line)))
               (while (and (< removed count)
                          (< pos len)
                          (memq (aref line pos) '(?\s ?\t)))
                 (setq removed (1+ removed))
                 (setq pos (1+ pos)))
               (substring line pos)))
           lines-to-process)))
    (concat
     (mapconcat #'identity dedented-lines "\n")
     (if has-trailing-newline "\n" ""))))

(defun cj/dedent-lines-in-region-or-buffer (count)
  "Remove up to COUNT leading whitespace characters from each line.
COUNT is the number of characters to remove (default 4).
Works on region if active, otherwise entire buffer."
  (interactive "p")
  (let* ((start-pos (if (use-region-p)
                        (region-beginning)
                      (point-min)))
         (end-pos (if (use-region-p)
                      (region-end)
                    (point-max)))
         (text (buffer-substring start-pos end-pos))
         (insertion (cj/--dedent-lines text count)))
    (delete-region start-pos end-pos)
    (goto-char start-pos)
    (insert insertion)))

;; Text enclosure keymap
(defvar-keymap cj/enclose-map
  :doc "Keymap for text enclosure: wrapping, line manipulation, and indentation"
  "s" #'cj/surround-word-or-region
  "w" #'cj/wrap-word-or-region
  "u" #'cj/unwrap-word-or-region
  "a" #'cj/append-to-lines-in-region-or-buffer
  "p" #'cj/prepend-to-lines-in-region-or-buffer
  "i" #'cj/indent-lines-in-region-or-buffer
  "d" #'cj/dedent-lines-in-region-or-buffer
  "I" #'change-inner
  "O" #'change-outer)

(keymap-set cj/custom-keymap "s" cj/enclose-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; s" "text enclose menu"
    "C-; s s" "surround text"
    "C-; s w" "wrap text"
    "C-; s u" "unwrap text"
    "C-; s a" "append to lines"
    "C-; s p" "prepend to lines"
    "C-; s i" "indent lines"
    "C-; s d" "dedent lines"
    "C-; s I" "change inner"
    "C-; s O" "change outer"))

(provide 'custom-text-enclose)
;;; custom-text-enclose.el ends here.
