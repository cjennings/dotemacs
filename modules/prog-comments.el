;;; prog-comments.el --- Comments and Underscores -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple utility functions for creating and managing comments.

;;; Code:

;; ------------------------------ Comment Reformat -----------------------------
;; uncomments the selected text,joins into one paragraph,reapplies comments

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
(global-set-key (kbd "C-z c r") 'cj/comment-reformat)

;; ------------------------------ Comment Centered -----------------------------
;; Horizontal comment char with centered text. Defaults to appropriate comments
;; per major mode.

(defun cj/comment-centered (&optional comment-char)
  "Insert comment text centered around the 'COMMENT-CHAR' character.
Will default to the '#' character if called with no arguments. Uses
\\="fill-column"\\= or 80 (whichever is less) to calculate the comment length.
Will begin and end the line  with the appropriate comment symbols based on
programming mode."
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
        (insert " ")
        (insert comment-end)))))
(global-set-key (kbd "C-z c l") 'cj/comment-line)

;; ------------------------------- Comment Hyphen ------------------------------
;; Horizontal dashes with centered text, typically used to indicating sections

(defun cj/comment-hyphen()
  "Insert a centered comment with '-' (hyphens) on each side."
  (interactive)
  (cj/comment-centered "-"))
(global-set-key (kbd "C-z c -") 'cj/comment-hyphen)

;; -------------------------------- Comment Box --------------------------------
;; Traditional comment boxes

(defun cj/comment-box ()
  "Insert a comment with '#' drawn around a string the user inputs.
The box extends to the fill column.  Places the point on the line after the
comment box."
  (interactive)
  (let* ((comment-char "#")
         (comment-pad 4) ; 4 = 2 comment chars & 2 spaces
         (comment (capitalize (string-trim (read-from-minibuffer "Comment: "))))
         (comment-length (length comment)))

    ;; message if the comment doesn't fit on a single line
    (if (> comment-length (- fill-column comment-pad))
        (message "Comment string is too big to fit in one line")
      (progn
        (dotimes (_ (- fill-column 1)) (insert comment-char))
        (newline)
        (insert comment-char)
        (insert " ")
        (insert comment)
        (dotimes(_ (- fill-column comment-length comment-pad)) (insert " ")))
      (insert comment-char)
      (newline)
      (dotimes (_ (- fill-column 1)) (insert comment-char)))))

;; ------------------------------ Underscore Line ------------------------------
;; Underlines the current line with the character of your choosing

(defun cj/underscore-line (char)
  "Insert the number of 'CHAR' underneath the current line to mimic an underscore."
  (interactive "cEnter the character for underlining: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

;; --------------------------- Remove Buffer Comments --------------------------
;; another nice suggestion from malabarba.
;; https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring

(defun cj/remove-buffer-comments ()
  (interactive)
  (goto-char (point-min))
  (let (kill-ring)
    (comment-kill (count-lines (point-min) (point-max)))))


(provide 'prog-comments)
;;; prog-comments.el ends here.
