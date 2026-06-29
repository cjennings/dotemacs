;;; custom-line-paragraph.el --- Line and paragraph editing commands -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/C.
;; Load shape: eager.
;; Eager reason: registers its C-; l line/paragraph submap at load. Currently
;;   eager by init order; a deferral candidate for Phase 3/4 (command/autoload +
;;   registration API).
;; Top-level side effects: defines cj/line-and-paragraph-map, registers it under
;;   C-; l.
;; Runtime requires: keybindings (expand-region on demand via declare-function).
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Line and paragraph transforms under C-; l: join, duplicate, delete matching
;; lines, remove duplicates, and underline text. Commands operate on the active
;; region when present and otherwise on the current line or paragraph.
;;
;;; Code:


(require 'keybindings)  ;; provides cj/custom-keymap
(declare-function er/mark-paragraph "expand-region") ;; for cj/join-paragraph

(defun cj/join-line-or-region ()
  "Join lines in the active region or join the current line with the previous one."
  (interactive)
  (if (use-region-p)
      ;; Compute the join count up front from the region's line span.
      ;; A position-based loop overshoots — after the final in-region join,
      ;; point still sits before the end marker, so one extra `join-line 1`
      ;; reaches past the region and pulls the next line in, leaving a stray
      ;; space at its head when the trailing newline is reinserted.
      (let* ((beg (region-beginning))
             (end (copy-marker (region-end)))
             (n   (count-lines beg end)))
        (goto-char beg)
        (dotimes (_ (max 0 (1- n)))
          (join-line 1))
        (goto-char end)
        (forward-line 1)
        (deactivate-mark))
    ;; No region - only join if there's a previous line
    (when (> (line-number-at-pos) 1)
      (join-line))
    (end-of-line)
    (newline)))

(defun cj/join-paragraph ()
  "Join all lines in the current paragraph using `cj/join-line-or-region'."
  (interactive)
  (require 'expand-region)
  (er/mark-paragraph)
  (cj/join-line-or-region)
  (forward-line))

(defun cj/duplicate-line-or-region (&optional comment)
  "Duplicate the current line or active region below.
Comment the duplicated text when optional COMMENT is non-nil.
Signal `user-error' when COMMENT is non-nil but the current mode has
no `comment-start' (e.g. `fundamental-mode'), since commenting would
produce malformed output silently."
  (interactive "P")
  (when (and comment (not (and (stringp comment-start)
                                (> (length comment-start) 0))))
    (user-error
     "Cannot comment in %s: no comment syntax defined" major-mode))
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
  "Remove duplicate lines in the region or buffer, keeping the first occurrence.
Operate on the active region when one exists; otherwise operate on the whole
buffer."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (let ((end-marker (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n"
                                 end-marker t))
          (replace-match "\\1\n\\2"))))))

(defun cj/remove-lines-containing (text)
  "Remove all lines containing TEXT.
If region is active, operate only on the region, otherwise on entire buffer.
The operation is undoable."
  (interactive "sRemove lines containing: ")
  (if (string-empty-p text)
      (message "Empty search string - nothing to remove")
    (save-excursion
      (save-restriction
        (let ((region-active (use-region-p))
              (count 0))
          (when region-active
            (narrow-to-region (region-beginning) (region-end)))
          (goto-char (point-min))
          ;; Count lines before deletion
          (while (re-search-forward (regexp-quote text) nil t)
            (setq count (1+ count))
            (beginning-of-line)
            (forward-line))
          ;; Go back and delete
          (goto-char (point-min))
          (delete-matching-lines (regexp-quote text))
          ;; Report what was done
          (message "Removed %d line%s containing '%s' from %s"
                   count
                   (if (= count 1) "" "s")
                   text
                   (if region-active "region" "buffer")))))))

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

;; ------------------------- Line And Paragraph Keymap -------------------------

(defvar-keymap cj/line-and-paragraph-map
  :doc "Keymap for line and paragraph operations."
  "j" #'cj/join-line-or-region
  "J" #'cj/join-paragraph
  "d" #'cj/duplicate-line-or-region
  "c" (lambda () (interactive) (cj/duplicate-line-or-region t))
  "R" #'cj/remove-duplicate-lines-region-or-buffer
  "r" #'cj/remove-lines-containing
  "u" #'cj/underscore-line)
(cj/register-prefix-map "l" cj/line-and-paragraph-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; l" "line and paragraph menu"
    "C-; l j" "join lines"
    "C-; l J" "join paragraph"
    "C-; l d" "duplicate"
    "C-; l c" "duplicate and comment"
    "C-; l R" "remove duplicates"
    "C-; l r" "remove matching"
    "C-; l u" "underscore line"))

;; --- delimiter jump (formerly in custom-misc.el) ---
(defun cj/jump-to-matching-paren ()
  "Jump to the matching delimiter if point is on (or just after) one.
If not on a delimiter, show a message. Respects the current syntax table."
  (interactive)
  (let* ((ca (char-after))
		 (cb (char-before))
		 ;; Check if on opening paren
		 (open-p (and ca (eq (char-syntax ca) ?\()))
		 ;; Check if on or just after closing paren
		 (close-p (or (and ca (eq (char-syntax ca) ?\)))
					  (and cb (eq (char-syntax cb) ?\))))))
	(cond
	 ;; Jump forward from opening
	 (open-p
	  (condition-case err
		  (forward-sexp)
		(scan-error
		 (message "No matching delimiter: %s" (error-message-string err)))))
	 ;; Jump backward from closing
	 (close-p
	  (condition-case err
		  (backward-sexp)
		(scan-error
		 (message "No matching delimiter: %s" (error-message-string err)))))
	 ;; Not on delimiter
	 (t
	  (message "Point is not on a delimiter.")))))

(cj/register-command ")" #'cj/jump-to-matching-paren "jump to paren")

(provide 'custom-line-paragraph)
;;; custom-line-paragraph.el ends here.
