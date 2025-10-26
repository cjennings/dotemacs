;;; custom-line-paragraph.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides line and paragraph manipulation utilities.
;; These utilities enhance text editing and formatting capabilities.
;;
;; Functions include:
;; - joining lines in a region or the current line with the previous one
;; - joining entire paragraphs into single lines
;; - duplicating lines or regions (with optional commenting)
;; - removing duplicate lines
;; - removing lines containing specific text
;; - underlining text with a custom character
;;
;; Bound to keymap prefix  C-; l
;;
;;; Code:


(defun cj/join-line-or-region ()
  "Join lines in the active region or join the current line with the previous one."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1))
        (goto-char end)
        (newline)
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
Comment the duplicated text when optional COMMENT is non-nil."
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
(keymap-set cj/custom-keymap "l" cj/line-and-paragraph-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; l" "line and paragraph menu")
  (which-key-add-key-based-replacements "C-; l c" "duplicate and comment"))

(provide 'custom-line-paragraph)
;;; custom-line-paragraph.el ends here.
