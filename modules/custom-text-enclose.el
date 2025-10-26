;;; custom-text-enclose.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; This module provides functions to surround words or regions with custom strings, and to append or prepend text to lines.

;; It includes three main functions:
;; - surround word or region with a user-specified string
;; - append text to the end of lines
;; - prepend text to the beginning of lines

;; All functions work on both the active region and the entire buffer when no region is selected.

;; Bound to keymap prefix C-; s

;;; Code:

(defun cj/surround-word-or-region ()
  "Surround the word at point or active region with a string read from the minibuffer."
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
  "Append STR to the end of each line in the region or entire buffer."
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
  "Prepend STR to the beginning of each line in the region or entire buffer."
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
(defvar-keymap cj/enclose-map
  :doc "Keymap for enclosing text: surrounding, appending, and prepending"
  "s" #'cj/surround-word-or-region
  "a" #'cj/append-to-lines-in-region-or-buffer
  "p" #'cj/prepend-to-lines-in-region-or-buffer)

(keymap-set cj/custom-keymap "s" cj/enclose-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; s" "text enclose menu"))

(provide 'custom-text-enclose)
;;; custom-text-enclose.el ends here.
