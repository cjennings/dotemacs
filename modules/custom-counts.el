;;; custom-counts.el --- Word and character counts -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L.
;; Load shape: eager.
;; Eager reason: registers its C-; # w and C-; # c command bindings at load.
;; Top-level side effects: binds the count commands under C-; # w and C-; # c.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Count words or characters in the active region, or the whole buffer when no
;; region is active, and report the total in the minibuffer.  Split out of the
;; former custom-misc.el grab-bag.

;;; Code:

(require 'keybindings)  ;; provides cj/register-command

(defun cj/--count-words (start end)
  "Internal implementation: Count words between START and END.
START and END define the region to count.
Returns the word count as an integer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (count-words start end))

(defun cj/count-words-buffer-or-region ()
  "Count the number of words in the buffer or region.
Display the result in the minibuffer."
  (interactive)
  (let* ((use-region (use-region-p))
         (begin (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max)))
         (area-type (if use-region "the region" "the buffer"))
         (word-count (cj/--count-words begin end)))
    (message "There are %d words in %s." word-count area-type)))

(defun cj/--count-characters (start end)
  "Internal implementation: Count characters between START and END.
START and END define the region to count.
Returns the character count as an integer."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (- end start))

(defun cj/count-characters-buffer-or-region ()
  "Count the number of characters in the buffer or region.
Display the result in the minibuffer."
  (interactive)
  (let* ((use-region (use-region-p))
         (begin (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max)))
         (area-type (if use-region "the region" "the buffer"))
         (char-count (cj/--count-characters begin end)))
    (message "There are %d characters in %s." char-count area-type)))

(cj/register-command "# w" #'cj/count-words-buffer-or-region "count words")
(cj/register-command "# c" #'cj/count-characters-buffer-or-region "count characters")

(provide 'custom-counts)
;;; custom-counts.el ends here
