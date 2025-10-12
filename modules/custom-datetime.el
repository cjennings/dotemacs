;;; custom-datetime.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:


(defvar readable-date-time-format "%A, %B %d, %Y at %I:%M:%S %p %Z "
  "Format string used by `cj/insert-readable-date-time'.

See `format-time-string' for possible replacements.")

(defun cj/insert-readable-date-time ()
  "Insert the current date and time into the current buffer.

Use `readable-date-time-format' for formatting."
  (interactive)
  (insert (format-time-string readable-date-time-format (current-time))))

(defvar sortable-date-time-format "%Y-%m-%d %a @ %H:%M:%S %z "
  "Format string used by `cj/insert-sortable-date-time'.

See `format-time-string' for possible replacements.")

(defun cj/insert-sortable-date-time ()
  "Insert the current date and time into the current buffer.

Use `sortable-date-time-format' for formatting."
  (interactive)
  (insert (format-time-string sortable-date-time-format (current-time))))

(defvar sortable-time-format "%I:%M:%S %p %Z "
  "Format string used by `cj/insert-sortable-time'.

See `format-time-string' for possible replacements.")

(defun cj/insert-sortable-time ()
  "Insert the current time into the current buffer.

Use `sortable-time-format' for formatting."
  (interactive)
  (insert (format-time-string sortable-time-format (current-time))))

(defvar readable-time-format  "%-I:%M %p "
  "Format string used by `cj/insert-readable-time'.

See `format-time-string' for possible replacements.")

(defun cj/insert-readable-time ()
  "Insert the current time into the current buffer.

Use `readable-time-format' for formatting."
  (interactive)
  (insert (format-time-string readable-time-format (current-time))))

(defvar sortable-date-format "%Y-%m-%d %a"
  "Format string used by `cj/insert-sortable-date'.

See `format-time-string' for possible replacements.")

(defun cj/insert-sortable-date ()
  "Insert the current date into the current buffer.

Use `sortable-date-format' for formatting."
  (interactive)
  (insert (format-time-string sortable-date-format (current-time))))

(defvar readable-date-format "%A, %B %d, %Y"
  "Format string used by `cj/insert-readable-date'.

See `format-time-string' for possible replacements.")

(defun cj/insert-readable-date ()
  "Insert the current date into the current buffer.

Use `readable-date-format' for formatting."
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

(provide 'custom-datetime)
;;; custom-datetime.el ends here.
