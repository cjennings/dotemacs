;;; custom-datetime.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Utilities for inserting date/time stamps in multiple formats.
;;
;; Interactive commands:
;; - cj/insert-readable-date-time
;; - cj/insert-sortable-date-time
;; - cj/insert-sortable-time
;; - cj/insert-readable-time
;; - cj/insert-sortable-date
;; - cj/insert-readable-date
;;
;; Each command uses a corresponding format variable:
;;   readable-date-time-format, sortable-date-time-format,
;;   sortable-time-format, readable-time-format,
;;   sortable-date-format, readable-date-format.
;; Customize these (see =format-time-string') to change output.
;; Some defaults include a trailing space for convenient typing.
;;
;; Key bindings:
;; A prefix map =cj/datetime-map' is installed on "d" under =cj/custom-keymap':
;;   r → readable date+time
;;   s → sortable date+time
;;   t → sortable time
;;   T → readable time
;;   d → sortable date
;;   D → readable date
;;
;;; Code:

(eval-when-compile (require 'keybindings))

;; ----------------------------- Readable Date Time ----------------------------

(defvar readable-date-time-format "%A, %B %d, %Y at %I:%M:%S %p %Z "
  "Format string used by `cj/insert-readable-date-time'.
See `format-time-string' for possible replacements.")

(defun cj/insert-readable-date-time ()
  "Insert the current date and time into the current buffer.
Use `readable-date-time-format' for formatting."
  (interactive)
  (insert (format-time-string readable-date-time-format (current-time))))

;; ----------------------------- Sortable Date Time ----------------------------

(defvar sortable-date-time-format "%Y-%m-%d %a @ %H:%M:%S %z "
  "Format string used by `cj/insert-sortable-date-time'.
See `format-time-string' for possible replacements.")

(defun cj/insert-sortable-date-time ()
  "Insert the current date and time into the current buffer.
Use `sortable-date-time-format' for formatting."
  (interactive)
  (insert (format-time-string sortable-date-time-format (current-time))))

;; ------------------------------- Sortable Time -------------------------------

(defvar sortable-time-format "%I:%M:%S %p %Z "
  "Format string used by `cj/insert-sortable-time'.
See `format-time-string' for possible replacements.")

(defun cj/insert-sortable-time ()
  "Insert the current time into the current buffer.
Use `sortable-time-format' for formatting."
  (interactive)
  (insert (format-time-string sortable-time-format (current-time))))

;; ------------------------------- Readable Time -------------------------------

(defvar readable-time-format  "%-I:%M %p "
  "Format string used by `cj/insert-readable-time'.
See `format-time-string' for possible replacements.")

(defun cj/insert-readable-time ()
  "Insert the current time into the current buffer.
Use `readable-time-format' for formatting."
  (interactive)
  (insert (format-time-string readable-time-format (current-time))))

;; ------------------------------- Sortable Date -------------------------------

(defvar sortable-date-format "%Y-%m-%d %a"
  "Format string used by `cj/insert-sortable-date'.
See `format-time-string' for possible replacements.")

(defun cj/insert-sortable-date ()
  "Insert the current date into the current buffer.
Use `sortable-date-format' for formatting."
  (interactive)
  (insert (format-time-string sortable-date-format (current-time))))

;; ------------------------------- Readable Date -------------------------------

(defvar readable-date-format "%A, %B %d, %Y"
  "Format string used by `cj/insert-readable-date'.
See `format-time-string' for possible replacements.")

(defun cj/insert-readable-date ()
  "Insert the current date into the current buffer.
Use `readable-date-format' for formatting."
  (interactive)
  (insert (format-time-string readable-date-format (current-time))))

;; ------------------------------ Date Time Keymap -----------------------------

;; Date/time insertion prefix and keymap

(defvar-keymap cj/datetime-map
  :doc "Keymap for date/time insertions."
  "r" #'cj/insert-readable-date-time
  "s" #'cj/insert-sortable-date-time
  "t" #'cj/insert-sortable-time
  "T" #'cj/insert-readable-time
  "d" #'cj/insert-sortable-date
  "D" #'cj/insert-readable-date )
(keymap-set cj/custom-keymap "d" cj/datetime-map)

(provide 'custom-datetime)
;;; custom-datetime.el ends here.
