;;; custom-datetime.el --- Insert formatted date and time strings -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/C.
;; Load shape: eager.
;; Eager reason: registers its C-; d datetime submap at load. Currently eager by
;;   init order; a deferral candidate for Phase 3/4 (command/autoload +
;;   registration API).
;; Top-level side effects: defines cj/datetime-map, registers it under C-; d.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Date/time insertion commands under C-; d. Each command is generated from a
;; customizable format variable and inserts format-time-string output at point.
;;
;;; Code:

(require 'keybindings)  ;; provides cj/custom-keymap

(defmacro cj/--define-datetime-inserter (name format-var thing)
  "Define interactive command NAME inserting the current THING at point.
THING is a short noun phrase (\"date and time\", \"time\", \"date\") used in
the docstring.  The inserted text is `format-time-string' applied to
FORMAT-VAR's value, so customizing FORMAT-VAR changes the output."
  (declare (indent defun))
  `(defun ,name ()
     ,(format "Insert the current %s into the current buffer.\nUse `%s' for formatting."
              thing format-var)
     (interactive)
     (insert (format-time-string ,format-var (current-time)))))

;; ----------------------------- Readable Date Time ----------------------------

(defvar readable-date-time-format "%A, %B %d, %Y at %I:%M:%S %p %Z "
  "Format string used by `cj/insert-readable-date-time'.
See `format-time-string' for possible replacements.")

(cj/--define-datetime-inserter cj/insert-readable-date-time
  readable-date-time-format "date and time")

;; ----------------------------- Sortable Date Time ----------------------------

(defvar sortable-date-time-format "%Y-%m-%d %a @ %H:%M:%S %z "
  "Format string used by `cj/insert-sortable-date-time'.
See `format-time-string' for possible replacements.")

(cj/--define-datetime-inserter cj/insert-sortable-date-time
  sortable-date-time-format "date and time")

;; ------------------------------- Sortable Time -------------------------------

(defvar sortable-time-format "%H:%M:%S %Z "
  "Format string used by `cj/insert-sortable-time'.
See `format-time-string' for possible replacements.")

(cj/--define-datetime-inserter cj/insert-sortable-time
  sortable-time-format "time")

;; ------------------------------- Readable Time -------------------------------

(defvar readable-time-format  "%-I:%M %p "
  "Format string used by `cj/insert-readable-time'.
See `format-time-string' for possible replacements.")

(cj/--define-datetime-inserter cj/insert-readable-time
  readable-time-format "time")

;; ------------------------------- Sortable Date -------------------------------

(defvar sortable-date-format "%Y-%m-%d %a"
  "Format string used by `cj/insert-sortable-date'.
See `format-time-string' for possible replacements.")

(cj/--define-datetime-inserter cj/insert-sortable-date
  sortable-date-format "date")

;; ------------------------------- Readable Date -------------------------------

(defvar readable-date-format "%A, %B %d, %Y"
  "Format string used by `cj/insert-readable-date'.
See `format-time-string' for possible replacements.")

(cj/--define-datetime-inserter cj/insert-readable-date
  readable-date-format "date")

;; ------------------------------ Date Time Keymap -----------------------------

(defvar-keymap cj/datetime-map
  :doc "Keymap for date/time insertions."
  "r" #'cj/insert-readable-date-time
  "s" #'cj/insert-sortable-date-time
  "t" #'cj/insert-sortable-time
  "T" #'cj/insert-readable-time
  "d" #'cj/insert-sortable-date
  "D" #'cj/insert-readable-date )
(cj/register-prefix-map "d" cj/datetime-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; d" "date/time insertion menu"
    "C-; d r" "readable date-time"
    "C-; d s" "sortable date-time"
    "C-; d t" "sortable time"
    "C-; d T" "readable time"
    "C-; d d" "sortable date"
    "C-; d D" "readable date"))

(provide 'custom-datetime)
;;; custom-datetime.el ends here.
