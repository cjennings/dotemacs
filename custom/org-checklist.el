;;; org-checklist.el --- org functions for checklist handling  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2014, 2021 James TD Smith

;; Author: James TD Smith (@ ahktenzero (. mohorovi cc))
;; Version: 1.0
;; Keywords: org, checklists
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some functions for handing repeated tasks which involve
;; checking off a list of items. By setting the RESET_CHECK_BOXES property in an
;; item, when the TODO state is set to done all checkboxes under that item are
;; cleared. If the LIST_EXPORT_BASENAME property is set, a file will be created
;; using the value of that property plus a timestamp, containing all the items
;; in the list which are not checked. Additionally the user will be prompted to
;; print the list.
;;
;; I use this for to keep track of stores of various things (food stores,
;; components etc) which I check periodically and use the exported list of items
;; which are not present as a shopping list.
;;
;;; Usage:
;; (require 'org-checklist)
;;
;; Set the RESET_CHECK_BOXES and LIST_EXPORT_BASENAME properties in items as
;; needed.
;;
;; https://orgmode.org/worg/org-contrib/org-checklist.html
;; https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/org-checklist.el
;;
;;; Code:
(require 'org)
(defvar org-state)
;; FIXME: This library requires
;; https://git.savannah.gnu.org/cgit/a2ps.git/tree/contrib/emacs/a2ps-print.el file
;; It is a part of a2ps distribution.
(load "a2ps-print" 'no-error)
(defvar a2ps-switches)
(declare-function a2ps-buffer "a2ps-print" (argp))

(setq org-default-properties (cons "RESET_CHECK_BOXES" (cons "LIST_EXPORT_BASENAME" org-default-properties)))

(defgroup org-checklist nil
  "Extended checklist handling for org"
  :tag "Org-checklist"
  :group 'org)

(defcustom org-checklist-export-time-format "%Y%m%d%H%M"
  "The format of timestamp appended to LIST_EXPORT_BASENAME to
  make the name of the export file."
  :link '(function-link format-time-string)
  :group 'org-checklist
  :type 'string)

(defcustom org-checklist-export-function 'org-export-as-ascii
  "function used to prepare the export file for printing"
  :group 'org-checklist
  :type '(radio (function-item :tag "ascii text" org-export-as-ascii)
		(function-item :tag "HTML"  org-export-as-html)
		(function-item :tag "LaTeX" :value org-export-as-latex)
		(function-item :tag "XOXO" :value org-export-as-xoxo)))

(defcustom org-checklist-export-params nil
  "options for the export function file for printing"
  :group 'org-checklist
  :type '(repeat string))

(defcustom org-checklist-a2ps-params nil
  "options for a2ps for printing"
  :group 'org-checklist
  :type '(repeat string))

(defun org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECK_BOXES")
      (org-reset-checkbox-state-subtree)))


(defun org-make-checklist-export ()
  "Produce a checklist containing all unchecked items from a list
of checkbox items"
  (interactive "*")
  (when (org-entry-get (point) "LIST_EXPORT_BASENAME")
    (let* ((export-file (concat (org-entry-get (point) "LIST_EXPORT_BASENAME" nil)
				"-" (format-time-string
				     org-checklist-export-time-format)
				".org"))
	   (print (pcase (org-entry-get (point) "PRINT_EXPORT" nil)
		    (`(or "" "nil" nil) nil)
		    (`nil (y-or-n-p "Print list? "))
		    (_ t)))
	   exported-lines
	   (title "Checklist export"))
      (save-restriction
	(save-excursion
	  (org-narrow-to-subtree)
	  (org-update-checkbox-count-maybe)
          (if (fboundp 'org-fold-show-subtree)
              (org-fold-show-subtree)
	    (with-no-warnings (org-show-subtree)))
	  (goto-char (point-min))
	  (when (looking-at org-complex-heading-regexp)
	    (setq title (match-string 4)))
	  (goto-char (point-min))
	  (let ((end (point-max)))
	    (while (< (point) end)
	      (when (and (org-at-item-checkbox-p)
			 (or (string= (match-string 0) "[ ]")
			     (string= (match-string 0) "[-]")))
                (setq exported-lines
                      (nconc exported-lines (list (thing-at-point 'line)))))
	      (beginning-of-line 2)))
	  (set-buffer (get-buffer-create export-file))
	  (org-insert-heading)
	  (insert (or title export-file) "\n")
	  (dolist (entry exported-lines) (insert entry))
	  (org-update-checkbox-count-maybe)
	  (write-file export-file)
	  (when print
	    (funcall org-checklist-export-function
		     org-checklist-export-params)
	    (let* ((current-a2ps-switches a2ps-switches)
		   (a2ps-switches (append current-a2ps-switches
					  org-checklist-a2ps-params)))
	      (a2ps-buffer nil))))))))

(defun org-checklist ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-make-checklist-export)
    (org-reset-checkbox-state-maybe)))

(add-hook 'org-after-todo-state-change-hook 'org-checklist)

(provide 'org-checklist)

;;; org-checklist.el ends here
