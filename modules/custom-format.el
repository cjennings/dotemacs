;;; custom-format.el --- Region and buffer reformatting -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L.
;; Load shape: eager.
;; Eager reason: registers its C-; f command binding at load.
;; Top-level side effects: binds cj/format-region-or-buffer under C-; f.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Reformat the active region, or the whole buffer when no region is active:
;; untabify, reindent, and delete trailing whitespace.  Split out of the
;; former custom-misc.el grab-bag.

;;; Code:

(require 'keybindings)  ;; provides cj/register-command

(defun cj/--format-region (start end)
  "Internal implementation: Reformat text between START and END.
START and END define the region to operate on.
Replaces tabs with spaces, reindents, and deletes trailing whitespace."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max)))))

(defun cj/format-region-or-buffer ()
  "Reformat the region or the entire buffer.
Replaces tabs with spaces, deletes trailing whitespace, and reindents."
  (interactive)
  (let ((start-pos (if (use-region-p) (region-beginning) (point-min)))
        (end-pos (if (use-region-p) (region-end) (point-max))))
    (cj/--format-region start-pos end-pos)
    (message "Formatted %s" (if (use-region-p) "region" "buffer"))))

(cj/register-command "f" #'cj/format-region-or-buffer "format buffer")

(provide 'custom-format)
;;; custom-format.el ends here
