;;; custom-text-transform.el --- Text glyph transforms -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L.
;; Load shape: eager.
;; Eager reason: registers its C-; / command binding at load.
;; Top-level side effects: binds cj/replace-fraction-glyphs under C-; /.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Convert between text fractions (1/4) and their Unicode glyphs (1/4 becomes
;; the vulgar-fraction character), over the region or whole buffer.  Split out
;; of the former custom-misc.el grab-bag.

;;; Code:

(require 'keybindings)  ;; provides cj/register-command

(defun cj/--replace-fraction-glyphs (start end to-glyphs)
  "Internal implementation: Replace fraction glyphs or text between START and END.
START and END define the region to operate on.
TO-GLYPHS when non-nil converts text (1/4) to glyphs (¼),
otherwise converts glyphs to text."
  (when (> start end)
    (error "Invalid region: start (%d) is greater than end (%d)" start end))
  (let ((replacements (if to-glyphs
                          '(("1/4" . "¼")
                            ("1/2" . "½")
                            ("3/4" . "¾")
                            ("1/3" . "⅓")
                            ("2/3" . "⅔"))
                        '(("¼" . "1/4")
                          ("½" . "1/2")
                          ("¾" . "3/4")
                          ("⅓" . "1/3")
                          ("⅔" . "2/3"))))
        (count 0)
        (end-marker (copy-marker end)))
    (save-excursion
      (dolist (r replacements)
        (goto-char start)
        (while (search-forward (car r) end-marker t)
          (replace-match (cdr r))
          (setq count (1+ count)))))
    count))

(defun cj/replace-fraction-glyphs (start end)
  "Replace common fraction glyphs between START and END.
Operate on the buffer or region designated by START and END.
Replace the text representations with glyphs when called with a
\\[universal-argument] prefix."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((count (cj/--replace-fraction-glyphs start end current-prefix-arg)))
    (message "Replaced %d fraction%s" count (if (= count 1) "" "s"))))

(cj/register-command "/" #'cj/replace-fraction-glyphs "fraction glyphs")

(provide 'custom-text-transform)
;;; custom-text-transform.el ends here
