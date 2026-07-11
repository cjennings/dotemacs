;;; custom-case.el --- Custom Functions Handling Text Case -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/C.
;; Load shape: eager.
;; Eager reason: registers its C-; c case submap at load. Currently eager by
;;   init order; a deferral candidate for Phase 3/4 (command/autoload +
;;   registration API).
;; Top-level side effects: defines cj/case-map, registers it under C-; c, remaps
;;   capitalize-region.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Utilities for changing text case.
;; - cj/title-case-region: Title-cases the active region, or the current line if
;;   no region. Follows common English rules: major words capitalized; minor words
;;   (a, an, and, as, at, but, by, for, if, in, is, nor, of, on, or, so, the, to, yet)
;;   lowercased except at the start or after :, !, or ?. Avoids capitalizing
;;   immediately after separators like -, \, ' or .; downcases first, then fixes caps.
;; - cj/upcase-dwim / cj/downcase-dwim: operate on the region, or on the symbol at
;;   point when no region is active.
;;
;; Integration: remaps capitalize-region to cj/title-case-region and exposes a case
;; submenu on cj/custom-keymap under "c": t (title), u (upcase), l (downcase).
;;
;;; Code:

(require 'keybindings)  ;; provides cj/custom-keymap

(defun cj/upcase-dwim ()
  "Upcase the active region, or upcase the symbol at point if no region."
  (interactive)
  (if (use-region-p)
	  (upcase-region (region-beginning) (region-end))
	(let ((bounds (bounds-of-thing-at-point 'symbol)))
	  (if bounds
		  (upcase-region (car bounds) (cdr bounds))
		(user-error "No symbol at point")))))

(defun cj/downcase-dwim ()
  "Downcase the active region, or downcase the symbol at point if no region."
  (interactive)
  (if (use-region-p)
	  (downcase-region (region-beginning) (region-end))
	(let ((bounds (bounds-of-thing-at-point 'symbol)))
	  (if bounds
		  (downcase-region (car bounds) (cdr bounds))
		(user-error "No symbol at point")))))

(defun cj/--title-case-capitalize-word-p (word is-first is-last prev-word-end word-skip chars-skip-reset)
  "Return non-nil when WORD at point should be capitalized in title case.
Point is at WORD's first character.  WORD is capitalized when it is the first
word (IS-FIRST) or the last word (IS-LAST), is not a minor skip word (in
WORD-SKIP), or immediately follows a skip-reset character (one of
CHARS-SKIP-RESET: : ! ? .), reached by skipping blanks back to PREV-WORD-END."
  (or is-first
      is-last
      (not (member word word-skip))
      (save-excursion
        (and (not (zerop (skip-chars-backward "[:blank:]" prev-word-end)))
             (memq (char-before (point)) chars-skip-reset)))))

(defun cj/title-case-region ()
  "Capitalize the region in title case format.
Title case is a capitalization convention where major words are capitalized,
and most minor words are lowercase.  Nouns, verbs (including linking verbs),
adjectives, adverbs,pronouns, and all words of four letters or more are
considered major words. Short (i.e., three letters or fewer) conjunctions,
short prepositions, and all articles are considered minor words.  The first
and last words are always capitalized, and a word following a sentence-ending
period (or a colon, exclamation mark, or question mark) restarts
capitalization even when it is a minor word."
  (interactive)
  (let ((beg nil)
        (end nil)
        (prev-word-end nil)
        (last-word-start nil)
        ;; Restart capitalization for a minor word after one of these, so
        ;; "Warning: An Example" capitalizes the "An" and a sentence-ending
        ;; period capitalizes the next word ("End. The Next").
        (chars-skip-reset '(?: ?! ?? ?.))
        ;; Don't capitalize characters directly after these. e.g.
        ;; "Foo-bar" or "Foo\bar" or "Foo's".  A period is here too, so
        ;; "3.14" and "foo.bar" are left alone; a period followed by a
        ;; blank still restarts via `chars-skip-reset' above.
        (chars-separator '(?\\ ?- ?' ?.))
        (word-chars "[:alnum:]")
        ;; "is" and other linking verbs are major words, so they are not in
        ;; this minor-word skip list.
        (word-skip
         (list "a" "an" "and" "as" "at" "but" "by"
               "for" "if" "in" "nor" "of"
               "on" "or" "so" "the" "to" "yet"))
        (is-first t))
    (cond
     ((region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end)))
     (t
      (setq beg (line-beginning-position))
      (setq end (line-end-position))))
    ;; The last word is always capitalized in title case, so find its start
    ;; once: from END, skip back over any trailing non-word chars, then over
    ;; the word itself.
    (setq last-word-start
          (save-excursion
            (goto-char end)
            (skip-chars-backward (concat "^" word-chars) beg)
            (skip-chars-backward word-chars beg)
            (point)))
    (save-excursion
      ;; work on uppercased text (e.g., headlines) by downcasing first
      (downcase-region beg end)
      (goto-char beg)
      (while (< (point) end)
        (setq prev-word-end (point))
        (skip-chars-forward (concat "^" word-chars) end)
        (when (>= (point) end)  ;; no word chars remaining
          (goto-char end))
        (let ((word-end
               (save-excursion
                 (skip-chars-forward word-chars end)
                 (point))))
          (unless (or (>= (point) end)
                      (memq (char-before (point)) chars-separator))
            (let* ((c-orig (char-to-string (char-after (point))))
                   (c-up (capitalize c-orig)))
              (unless (string-equal c-orig c-up)
                (let ((word (buffer-substring-no-properties (point) word-end)))
                  (when (cj/--title-case-capitalize-word-p
                         word is-first (= (point) last-word-start)
                         prev-word-end word-skip chars-skip-reset)
                    (delete-region (point) (1+ (point)))
                    (insert c-up))))))
          (goto-char word-end)
          (setq is-first nil))))))

;; replace the capitalize-region keybinding to call title-case
(keymap-global-set "<remap> <capitalize-region>" #'cj/title-case-region)

;; Case-change operations prefix and keymap
(defvar-keymap cj/case-map
  :doc "Keymap for case-change operations"
  "t" #'cj/title-case-region
  "u" #'cj/upcase-dwim
  "l" #'cj/downcase-dwim)
(cj/register-prefix-map "c" cj/case-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; c" "case change menu"
    "C-; c t" "title case"
    "C-; c u" "upcase"
    "C-; c l" "downcase"))

(provide 'custom-case)
;;; custom-case.el ends here.
