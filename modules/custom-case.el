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

(defconst cj/--title-case-reset-chars '(?: ?! ?? ?.)
  "Characters that restart capitalization for the following word.
So \"Warning: An Example\" capitalizes the \"An\" and a sentence-ending
period capitalizes the next word (\"End. The Next\").")

(defconst cj/--title-case-separator-chars '(?\\ ?- ?' ?.)
  "Characters whose following character is never capitalized.
Covers \"Foo-bar\", \"Foo\\bar\", and \"Foo's\".  The period keeps
\"3.14\" and \"foo.bar\" untouched; a period followed by a blank still
restarts capitalization via `cj/--title-case-reset-chars'.")

(defconst cj/--title-case-minor-words
  '("a" "an" "and" "as" "at" "but" "by"
    "for" "if" "in" "nor" "of"
    "on" "or" "so" "the" "to" "yet")
  "Minor words kept lowercase mid-title.
\"is\" and other linking verbs are major words, so they are not here.")

(defconst cj/--title-case-word-chars "[:alnum:]"
  "skip-chars set that constitutes a word for title-casing.")

(defun cj/--title-case-region-bounds ()
  "Return (BEG . END) for the active region, else the current line."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

(defun cj/--title-case-last-word-start (beg end)
  "Return the start position of the last word in BEG..END.
The last word is always capitalized in title case, so it is located once:
from END, skip back over trailing non-word characters, then the word."
  (save-excursion
    (goto-char end)
    (skip-chars-backward (concat "^" cj/--title-case-word-chars) beg)
    (skip-chars-backward cj/--title-case-word-chars beg)
    (point)))

(defun cj/--title-case-maybe-capitalize (word-end end is-first last-word-start prev-word-end)
  "Capitalize the character at point when title-case rules call for it.
Point sits on a word's first character, WORD-END past its last.  END bounds
the operation; IS-FIRST, LAST-WORD-START, and PREV-WORD-END feed
`cj/--title-case-capitalize-word-p'.  Modifies the buffer in place."
  (unless (or (>= (point) end)
              (memq (char-before (point)) cj/--title-case-separator-chars))
    (let* ((c-orig (char-to-string (char-after (point))))
           (c-up (capitalize c-orig)))
      (unless (string-equal c-orig c-up)
        (let ((word (buffer-substring-no-properties (point) word-end)))
          (when (cj/--title-case-capitalize-word-p
                 word is-first (= (point) last-word-start)
                 prev-word-end cj/--title-case-minor-words
                 cj/--title-case-reset-chars)
            (delete-region (point) (1+ (point)))
            (insert c-up)))))))

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
  (let* ((bounds (cj/--title-case-region-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (last-word-start (cj/--title-case-last-word-start beg end))
         (word-chars cj/--title-case-word-chars)
         (prev-word-end nil)
         (is-first t))
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
          (cj/--title-case-maybe-capitalize
           word-end end is-first last-word-start prev-word-end)
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
