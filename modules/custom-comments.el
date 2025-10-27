;;; custom-comments.el --- Custom Comment Operations -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;; This module provides custom comment formatting and manipulation utilities for code editing.
;;
;; Functions include:
;; - deleting all comments in a buffer,
;; - reformatting commented text into single-line paragraphs,
;; - creating centered comment headers with customizable separator characters,
;; - creating comment boxes around text
;; - inserting hyphen-style centered comments.
;;
;; These utilities help create consistent, well-formatted code comments and section headers.
;; Bound to keymap prefix: C-; C
;;
;; Comment Style Patterns:
;;
;; inline-border:
;;   ========== inline-border ==========
;;
;; simple-divider:
;;   ====================================
;;   simple-divider
;;   ====================================
;;
;; padded-divider:
;;   ====================================
;;     padded-divider
;;   ====================================
;;
;; box:
;;   ************************************
;;   * box                              *
;;   ************************************
;;
;; heavy-box:
;;   ************************************
;;   *                                  *
;;   *          heavy-box               *
;;   *                                  *
;;   ************************************
;;
;; unicode-box:
;;   ┌──────────────────────────────────┐
;;   │ unicode-box                      │
;;   └──────────────────────────────────┘
;;
;; block-banner:
;;   /************************************
;;    * block-banner
;;    ************************************/
;;
;;; Code:

(eval-when-compile (defvar cj/custom-keymap)) ;; cj/custom-keymap defined in keybindings.el
(autoload 'cj/join-line-or-region "custom-line-paragraph" nil t)

;; ======================== Comment Manipulation Functions =====================

;; --------------------------- Delete Buffer Comments --------------------------

(defun cj/delete-buffer-comments ()
  "Delete all comments within the current buffer."
  (interactive)
  (goto-char (point-min))
  (let (kill-ring)
    (comment-kill (count-lines (point-min) (point-max)))))

;; ------------------------------ Comment Reformat -----------------------------

(defun cj/comment-reformat ()
  "Reformat commented text into a single paragraph."
  (interactive)
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end)))
            (orig-fill-column fill-column))
        (uncomment-region beg end)
        (setq fill-column (- fill-column 3))
        (cj/join-line-or-region)
        (comment-region beg end)
        (setq fill-column orig-fill-column )))
  ;; if no region
  (message "No region was selected. Select the comment lines to reformat."))

;; ======================== Comment Generation Functions =======================

;; ----------------------------- Inline Border ---------------------------------

(defun cj/--comment-inline-border (cmt-start cmt-end decoration-char text length)
  "Internal implementation: Generate single-line centered comment with decoration.
CMT-START and CMT-END are the comment syntax strings.
DECORATION-CHAR is the character to use for borders (string).
TEXT is the comment text (will be centered).
LENGTH is the total width of the line."
  (let* ((current-column-pos (current-column))
         (text-length (length text))
         (comment-start-len (+ (length cmt-start)
                              (if (equal cmt-start ";") 1 0)))  ; doubled semicolon
         ;; Calculate available space for decoration + text + spaces
         (available-width (- length current-column-pos
                            comment-start-len
                            (if (string-empty-p cmt-end) 0 (1+ (length cmt-end)))
                            1))  ; space after comment-start
         ;; Space for decoration on each side (excluding text and its surrounding spaces)
         (space-on-each-side (/ (- available-width
                                  text-length
                                  (if (> text-length 0) 2 0))  ; spaces around text
                               2))
         (min-space 2))
    ;; Validate we have enough space
    (when (< space-on-each-side min-space)
      (error "Length %d is too small for text '%s' (need at least %d more chars)"
             length text (- min-space space-on-each-side)))
    ;; Generate the line
    (insert cmt-start)
    (when (equal cmt-start ";")
      (insert cmt-start))
    (insert " ")
    ;; Left decoration
    (dotimes (_ space-on-each-side)
      (insert decoration-char))
    ;; Text with spaces
    (when (> text-length 0)
      (insert " " text " "))
    ;; Right decoration (handle odd-length text)
    (dotimes (_ (if (= (% text-length 2) 0)
                    (- space-on-each-side 1)
                  space-on-each-side))
      (insert decoration-char))
    ;; Comment end
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)))

(defun cj/comment-inline-border (&optional decoration-char)
  "Insert single-line comment with TEXT centered around DECORATION-CHAR borders.
DECORATION-CHAR defaults to \"#\" if not provided.
Uses the lesser of `fill-column\\=' or 80 for line length."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start)
                           comment-start
                         (read-string "Comment start character(s): ")))
         (comment-end (if (and (boundp 'comment-end) comment-end)
                         comment-end
                       ""))
         (decoration-char (or decoration-char "#"))
         (text (capitalize (string-trim (read-from-minibuffer "Comment: "))))
         (length (min fill-column 80)))
    (cj/--comment-inline-border comment-start comment-end decoration-char text length)))

;; ---------------------------- Simple Divider ---------------------------------

(defun cj/--comment-simple-divider (cmt-start cmt-end decoration-char text length)
  "Internal implementation: Generate a simple divider comment.
CMT-START and CMT-END are the comment syntax strings.
DECORATION-CHAR is the character to use for the divider lines.
TEXT is the comment text.
LENGTH is the total width of each line."
  (let* ((current-column-pos (current-column))
         (min-length (+ current-column-pos
                       (length cmt-start)
                       (if (equal cmt-start ";") 1 0)  ; doubled semicolon
                       1  ; space after comment-start
                       3  ; minimum decoration chars
                       (if (string-empty-p cmt-end) 0 (1+ (length cmt-end))))))
    (when (< length min-length)
      (error "Length %d is too small to generate comment (minimum %d)" length min-length))
    (let* ((available-width (- length current-column-pos
                              (length cmt-start)
                              (if (string-empty-p cmt-end) 0 (1+ (length cmt-end)))))
           (line (make-string available-width (string-to-char decoration-char))))
    ;; Top line
    (insert cmt-start)
    (when (equal cmt-start ";") (insert cmt-start))
    (insert " ")
    (insert line)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)

    ;; Text line
    (dotimes (_ current-column-pos) (insert " "))
    (insert cmt-start)
    (when (equal cmt-start ";") (insert cmt-start))
    (insert " " text)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)

    ;; Bottom line
    (dotimes (_ current-column-pos) (insert " "))
    (insert cmt-start)
    (when (equal cmt-start ";") (insert cmt-start))
    (insert " ")
    (insert line)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline))))

(defun cj/comment-simple-divider ()
  "Insert a simple divider comment banner.
Prompts for decoration character, text, and length option."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start)
                           comment-start
                         (read-string "Comment start character(s): ")))
         (comment-end (if (and (boundp 'comment-end) comment-end)
                         comment-end
                       ""))
         (decoration-char (read-string "Decoration character (default =): " nil nil "="))
         (text (read-string "Comment text: "))
         (length-option (completing-read "Length: "
                                        '("fill-column" "half-column" "match-text")
                                        nil t nil nil "fill-column"))
         (length (cond
                  ((string= length-option "fill-column") fill-column)
                  ((string= length-option "half-column") (/ fill-column 2))
                  ((string= length-option "match-text")
                   (+ (length comment-start)
                      (if (equal comment-start ";") 1 0)
                      1  ; space after comment-start
                      (length text)
                      (if (string-empty-p comment-end) 0 (1+ (length comment-end))))))))
    (cj/--comment-simple-divider comment-start comment-end decoration-char text length)))

;; ---------------------------- Padded Divider ---------------------------------

(defun cj/--comment-padded-divider (cmt-start cmt-end decoration-char text length padding)
  "Internal implementation: Generate a padded divider comment.
CMT-START and CMT-END are the comment syntax strings.
DECORATION-CHAR is the character to use for the divider lines.
TEXT is the comment text.
LENGTH is the total width of each line.
PADDING is the number of spaces before the text."
  (when (< padding 0)
    (error "Padding %d cannot be negative" padding))
  (let* ((current-column-pos (current-column))
         (min-length (+ current-column-pos
                       (length cmt-start)
                       (if (equal cmt-start ";") 1 0)  ; doubled semicolon
                       1  ; space after comment-start
                       3  ; minimum decoration chars
                       (if (string-empty-p cmt-end) 0 (1+ (length cmt-end))))))
    (when (< length min-length)
      (error "Length %d is too small to generate comment (minimum %d)" length min-length))
    (let* ((available-width (- length current-column-pos
                              (length cmt-start)
                              (if (string-empty-p cmt-end) 0 (1+ (length cmt-end)))))
           (line (make-string available-width (string-to-char decoration-char))))
      ;; Top line
    (insert cmt-start)
    (when (equal cmt-start ";") (insert cmt-start))
    (insert " ")
    (insert line)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)

    ;; Text line with padding
    (dotimes (_ current-column-pos) (insert " "))
    (insert cmt-start)
    (when (equal cmt-start ";") (insert cmt-start))
    (insert " ")
    (dotimes (_ padding) (insert " "))
    (insert text)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)

    ;; Bottom line
    (dotimes (_ current-column-pos) (insert " "))
    (insert cmt-start)
    (when (equal cmt-start ";") (insert cmt-start))
    (insert " ")
    (insert line)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline))))

(defun cj/comment-padded-divider ()
  "Insert a padded divider comment banner.
Prompts for decoration character, text, padding, and length option."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start)
                           comment-start
                         (read-string "Comment start character(s): ")))
         (comment-end (if (and (boundp 'comment-end) comment-end)
                         comment-end
                       ""))
         (decoration-char (read-string "Decoration character (default =): " nil nil "="))
         (text (read-string "Comment text: "))
         (padding (string-to-number (read-string "Padding spaces (default 2): " nil nil "2")))
         (length-option (completing-read "Length: "
                                        '("fill-column" "half-column" "match-text")
                                        nil t nil nil "fill-column"))
         (length (cond
                  ((string= length-option "fill-column") fill-column)
                  ((string= length-option "half-column") (/ fill-column 2))
                  ((string= length-option "match-text")
                   (+ (length comment-start)
                      (if (equal comment-start ";") 1 0)
                      1  ; space after comment-start
                      padding
                      (length text)
                      (if (string-empty-p comment-end) 0 (1+ (length comment-end))))))))
    (cj/--comment-padded-divider comment-start comment-end decoration-char text length padding)))

;; -------------------------------- Comment Box --------------------------------

(defun cj/--comment-box (cmt-start cmt-end decoration-char text length)
  "Internal implementation: Generate a 3-line box comment with centered text.
CMT-START and CMT-END are the comment syntax strings.
DECORATION-CHAR is the character to use for borders.
TEXT is the comment text (centered).
LENGTH is the total width of each line."
  (let* ((current-column-pos (current-column))
         (comment-char (if (equal cmt-start ";") ";;" cmt-start))
         (comment-end-char (if (string-empty-p cmt-end) comment-char cmt-end))
         (min-length (+ current-column-pos
                       (length comment-char)
                       2  ; spaces around content
                       (length comment-end-char)
                       6)))  ; minimum: 3 border chars + text space + 3 border chars
    (when (< length min-length)
      (error "Length %d is too small to generate comment (minimum %d)" length min-length))
    (let* ((available-width (- length current-column-pos
                              (length comment-char)
                              (length comment-end-char)
                              2))  ; spaces around content
           (border-line (make-string available-width (string-to-char decoration-char)))
           (text-length (length text))
           ;; For text line: need space for decoration + space + text + space + decoration
           (text-available (- available-width 4))  ; 2 for side decorations, 2 for spaces
           (padding-each-side (max 1 (/ (- text-available text-length) 2)))
           (right-padding (if (= (% (- text-available text-length) 2) 0)
                             padding-each-side
                           (1+ padding-each-side))))
      ;; Top border
      (insert comment-char " " border-line " " comment-end-char)
      (newline)

      ;; Centered text line with side borders
      (dotimes (_ current-column-pos) (insert " "))
      (insert comment-char " " decoration-char " ")
      (dotimes (_ padding-each-side) (insert " "))
      (insert text)
      (dotimes (_ right-padding) (insert " "))
      (insert " " decoration-char " " comment-end-char)
      (newline)

      ;; Bottom border
      (dotimes (_ current-column-pos) (insert " "))
      (insert comment-char " " border-line " " comment-end-char)
      (newline))))

(defun cj/comment-box ()
  "Insert a 3-line comment box with centered text.
Prompts for decoration character, text, and uses `fill-column' for length."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start)
                           comment-start
                         (read-string "Comment start character(s): ")))
         (comment-end (if (and (boundp 'comment-end) comment-end)
                         comment-end
                       ""))
         (decoration-char (read-string "Decoration character (default -): " nil nil "-"))
         (text (capitalize (string-trim (read-from-minibuffer "Comment: "))))
         (length (min fill-column 80)))
    (cj/--comment-box comment-start comment-end decoration-char text length)))

;; ------------------------------ Heavy Box ------------------------------------

(defun cj/--comment-heavy-box (cmt-start cmt-end decoration-char text length)
  "Internal implementation: Generate a heavy box comment with blank lines.
CMT-START and CMT-END are the comment syntax strings.
DECORATION-CHAR is the character to use for borders.
TEXT is the comment text (centered).
LENGTH is the total width of each line."
  (let* ((current-column-pos (current-column))
         (comment-char (if (equal cmt-start ";") ";;" cmt-start))
         (comment-end-char (if (string-empty-p cmt-end) comment-char cmt-end))
         (available-width (- length current-column-pos
                            (length comment-char)
                            (length comment-end-char)
                            2))  ; spaces around content
         (border-line (make-string available-width (string-to-char decoration-char)))
         (text-length (length text))
         (padding-each-side (max 1 (/ (- available-width text-length) 2)))
         (right-padding (if (= (% (- available-width text-length) 2) 0)
                           padding-each-side
                         (1+ padding-each-side))))
    ;; Top border
    (insert comment-char " " border-line " " comment-end-char)
    (newline)

    ;; Empty line with side borders
    (dotimes (_ current-column-pos) (insert " "))
    (insert decoration-char)
    (dotimes (_ available-width) (insert " "))
    (insert " " decoration-char)
    (newline)

    ;; Centered text line
    (dotimes (_ current-column-pos) (insert " "))
    (insert decoration-char " ")
    (dotimes (_ padding-each-side) (insert " "))
    (insert text)
    (dotimes (_ right-padding) (insert " "))
    (insert " " decoration-char)
    (newline)

    ;; Empty line with side borders
    (dotimes (_ current-column-pos) (insert " "))
    (insert decoration-char)
    (dotimes (_ available-width) (insert " "))
    (insert " " decoration-char)
    (newline)

    ;; Bottom border
    (dotimes (_ current-column-pos) (insert " "))
    (insert comment-char " " border-line " " comment-end-char)
    (newline)))

(defun cj/comment-heavy-box ()
  "Insert a heavy box comment with blank lines around centered text.
Prompts for decoration character, text, and length option."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start)
                           comment-start
                         (read-string "Comment start character(s): ")))
         (comment-end (if (and (boundp 'comment-end) comment-end)
                         comment-end
                       ""))
         (decoration-char (read-string "Decoration character (default *): " nil nil "*"))
         (text (read-string "Comment text: "))
         (length-option (completing-read "Length: "
                                        '("fill-column" "half-column" "padded-text")
                                        nil t nil nil "fill-column"))
         (length (cond
                  ((string= length-option "fill-column") fill-column)
                  ((string= length-option "half-column") (/ fill-column 2))
                  ((string= length-option "padded-text")
                   (+ (current-column)
                      (length (if (equal comment-start ";") ";;" comment-start))
                      2  ; decoration char + space
                      4  ; minimum padding (2 on each side)
                      (length text)
                      (if (string-empty-p comment-end)
                          1  ; just the side decoration
                        (1+ (length comment-end))))))))
    (cj/--comment-heavy-box comment-start comment-end decoration-char text length)))

;; ---------------------------- Unicode Box ------------------------------------

(defun cj/--comment-unicode-box (cmt-start cmt-end text length box-style)
  "Internal implementation: Generate a unicode box comment.
CMT-START and CMT-END are the comment syntax strings.
TEXT is the comment text.
LENGTH is the total width of each line.
BOX-STYLE is either \\='single or \\='double for line style."
  (let* ((current-column-pos (current-column))
         (comment-char (if (equal cmt-start ";") ";;" cmt-start))
         (min-length (+ current-column-pos
                       (length comment-char)
                       1  ; space after comment-char
                       5  ; minimum: corner + corner + padding
                       (if (string-empty-p cmt-end) 0 (1+ (length cmt-end))))))
    (when (< length min-length)
      (error "Length %d is too small to generate comment (minimum %d)" length min-length))
    (let* ((available-width (- length current-column-pos
                              (length comment-char)
                              (if (string-empty-p cmt-end) 0 (1+ (length cmt-end)))
                              3))  ; box corners and padding
           (top-left (if (eq box-style 'double) "╔" "┌"))
         (top-right (if (eq box-style 'double) "╗" "┐"))
         (bottom-left (if (eq box-style 'double) "╚" "└"))
         (bottom-right (if (eq box-style 'double) "╝" "┘"))
         (horizontal (if (eq box-style 'double) "═" "─"))
         (vertical (if (eq box-style 'double) "║" "│"))
         (text-padding (- available-width (length text) 2)))
    ;; Top line
    (insert comment-char " " top-left)
    (dotimes (_ available-width) (insert horizontal))
    (insert top-right)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)

    ;; Text line
    (dotimes (_ current-column-pos) (insert " "))
    (insert comment-char " " vertical " " text)
    (dotimes (_ text-padding) (insert " "))
    (insert " " vertical)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline)

    ;; Bottom line
    (dotimes (_ current-column-pos) (insert " "))
    (insert comment-char " " bottom-left)
    (dotimes (_ available-width) (insert horizontal))
    (insert bottom-right)
    (when (not (string-empty-p cmt-end))
      (insert " " cmt-end))
    (newline))))

(defun cj/comment-unicode-box ()
  "Insert a unicode box comment.
Prompts for text, box style, and length option."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start)
                           comment-start
                         (read-string "Comment start character(s): ")))
         (comment-end (if (and (boundp 'comment-end) comment-end)
                         comment-end
                       ""))
         (text (read-string "Comment text: "))
         (box-style (intern (completing-read "Box style: "
                                             '("single" "double")
                                             nil t nil nil "single")))
         (length-option (completing-read "Length: "
                                        '("fill-column" "half-column" "padded-text")
                                        nil t nil nil "fill-column"))
         (length (cond
                  ((string= length-option "fill-column") fill-column)
                  ((string= length-option "half-column") (/ fill-column 2))
                  ((string= length-option "padded-text")
                   (+ (current-column)
                      (length (if (equal comment-start ";") ";;" comment-start))
                      5  ; box chars and spaces
                      (length text)
                      (if (string-empty-p comment-end) 0 (1+ (length comment-end))))))))
    (cj/--comment-unicode-box comment-start comment-end text length box-style)))

;; ---------------------------- Block Banner -----------------------------------

(defun cj/--comment-block-banner (cmt-start cmt-end decoration-char text length)
  "Internal implementation: Generate a block banner comment (JSDoc/Doxygen style).
CMT-START should be the block comment start (e.g., '/*').
CMT-END should be the block comment end (e.g., '*/').
DECORATION-CHAR is the character to use for the border line.
TEXT is the comment text.
LENGTH is the total width of each line."
  (let* ((current-column-pos (current-column))
         (min-length (+ current-column-pos
                       (length cmt-start)
                       3)))  ; minimum: 3 decoration chars
    (when (< length min-length)
      (error "Length %d is too small to generate comment (minimum %d)" length min-length))
    (let* ((available-width (- length current-column-pos (length cmt-start) 1))
           (border-line (make-string available-width (string-to-char decoration-char))))
    ;; Top line
    (insert cmt-start border-line)
    (newline)

    ;; Text line
    (dotimes (_ current-column-pos) (insert " "))
    (insert " " decoration-char " " text)
    (newline)

    ;; Bottom line
    (dotimes (_ current-column-pos) (insert " "))
    (insert " ")
    (dotimes (_ (- available-width (length cmt-end)))
      (insert decoration-char))
    (insert cmt-end)
    (newline))))

(defun cj/comment-block-banner ()
  "Insert a block banner comment (JSDoc/Doxygen style).
Prompts for decoration character, text, and length option."
  (interactive)
  (let* ((comment-start (if (and (boundp 'comment-start) comment-start
                                (string-match-p "/\\*" comment-start))
                           comment-start
                         (read-string "Block comment start (e.g., /*): " nil nil "/*")))
         (comment-end (if (and (boundp 'comment-end) comment-end
                              (not (string-empty-p comment-end)))
                         comment-end
                       (read-string "Block comment end (e.g., */): " nil nil "*/")))
         (decoration-char (read-string "Decoration character (default *): " nil nil "*"))
         (text (read-string "Comment text: "))
         (length-option (completing-read "Length: "
                                        '("fill-column" "half-column" "match-text")
                                        nil t nil nil "fill-column"))
         (length (cond
                  ((string= length-option "fill-column") fill-column)
                  ((string= length-option "half-column") (/ fill-column 2))
                  ((string= length-option "match-text")
                   (+ (current-column)
                      (length comment-start)
                      2  ; space + decoration
                      (length text))))))
    (cj/--comment-block-banner comment-start comment-end decoration-char text length)))

;; ------------------------------- Comment Hyphen ------------------------------

(defun cj/comment-hyphen()
  "Insert a centered comment with `-' (hyphens) on each side.
Leverages cj/comment-inline-border."
  (interactive)
  (cj/comment-inline-border "-"))

;; ------------------------------- Comment Keymap ------------------------------

(defvar-keymap cj/comment-map
  :doc "Keymap for code comment operations"
  "r" #'cj/comment-reformat
  "d" #'cj/delete-buffer-comments
  "c" #'cj/comment-inline-border
  "-" #'cj/comment-hyphen
  "s" #'cj/comment-simple-divider
  "p" #'cj/comment-padded-divider
  "b" #'cj/comment-box
  "h" #'cj/comment-heavy-box
  "u" #'cj/comment-unicode-box
  "n" #'cj/comment-block-banner)
(keymap-set cj/custom-keymap "C" cj/comment-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; C" "code comment menu"
    "C-; C r" "reformat comment"
    "C-; C d" "delete comments"
    "C-; C c" "inline border"
    "C-; C -" "hyphen divider"
    "C-; C s" "simple divider"
    "C-; C p" "padded divider"
    "C-; C b" "box"
    "C-; C h" "heavy box"
    "C-; C u" "unicode box"
    "C-; C n" "block banner"))

(provide 'custom-comments)
;;; custom-comments.el ends here.
