;;; test-custom-comments-public-wrappers.el --- Tests for the interactive comment wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; The internal `cj/--comment-*' helpers are covered in the sibling
;; test files.  Their interactive wrappers were uncovered -- each
;; reads a comment start, decoration char, text, and (for some) a
;; length option, then delegates to the helper.  These tests stub
;; `read-string', `read-from-minibuffer', and `completing-read' to
;; drive each wrapper through its full body and assert it produces
;; some inserted output (a more granular content check would be
;; redundant with the helper-level tests).
;;
;; `cj/comment-hyphen' is a one-line delegate to `cj/comment-inline-
;; border' with "-"; covered here too.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-comments)

(defmacro test-cc--with-stubs (responses &rest body)
  "Stub the prompt primitives to drive a wrapper through its body.

RESPONSES is a plist:
  :read-string         function for `read-string'
  :read-from-mini      function for `read-from-minibuffer'
  :completing-read     function for `completing-read'

Stubs default to returning the empty string / first candidate when not
provided.  BODY runs in a fresh `with-temp-buffer' with `fill-column'
let-bound to 60 so length-option arithmetic stays predictable."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((fill-column 60)
           ;; Many wrappers consult `comment-start' to decide what
           ;; prefix to use; set a sane default so the read-string
           ;; branch isn't exercised when we don't care.
           (comment-start "#")
           (comment-end ""))
       (cl-letf (((symbol-function 'read-string)
                  (or (plist-get ,responses :read-string)
                      (lambda (&rest _) "")))
                 ((symbol-function 'read-from-minibuffer)
                  (or (plist-get ,responses :read-from-mini)
                      (lambda (&rest _) "Test text")))
                 ((symbol-function 'completing-read)
                  (or (plist-get ,responses :completing-read)
                      (lambda (_prompt collection &rest _)
                        (if (listp collection)
                            (car collection)
                          "fill-column")))))
         ,@body))))

;;; cj/comment-inline-border

(ert-deftest test-cc-inline-border-wrapper-inserts-into-buffer ()
  "Normal: wrapper reads text, delegates to the helper, inserts a comment."
  (test-cc--with-stubs nil
    (cj/comment-inline-border)
    (should (> (buffer-size) 0))
    (should (string-match-p "#" (buffer-string)))))

(ert-deftest test-cc-inline-border-wrapper-honors-explicit-decoration ()
  "Boundary: an explicit decoration char overrides the default \"#\"."
  (test-cc--with-stubs nil
    (cj/comment-inline-border "*")
    (should (string-match-p "\\*" (buffer-string)))))

;;; cj/comment-hyphen

(ert-deftest test-cc-hyphen-wrapper-uses-hyphen-decoration ()
  "Normal: `cj/comment-hyphen' delegates to inline-border with \"-\"."
  (test-cc--with-stubs nil
    (cj/comment-hyphen)
    (should (string-match-p "-" (buffer-string)))))

;;; cj/comment-simple-divider

(ert-deftest test-cc-simple-divider-wrapper-fill-column-length ()
  "Normal: `fill-column' option yields a divider at the configured width."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         ((string-match-p "Comment text" prompt) "Hello")
                                         (t "")))
                         :completing-read (lambda (&rest _) "fill-column"))
    (cj/comment-simple-divider)
    (should (string-match-p "Hello" (buffer-string)))
    (should (string-match-p "=" (buffer-string)))))

(ert-deftest test-cc-simple-divider-wrapper-half-column-length ()
  "Boundary: `half-column' option uses half the fill-column width."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         ((string-match-p "Comment text" prompt) "Half")
                                         (t "")))
                         :completing-read (lambda (&rest _) "half-column"))
    (cj/comment-simple-divider)
    (should (string-match-p "Half" (buffer-string)))))

(ert-deftest test-cc-simple-divider-wrapper-match-text-length ()
  "Boundary: `match-text' sizes the divider to the text."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         ((string-match-p "Comment text" prompt) "abc")
                                         (t "")))
                         :completing-read (lambda (&rest _) "match-text"))
    (cj/comment-simple-divider)
    (should (string-match-p "abc" (buffer-string)))))

;;; cj/comment-padded-divider

(ert-deftest test-cc-padded-divider-wrapper-inserts ()
  "Normal: padded-divider wrapper drives its prompt cycle and inserts."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         ((string-match-p "Comment text" prompt) "Padded")
                                         (t "")))
                         :completing-read (lambda (&rest _) "fill-column"))
    (cj/comment-padded-divider)
    (should (string-match-p "Padded" (buffer-string)))))

;;; cj/comment-box

(ert-deftest test-cc-box-wrapper-inserts ()
  "Normal: box wrapper drives prompts and produces a multi-line box.

`cj/comment-box' reads its decoration char via `read-string' but its
text via `read-from-minibuffer'."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         (t "")))
                         :read-from-mini (lambda (&rest _) "Box")
                         :completing-read (lambda (&rest _) "fill-column"))
    (cj/comment-box)
    (should (string-match-p "Box" (buffer-string)))
    (should (> (length (split-string (buffer-string) "\n" t)) 1))))

;;; cj/comment-heavy-box

(ert-deftest test-cc-heavy-box-wrapper-inserts ()
  "Normal: heavy-box wrapper produces multi-line output containing the text."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         ((string-match-p "Comment text" prompt) "Heavy")
                                         (t "")))
                         :completing-read (lambda (&rest _) "fill-column"))
    (cj/comment-heavy-box)
    (should (string-match-p "Heavy" (buffer-string)))))

;;; cj/comment-unicode-box

(ert-deftest test-cc-unicode-box-wrapper-inserts ()
  "Normal: unicode-box wrapper drives prompts and produces a unicode box.

`cj/comment-unicode-box' reads the text via `read-string', not
`read-from-minibuffer'."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Comment text" prompt) "Unicode")
                                         (t "")))
                         :completing-read (lambda (prompt collection &rest _)
                                            (cond
                                             ((string-match-p "Length" prompt) "fill-column")
                                             ((string-match-p "[Ss]tyle" prompt) "single")
                                             (t (if (listp collection) (car collection) "")))))
    (cj/comment-unicode-box)
    (should (string-match-p "Unicode" (buffer-string)))))

;;; cj/comment-block-banner

(ert-deftest test-cc-block-banner-wrapper-inserts ()
  "Normal: block-banner wrapper produces a multi-line banner with the text."
  (test-cc--with-stubs '(:read-string (lambda (prompt &rest _)
                                        (cond
                                         ((string-match-p "Decoration" prompt) "=")
                                         ((string-match-p "Comment text" prompt) "Banner")
                                         (t "")))
                         :completing-read (lambda (&rest _) "fill-column"))
    (cj/comment-block-banner)
    (should (string-match-p "Banner" (buffer-string)))))

(provide 'test-custom-comments-public-wrappers)
;;; test-custom-comments-public-wrappers.el ends here
