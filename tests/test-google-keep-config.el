;;; test-google-keep-config.el --- Tests for google-keep-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the pure JSON-to-org core of google-keep-config.el (the part that
;; later extracts to a package) plus the parse-render-write chain.  The bridge
;; subprocess + auth are the IO boundary, exercised live once the token is set.

;;; Code:

(require 'ert)
(require 'google-keep-config)

(defun test-google-keep--note (&rest overrides)
  "Build a note alist (parse-shaped) with OVERRIDES merged in."
  (let ((base (list (cons 'id "abc")
                    (cons 'title "Groceries")
                    (cons 'text "milk\neggs")
                    (cons 'labels '("shopping" "home"))
                    (cons 'pinned nil)
                    (cons 'archived nil)
                    (cons 'color "WHITE")
                    (cons 'updated "2026-06-25T04:00:00Z"))))
    (dolist (pair overrides base)
      (setf (alist-get (car pair) base) (cdr pair)))))

;;; cj/keep--parse-json

(ert-deftest test-google-keep-parse-json-array ()
  "Normal: a JSON array parses to a list of note alists."
  (let ((notes (cj/keep--parse-json
                "[{\"id\":\"a\",\"title\":\"T\",\"labels\":[\"x\"],\"pinned\":true}]")))
    (should (= 1 (length notes)))
    (should (equal "a" (alist-get 'id (car notes))))
    (should (equal '("x") (alist-get 'labels (car notes))))
    (should (eq t (alist-get 'pinned (car notes))))))

(ert-deftest test-google-keep-parse-json-empty ()
  "Boundary: an empty Keep ([]) parses to an empty list."
  (should (null (cj/keep--parse-json "[]"))))

;;; cj/keep--label-to-tag

(ert-deftest test-google-keep-label-to-tag-plain ()
  "Normal: an alphanumeric label is unchanged."
  (should (equal "shopping" (cj/keep--label-to-tag "shopping"))))

(ert-deftest test-google-keep-label-to-tag-sanitizes ()
  "Boundary: spaces and punctuation become underscores (valid org tag chars)."
  (should (equal "to_do_list_" (cj/keep--label-to-tag "to do/list!"))))

;;; cj/keep--note-tags

(ert-deftest test-google-keep-note-tags-labels ()
  "Normal: labels render as a trailing org-tag string."
  (should (equal " :shopping:home:" (cj/keep--note-tags (test-google-keep--note)))))

(ert-deftest test-google-keep-note-tags-archived ()
  "Normal: an archived note gains the archived tag."
  (should (equal " :shopping:home:archived:"
                 (cj/keep--note-tags (test-google-keep--note (cons 'archived t))))))

(ert-deftest test-google-keep-note-tags-none ()
  "Boundary: no labels and not archived yields an empty tag string."
  (should (equal "" (cj/keep--note-tags
                     (test-google-keep--note (cons 'labels nil))))))

;;; cj/keep--note-heading

(ert-deftest test-google-keep-note-heading-full ()
  "Normal: a full note renders heading, properties, link, and body."
  (let ((s (cj/keep--note-heading (test-google-keep--note))))
    (should (string-match-p "\\`\\* Groceries :shopping:home:\n" s))
    (should (string-match-p ":KEEP_ID: abc\n" s))
    (should (string-match-p ":UPDATED: 2026-06-25T04:00:00Z\n" s))
    (should (string-match-p "\\[\\[https://keep.google.com/#NOTE/abc\\]\\[open in Keep\\]\\]" s))
    (should (string-match-p "milk\neggs\n" s))))

(ert-deftest test-google-keep-note-heading-untitled ()
  "Boundary: an empty title falls back to (untitled)."
  (let ((s (cj/keep--note-heading (test-google-keep--note (cons 'title "")))))
    (should (string-match-p "\\`\\* (untitled)" s))))

(ert-deftest test-google-keep-note-heading-empty-text ()
  "Boundary: an empty body emits no trailing text block."
  (let ((s (cj/keep--note-heading
            (test-google-keep--note (cons 'text "") (cons 'labels nil)))))
    (should-not (string-match-p "open in Keep\\]\\]\n.+[^\n]" s))))

;;; cj/keep--sort-pinned-first

(ert-deftest test-google-keep-sort-pinned-first ()
  "Normal: pinned notes come first, order otherwise preserved."
  (let* ((a (test-google-keep--note (cons 'id "a") (cons 'pinned nil)))
         (b (test-google-keep--note (cons 'id "b") (cons 'pinned t)))
         (c (test-google-keep--note (cons 'id "c") (cons 'pinned nil)))
         (sorted (cj/keep--sort-pinned-first (list a b c))))
    (should (equal '("b" "a" "c") (mapcar (lambda (n) (alist-get 'id n)) sorted)))))

;;; cj/keep--render

(ert-deftest test-google-keep-render-header-and-notes ()
  "Normal: the page carries the read-only header and a heading per note."
  (let ((s (cj/keep--render (list (test-google-keep--note)) "2026-06-25 04:00")))
    (should (string-match-p "read-only view" s))
    (should (string-match-p "Last refresh: 2026-06-25 04:00" s))
    (should (string-match-p "^\\* Groceries" s))))

(ert-deftest test-google-keep-render-empty ()
  "Boundary: no notes still produces a valid header-only page."
  (let ((s (cj/keep--render nil)))
    (should (string-match-p "#\\+TITLE: Google Keep" s))
    (should-not (string-match-p "^\\* " s))))

;;; cj/keep--write-atomically + the parse-render-write chain

(ert-deftest test-google-keep-write-atomically ()
  "Normal: content lands in the target file via temp + rename."
  (let* ((dir (make-temp-file "keep-test-" t))
         (file (expand-file-name "keep.org" dir)))
    (unwind-protect
        (progn
          (cj/keep--write-atomically "hello\n" file)
          (should (equal "hello\n"
                         (with-temp-buffer (insert-file-contents file)
                                           (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest test-google-keep-write-notes-chain ()
  "Normal: JSON in, a rendered org file out, with the note count returned."
  (let* ((dir (make-temp-file "keep-test-" t))
         (keep-file (expand-file-name "keep.org" dir)))
    (unwind-protect
        (let ((n (cj/keep--write-notes
                  "[{\"id\":\"a\",\"title\":\"One\",\"labels\":[],\"pinned\":false,\"archived\":false,\"color\":\"WHITE\",\"updated\":\"2026-06-25T04:00:00Z\"}]")))
          (should (= 1 n))
          (should (string-match-p "^\\* One"
                                  (with-temp-buffer (insert-file-contents keep-file)
                                                    (buffer-string)))))
      (delete-directory dir t))))

(provide 'test-google-keep-config)
;;; test-google-keep-config.el ends here
