;;; test-custom-ordering-wrappers.el --- Tests for the user-facing ordering wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover each `cj/--<op>' internal in isolation.  This
;; file covers the buffer-mutating wrapper commands:
;;
;;   cj/arrayify, cj/listify, cj/arrayify-json, cj/arrayify-python
;;   cj/unarrayify
;;   cj/toggle-quotes
;;   cj/reverse-lines
;;   cj/number-lines
;;   cj/alphabetize-region
;;   cj/comma-separated-text-to-lines
;;
;; Each test runs the wrapper inside `with-temp-buffer' with the whole
;; buffer as the region and asserts the contents after replacement.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-ordering)

(defmacro test-custom-ordering--with (input &rest body)
  "Insert INPUT into a temp buffer, evaluate BODY, return buffer string."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,input)
     (set-mark (point-min))
     (goto-char (point-max))
     ,@body
     (buffer-string)))

;;; cj/arrayify (quoted, comma-separated)

(ert-deftest test-custom-ordering-arrayify-replaces-region-with-quoted-list ()
  "Normal: arrayify quotes every word and joins with \", \"."
  (let ((result (test-custom-ordering--with "apple banana cherry"
                  (cj/arrayify (point-min) (point-max) "'"))))
    (should (equal result "'apple', 'banana', 'cherry'"))))

;;; cj/listify (unquoted)

(ert-deftest test-custom-ordering-listify-joins-with-comma-space ()
  "Normal: listify produces a bare comma-separated list."
  (let ((result (test-custom-ordering--with "apple banana cherry"
                  (cj/listify (point-min) (point-max)))))
    (should (equal result "apple, banana, cherry"))))

;;; cj/arrayify-json

(ert-deftest test-custom-ordering-arrayify-json-wraps-in-brackets ()
  "Normal: arrayify-json wraps the quoted list in square brackets."
  (let ((result (test-custom-ordering--with "apple banana"
                  (cj/arrayify-json (point-min) (point-max)))))
    (should (equal result "[\"apple\", \"banana\"]"))))

;;; cj/arrayify-python

(ert-deftest test-custom-ordering-arrayify-python-wraps-in-brackets ()
  "Normal: arrayify-python wraps the quoted list in square brackets."
  (let ((result (test-custom-ordering--with "apple banana"
                  (cj/arrayify-python (point-min) (point-max)))))
    (should (equal result "[\"apple\", \"banana\"]"))))

;;; cj/unarrayify

(ert-deftest test-custom-ordering-unarrayify-splits-on-comma-space ()
  "Normal: unarrayify splits on \", \" and strips quotes."
  (let ((result (test-custom-ordering--with "'a', 'b', 'c'"
                  (cj/unarrayify (point-min) (point-max)))))
    (should (equal result "a\nb\nc"))))

;;; cj/toggle-quotes

(ert-deftest test-custom-ordering-toggle-quotes-swaps-quote-styles ()
  "Normal: toggle-quotes swaps single and double quote characters."
  (let ((result (test-custom-ordering--with "say \"hi\" and 'bye'"
                  (cj/toggle-quotes (point-min) (point-max)))))
    (should (equal result "say 'hi' and \"bye\""))))

;;; cj/reverse-lines

(ert-deftest test-custom-ordering-reverse-lines-inverts-line-order ()
  "Normal: reverse-lines flips top-to-bottom into bottom-to-top."
  (let ((result (test-custom-ordering--with "one\ntwo\nthree"
                  (cj/reverse-lines (point-min) (point-max)))))
    (should (equal result "three\ntwo\none"))))

;;; cj/number-lines

(ert-deftest test-custom-ordering-number-lines-prefixes-with-format ()
  "Normal: number-lines prefixes each line per the format string."
  (let ((result (test-custom-ordering--with "alpha\nbeta\ngamma"
                  (cj/number-lines (point-min) (point-max) "N. " nil))))
    (should (equal result "1. alpha\n2. beta\n3. gamma"))))

;;; cj/alphabetize-region

(ert-deftest test-custom-ordering-alphabetize-sorts-words ()
  "Normal: alphabetize-region sorts and joins with \", \"."
  (let ((result
         (with-temp-buffer
           (insert "cherry apple banana")
           (transient-mark-mode 1)
           (set-mark (point-min))
           (goto-char (point-max))
           (activate-mark)
           (cj/alphabetize-region)
           (buffer-string))))
    (should (equal result "apple, banana, cherry"))))

(ert-deftest test-custom-ordering-alphabetize-without-region-errors ()
  "Error: alphabetize-region without an active region signals."
  (with-temp-buffer
    (insert "anything")
    (deactivate-mark)
    (should-error (cj/alphabetize-region) :type 'user-error)))

;;; cj/comma-separated-text-to-lines

(ert-deftest test-custom-ordering-comma-to-lines-replaces-region ()
  "Normal: comma-separated-text-to-lines splits commas into newlines."
  (let ((result
         (with-temp-buffer
           (insert "a,b,c")
           (transient-mark-mode 1)
           (set-mark (point-min))
           (goto-char (point-max))
           (activate-mark)
           (cj/comma-separated-text-to-lines)
           (buffer-string))))
    (should (equal result "a\nb\nc"))))

(ert-deftest test-custom-ordering-comma-to-lines-without-region-errors ()
  "Error: comma-separated-text-to-lines without a region signals."
  (with-temp-buffer
    (insert "a,b,c")
    (deactivate-mark)
    (should-error (cj/comma-separated-text-to-lines))))

(provide 'test-custom-ordering-wrappers)
;;; test-custom-ordering-wrappers.el ends here
