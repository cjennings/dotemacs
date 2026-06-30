;;; test-nov-reading--text-scale.el --- nov reading text-scale persistence tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the persisted global reading text-scale offset: parsing the stored
;; value (pure) and the save/load round-trip through the data file.  The live
;; text-scale application in the +/-/= commands is exercised live, not here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'nov-reading)

(declare-function cj/nov-reading--parse-text-scale "nov-reading" (s))
(declare-function cj/nov-reading--load-text-scale "nov-reading" ())
(declare-function cj/nov-reading--save-text-scale "nov-reading" (amount))
(defvar cj/nov-reading-text-scale-file)

;;; --------------------- cj/nov-reading--parse-text-scale ----------------------

(ert-deftest test-nov-reading-parse-text-scale-positive ()
  "Normal: a positive integer string parses to that integer."
  (should (= (cj/nov-reading--parse-text-scale "3") 3)))

(ert-deftest test-nov-reading-parse-text-scale-negative ()
  "Normal: a negative integer string parses to that integer."
  (should (= (cj/nov-reading--parse-text-scale "-2") -2)))

(ert-deftest test-nov-reading-parse-text-scale-trailing-newline ()
  "Boundary: surrounding whitespace/newline is tolerated."
  (should (= (cj/nov-reading--parse-text-scale "4\n") 4)))

(ert-deftest test-nov-reading-parse-text-scale-zero ()
  "Boundary: \"0\" parses to 0."
  (should (= (cj/nov-reading--parse-text-scale "0") 0)))

(ert-deftest test-nov-reading-parse-text-scale-nil ()
  "Boundary: nil parses to 0."
  (should (= (cj/nov-reading--parse-text-scale nil) 0)))

(ert-deftest test-nov-reading-parse-text-scale-empty ()
  "Boundary: an empty string parses to 0."
  (should (= (cj/nov-reading--parse-text-scale "") 0)))

(ert-deftest test-nov-reading-parse-text-scale-garbage ()
  "Error: non-numeric content parses to 0."
  (should (= (cj/nov-reading--parse-text-scale "garbage") 0)))

(ert-deftest test-nov-reading-parse-text-scale-float-rejected ()
  "Error: a non-integer numeric string parses to 0 (offsets are integers)."
  (should (= (cj/nov-reading--parse-text-scale "3.5") 0)))

;;; ------------------ cj/nov-reading--save/load round-trip ---------------------

(ert-deftest test-nov-reading-save-load-roundtrip-positive ()
  "Normal: a saved positive offset loads back unchanged."
  (let ((cj/nov-reading-text-scale-file (make-temp-file "nov-scale-")))
    (unwind-protect
        (progn
          (cj/nov-reading--save-text-scale 4)
          (should (= (cj/nov-reading--load-text-scale) 4)))
      (delete-file cj/nov-reading-text-scale-file))))

(ert-deftest test-nov-reading-save-load-roundtrip-negative ()
  "Normal: a saved negative offset loads back unchanged."
  (let ((cj/nov-reading-text-scale-file (make-temp-file "nov-scale-")))
    (unwind-protect
        (progn
          (cj/nov-reading--save-text-scale -3)
          (should (= (cj/nov-reading--load-text-scale) -3)))
      (delete-file cj/nov-reading-text-scale-file))))

(ert-deftest test-nov-reading-save-load-roundtrip-zero ()
  "Boundary: a saved 0 offset loads back as 0."
  (let ((cj/nov-reading-text-scale-file (make-temp-file "nov-scale-")))
    (unwind-protect
        (progn
          (cj/nov-reading--save-text-scale 0)
          (should (= (cj/nov-reading--load-text-scale) 0)))
      (delete-file cj/nov-reading-text-scale-file))))

(ert-deftest test-nov-reading-load-missing-file-defaults-zero ()
  "Boundary: loading when no file exists yet returns 0."
  (let ((cj/nov-reading-text-scale-file
         (expand-file-name "nov-scale-absent"
                           (make-temp-file "nov-scale-dir-" t))))
    (unwind-protect
        (should (= (cj/nov-reading--load-text-scale) 0))
      (delete-directory (file-name-directory cj/nov-reading-text-scale-file) t))))

(ert-deftest test-nov-reading-save-creates-missing-directory ()
  "Boundary: save creates the data directory when it is absent."
  (let* ((dir (make-temp-file "nov-scale-dir-" t))
         (cj/nov-reading-text-scale-file
          (expand-file-name "sub/nov-reading-text-scale" dir)))
    (unwind-protect
        (progn
          (cj/nov-reading--save-text-scale 2)
          (should (file-readable-p cj/nov-reading-text-scale-file))
          (should (= (cj/nov-reading--load-text-scale) 2)))
      (delete-directory dir t))))

(provide 'test-nov-reading--text-scale)
;;; test-nov-reading--text-scale.el ends here
