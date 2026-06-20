;;; test-jumper--location-candidates.el --- Tests for jumper--location-candidates -*- lexical-binding: t; -*-

;;; Commentary:
;; jumper--location-candidates is the (display . index) builder extracted from
;; the verbatim cl-loop in jumper-jump-to-location and jumper-remove-location.
;; It composes jumper--format-location (which now goes through the extracted
;; jumper--with-marker-at).  The wrappers cover it transitively; this exercises
;; it directly against stored locations.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'jumper)

(ert-deftest test-jumper-location-candidates-one-pair-per-stored-location ()
  "Normal: one (display . index) pair per stored location, indices in order."
  (let ((saved-regs jumper--registers)
        (saved-idx jumper--next-index))
    (unwind-protect
        (progn
          (setq jumper--registers (make-vector jumper-max-locations nil)
                jumper--next-index 0)
          (with-temp-buffer
            (insert "line one\nline two\nline three\n")
            (goto-char (point-min))
            (should (integerp (jumper--do-store-location)))   ; index 0
            (forward-line 2)
            (should (integerp (jumper--do-store-location)))   ; index 1
            (let ((cands (jumper--location-candidates)))
              (should (= (length cands) 2))
              (should (equal (mapcar #'cdr cands) '(0 1)))
              (should (stringp (car (nth 0 cands))))
              (should (stringp (car (nth 1 cands)))))))
      (setq jumper--registers saved-regs
            jumper--next-index saved-idx))))

(ert-deftest test-jumper-location-candidates-empty-when-none-stored ()
  "Boundary: no stored locations yields an empty candidate list."
  (let ((saved-regs jumper--registers)
        (saved-idx jumper--next-index))
    (unwind-protect
        (progn
          (setq jumper--registers (make-vector jumper-max-locations nil)
                jumper--next-index 0)
          (should (null (jumper--location-candidates))))
      (setq jumper--registers saved-regs
            jumper--next-index saved-idx))))

(provide 'test-jumper--location-candidates)
;;; test-jumper--location-candidates.el ends here
