;;; test-calendar-sync--add-months.el --- Tests for month arithmetic -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--add-months. Pure date arithmetic.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--add-months-normal-forward ()
  "Adding 3 months advances correctly."
  (should (equal '(2026 6 15) (calendar-sync--add-months '(2026 3 15) 3))))

(ert-deftest test-calendar-sync--add-months-normal-backward ()
  "Subtracting months goes backward."
  (should (equal '(2025 12 15) (calendar-sync--add-months '(2026 3 15) -3))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--add-months-boundary-year-rollover ()
  "Adding months past December rolls into next year."
  (should (equal '(2027 2 1) (calendar-sync--add-months '(2026 11 1) 3))))

(ert-deftest test-calendar-sync--add-months-boundary-year-rollback ()
  "Subtracting months past January rolls into previous year."
  (should (equal '(2025 11 1) (calendar-sync--add-months '(2026 2 1) -3))))

(ert-deftest test-calendar-sync--add-months-boundary-zero ()
  "Adding zero months returns same date."
  (should (equal '(2026 3 15) (calendar-sync--add-months '(2026 3 15) 0))))

;;; Error Cases

(ert-deftest test-calendar-sync--add-months-error-large-offset ()
  "Adding 24 months (2 years) works correctly."
  (should (equal '(2028 3 15) (calendar-sync--add-months '(2026 3 15) 24))))

(provide 'test-calendar-sync--add-months)
;;; test-calendar-sync--add-months.el ends here
