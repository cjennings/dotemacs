;;; test-custom-datetime-all-methods.el --- Tests for custom-datetime.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for all six insert functions in custom-datetime.el.
;;
;; All functions follow the same pattern: (insert (format-time-string FMT (current-time)))
;; They are thin wrappers, so tests focus on:
;; - Each function inserts text matching its format variable
;; - Custom format variables are respected
;; - Trailing space convention is preserved where present
;;
;; We mock current-time to a fixed value for deterministic output.

;;; Code:

(require 'ert)

(unless (boundp 'cj/custom-keymap)
  (defvar cj/custom-keymap (make-sparse-keymap)))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-datetime)

;; Fixed time: 2026-02-15 14:30:45 CST (UTC-6)
(defconst test-datetime--fixed-time (encode-time 45 30 14 15 2 2026)
  "Fixed time value for deterministic tests.")

(defmacro test-datetime--with-fixed-time (&rest body)
  "Execute BODY with current-time mocked to test-datetime--fixed-time."
  `(cl-letf (((symbol-function 'current-time)
              (lambda () test-datetime--fixed-time)))
     ,@body))

;; Helper: call FUNC in a temp buffer with mocked time and return inserted text
(defun test-datetime--run (func)
  "Call FUNC with mocked time and return the buffer contents."
  (with-temp-buffer
    (test-datetime--with-fixed-time
     (funcall func))
    (buffer-string)))

;;; Normal Cases — Each function inserts expected format

(ert-deftest test-custom-datetime-all-methods-normal-readable-date-time ()
  "cj/insert-readable-date-time should insert human-readable date and time."
  (let ((result (test-datetime--run #'cj/insert-readable-date-time)))
    (should (string-match-p "Sunday, February 15, 2026" result))
    (should (string-match-p "02:30:45 PM" result))))

(ert-deftest test-custom-datetime-all-methods-normal-sortable-date-time ()
  "cj/insert-sortable-date-time should insert ISO-style date and time."
  (let ((result (test-datetime--run #'cj/insert-sortable-date-time)))
    (should (string-match-p "2026-02-15" result))
    (should (string-match-p "14:30:45" result))))

(ert-deftest test-custom-datetime-all-methods-normal-sortable-time ()
  "cj/insert-sortable-time should insert time with AM/PM and timezone."
  (let ((result (test-datetime--run #'cj/insert-sortable-time)))
    (should (string-match-p "02:30:45 PM" result))))

(ert-deftest test-custom-datetime-all-methods-normal-readable-time ()
  "cj/insert-readable-time should insert short time with AM/PM."
  (let ((result (test-datetime--run #'cj/insert-readable-time)))
    (should (string-match-p "2:30 PM" result))))

(ert-deftest test-custom-datetime-all-methods-normal-sortable-date ()
  "cj/insert-sortable-date should insert ISO date with day abbreviation."
  (let ((result (test-datetime--run #'cj/insert-sortable-date)))
    (should (string-match-p "2026-02-15 Sun" result))))

(ert-deftest test-custom-datetime-all-methods-normal-readable-date ()
  "cj/insert-readable-date should insert full human-readable date."
  (let ((result (test-datetime--run #'cj/insert-readable-date)))
    (should (equal result "Sunday, February 15, 2026"))))

;;; Boundary Cases

(ert-deftest test-custom-datetime-all-methods-boundary-trailing-space-convention ()
  "Functions with trailing space in format should include it in output."
  ;; These formats have trailing spaces by default
  (let ((result-rdt (test-datetime--run #'cj/insert-readable-date-time))
        (result-sdt (test-datetime--run #'cj/insert-sortable-date-time))
        (result-st (test-datetime--run #'cj/insert-sortable-time))
        (result-rt (test-datetime--run #'cj/insert-readable-time)))
    (should (string-suffix-p " " result-rdt))
    (should (string-suffix-p " " result-sdt))
    (should (string-suffix-p " " result-st))
    (should (string-suffix-p " " result-rt))))

(ert-deftest test-custom-datetime-all-methods-boundary-no-trailing-space ()
  "Functions without trailing space in format should not add one."
  (let ((result-sd (test-datetime--run #'cj/insert-sortable-date))
        (result-rd (test-datetime--run #'cj/insert-readable-date)))
    (should-not (string-suffix-p " " result-sd))
    (should-not (string-suffix-p " " result-rd))))

(ert-deftest test-custom-datetime-all-methods-boundary-custom-format-override ()
  "Overriding a format variable should change the output."
  (let ((readable-date-format "%d/%m/%Y"))
    (should (equal (test-datetime--run #'cj/insert-readable-date)
                   "15/02/2026"))))

(ert-deftest test-custom-datetime-all-methods-boundary-inserts-at-point ()
  "Inserted text should appear at point, not replacing buffer contents."
  (with-temp-buffer
    (insert "before ")
    (test-datetime--with-fixed-time
     (cj/insert-sortable-date))
    (should (string-prefix-p "before 2026-02-15" (buffer-string)))))

(provide 'test-custom-datetime-all-methods)
;;; test-custom-datetime-all-methods.el ends here
