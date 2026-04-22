;;; test-host-environment.el --- Tests for host-environment.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `env-laptop-p' and its pure helpers.
;;
;; Regression driver: upower on a desktop with only an AC adapter reports
;; `(battery-format "%B" ...)' as "unknown", not "N/A".  The previous
;; implementation treated any non-"N/A" value as "has a battery", so
;; desktops were misclassified as laptops.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'host-environment)

;;; env--battery-status-char-indicates-battery-p

(ert-deftest test-host-environment-battery-char-live-codes ()
  "Normal: \"+\", \"-\", and \"!\" indicate a live battery."
  (should (env--battery-status-char-indicates-battery-p "+"))
  (should (env--battery-status-char-indicates-battery-p "-"))
  (should (env--battery-status-char-indicates-battery-p "!")))

(ert-deftest test-host-environment-battery-char-na-returns-nil ()
  "Boundary: legacy \"N/A\" sentinel means no battery."
  (should-not (env--battery-status-char-indicates-battery-p "N/A")))

(ert-deftest test-host-environment-battery-char-unknown-returns-nil ()
  "Boundary: upower \"unknown\" (desktop with AC adapter) means no battery."
  (should-not (env--battery-status-char-indicates-battery-p "unknown")))

(ert-deftest test-host-environment-battery-char-empty-returns-nil ()
  "Boundary: empty string means no battery."
  (should-not (env--battery-status-char-indicates-battery-p "")))

(ert-deftest test-host-environment-battery-char-nil-returns-nil ()
  "Error: nil input means no battery."
  (should-not (env--battery-status-char-indicates-battery-p nil)))

;;; env--power-supply-has-battery-p

(ert-deftest test-host-environment-power-supply-with-bat-dir ()
  "Normal: a directory with BAT0 subdirectory reports a battery."
  (let ((dir (make-temp-file "power-supply-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "BAT0" dir))
          (should (env--power-supply-has-battery-p dir)))
      (delete-directory dir t))))

(ert-deftest test-host-environment-power-supply-no-bat-dir ()
  "Boundary: a directory with only AC adapter entries reports no battery."
  (let ((dir (make-temp-file "power-supply-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "ACAD" dir))
          (make-directory (expand-file-name "ucsi-source-psy-USBC000:001" dir))
          (should-not (env--power-supply-has-battery-p dir)))
      (delete-directory dir t))))

(ert-deftest test-host-environment-power-supply-missing-dir-returns-nil ()
  "Error: nonexistent power-supply directory is treated as no battery."
  (should-not (env--power-supply-has-battery-p "/nonexistent/path/for-test-12345")))

(provide 'test-host-environment)
;;; test-host-environment.el ends here
