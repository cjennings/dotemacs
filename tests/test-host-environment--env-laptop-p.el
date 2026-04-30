;;; test-host-environment--env-laptop-p.el --- Tests for env-laptop-p / env-desktop-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `env-laptop-p' and the inverse `env-desktop-p'.  On Linux,
;; the function delegates to `env--power-supply-has-battery-p' against
;; /sys/class/power_supply.  On other platforms, it reads the battery
;; status char.  Tests mock `system-type' and the helpers at their
;; boundary so the predicate's dispatch logic is exercised without
;; touching real system files.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'host-environment)

;; Forward-declare and initialize to nil so let-binding inside tests
;; sees this as special under lexical-binding, and so cl-letf's
;; symbol-value place can read the old value without hitting void-variable.
(defvar battery-status-function nil)

;;; env-laptop-p — Linux dispatch

(ert-deftest test-host-environment-laptop-p-linux-with-battery ()
  "Normal: on Linux, env-laptop-p delegates to power-supply helper."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'env--power-supply-has-battery-p)
               (lambda (_dir) t)))
      (should (env-laptop-p)))))

(ert-deftest test-host-environment-laptop-p-linux-without-battery ()
  "Boundary: on Linux, env-laptop-p returns nil when no BAT* dir is found."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'env--power-supply-has-battery-p)
               (lambda (_dir) nil)))
      (should-not (env-laptop-p)))))

(ert-deftest test-host-environment-laptop-p-linux-passes-correct-dir ()
  "Boundary: on Linux, the power-supply helper receives /sys/class/power_supply."
  (let ((system-type 'gnu/linux)
        captured)
    (cl-letf (((symbol-function 'env--power-supply-has-battery-p)
               (lambda (dir) (setq captured dir) t)))
      (env-laptop-p)
      (should (equal captured "/sys/class/power_supply")))))

;;; env-laptop-p — non-Linux dispatch via battery-format

(ert-deftest test-host-environment-laptop-p-non-linux-with-battery-char ()
  "Normal: on non-Linux, env-laptop-p reads the battery status char."
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'require)
               (lambda (feat &rest _) (eq feat 'battery)))
              ((symbol-function 'battery-format)
               (lambda (_format _data) "+"))
              ((symbol-value 'battery-status-function) (lambda () 'fake-data)))
      (should (env-laptop-p)))))

(ert-deftest test-host-environment-laptop-p-non-linux-no-battery-char ()
  "Boundary: on non-Linux, an N/A char means no battery."
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'require)
               (lambda (feat &rest _) (eq feat 'battery)))
              ((symbol-function 'battery-format)
               (lambda (_format _data) "N/A"))
              ((symbol-value 'battery-status-function) (lambda () 'fake-data)))
      (should-not (env-laptop-p)))))

(ert-deftest test-host-environment-laptop-p-non-linux-no-battery-feature ()
  "Error: on non-Linux without the battery feature, env-laptop-p returns nil."
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'require)
               (lambda (_feat &rest _) nil))
              ((symbol-value 'battery-status-function) nil))
      (should-not (env-laptop-p)))))

;;; env-desktop-p — inverse of env-laptop-p

(ert-deftest test-host-environment-desktop-p-true-when-not-laptop ()
  "Normal: env-desktop-p is t when env-laptop-p is nil."
  (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
    (should (env-desktop-p))))

(ert-deftest test-host-environment-desktop-p-nil-when-laptop ()
  "Boundary: env-desktop-p is nil when env-laptop-p is non-nil."
  (cl-letf (((symbol-function 'env-laptop-p) (lambda () t)))
    (should-not (env-desktop-p))))

(provide 'test-host-environment--env-laptop-p)
;;; test-host-environment--env-laptop-p.el ends here
