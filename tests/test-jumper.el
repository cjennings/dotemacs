;;; test-jumper.el --- Tests for jumper.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for jumper.el - location navigation using registers.
;;
;; Testing approach:
;; - Tests focus on internal `jumper--do-*` functions (pure business logic)
;; - Interactive wrappers are thin UI layers and tested minimally
;; - Each test is isolated with setup/teardown to reset global state
;; - Tests verify return values, not user messages

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load the module
(require 'jumper)

;;; Test Utilities

(defvar test-jumper--original-registers nil
  "Backup of jumper registers before test.")

(defvar test-jumper--original-index nil
  "Backup of jumper index before test.")

(defun test-jumper-setup ()
  "Reset jumper state before each test."
  ;; Backup current state
  (setq test-jumper--original-registers jumper--registers)
  (setq test-jumper--original-index jumper--next-index)
  ;; Reset to clean state
  (setq jumper--registers (make-vector jumper-max-locations nil))
  (setq jumper--next-index 0))

(defun test-jumper-teardown ()
  "Restore jumper state after each test."
  (setq jumper--registers test-jumper--original-registers)
  (setq jumper--next-index test-jumper--original-index))

;;; Normal Cases - Store Location

(ert-deftest test-jumper-store-first-location ()
  "Should store first location and return register character."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (let ((result (jumper--do-store-location)))
      (should (= result ?0))
      (should (= jumper--next-index 1))))
  (test-jumper-teardown))

(ert-deftest test-jumper-store-multiple-locations ()
  "Should store multiple locations in sequence."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (should (= (jumper--do-store-location) ?0))
    (forward-line 1)
    (should (= (jumper--do-store-location) ?1))
    (forward-line 1)
    (should (= (jumper--do-store-location) ?2))
    (should (= jumper--next-index 3)))
  (test-jumper-teardown))

(ert-deftest test-jumper-store-duplicate-location ()
  "Should detect and reject duplicate locations."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (should (= (jumper--do-store-location) ?0))
    (should (eq (jumper--do-store-location) 'already-exists))
    (should (= jumper--next-index 1)))
  (test-jumper-teardown))

;;; Normal Cases - Jump to Location

(ert-deftest test-jumper-jump-to-stored-location ()
  "Should jump to a previously stored location."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (jumper--do-store-location)
    (goto-char (point-max))
    (let ((result (jumper--do-jump-to-location 0)))
      (should (eq result 'jumped))
      (should (= (point) (point-min)))))
  (test-jumper-teardown))

(ert-deftest test-jumper-jump-toggle-with-single-location ()
  "Should toggle between current and stored location."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (jumper--do-store-location)
    ;; Move away
    (goto-char (point-max))
    ;; Toggle should jump back
    (let ((result (jumper--do-jump-to-location nil)))
      (should (eq result 'jumped))
      (should (= (point) (point-min)))))
  (test-jumper-teardown))

(ert-deftest test-jumper-jump-already-at-location ()
  "Should detect when already at the only stored location."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2")
    (goto-char (point-min))
    (jumper--do-store-location)
    ;; Try to toggle while at the location
    (let ((result (jumper--do-jump-to-location nil)))
      (should (eq result 'already-there))))
  (test-jumper-teardown))

;;; Normal Cases - Remove Location

(ert-deftest test-jumper-remove-location ()
  "Should remove a stored location."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (jumper--do-store-location)
    (let ((result (jumper--do-remove-location 0)))
      (should (eq result t))
      (should (= jumper--next-index 0))))
  (test-jumper-teardown))

(ert-deftest test-jumper-remove-reorders-registers ()
  "Should reorder registers after removal from middle."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (jumper--do-store-location)  ; Register 0
    (forward-line 1)
    (jumper--do-store-location)  ; Register 1
    (forward-line 1)
    (jumper--do-store-location)  ; Register 2
    ;; Remove middle (index 1)
    (jumper--do-remove-location 1)
    (should (= jumper--next-index 2))
    ;; What was at index 2 should now be at index 1
    (should (= (aref jumper--registers 1) ?2)))
  (test-jumper-teardown))

;;; Boundary Cases - Store Location

(ert-deftest test-jumper-store-at-capacity ()
  "Should successfully store location at maximum capacity."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    ;; Fill to capacity
    (dotimes (i jumper-max-locations)
      (forward-char 1)
      (should (= (jumper--do-store-location) (+ ?0 i))))
    (should (= jumper--next-index jumper-max-locations)))
  (test-jumper-teardown))

(ert-deftest test-jumper-store-when-full ()
  "Should return 'no-space when all registers are full."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "01234567890123456789")
    (goto-char (point-min))
    ;; Fill to capacity
    (dotimes (i jumper-max-locations)
      (forward-char 1)
      (jumper--do-store-location))
    ;; Try to store one more
    (forward-char 1)
    (should (eq (jumper--do-store-location) 'no-space))
    (should (= jumper--next-index jumper-max-locations)))
  (test-jumper-teardown))

(ert-deftest test-jumper-store-in-different-buffers ()
  "Should store locations across different buffers."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "buffer 1")
    (goto-char (point-min))
    (should (= (jumper--do-store-location) ?0))
    (with-temp-buffer
      (insert "buffer 2")
      (goto-char (point-min))
      (should (= (jumper--do-store-location) ?1))
      (should (= jumper--next-index 2))))
  (test-jumper-teardown))

;;; Boundary Cases - Jump to Location

(ert-deftest test-jumper-jump-with-no-locations ()
  "Should return 'no-locations when nothing is stored."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test")
    (let ((result (jumper--do-jump-to-location 0)))
      (should (eq result 'no-locations))))
  (test-jumper-teardown))

(ert-deftest test-jumper-jump-to-first-location ()
  "Should jump to location at index 0."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2")
    (goto-char (point-min))
    (jumper--do-store-location)
    (forward-line 1)
    (jumper--do-store-location)
    (goto-char (point-max))
    (jumper--do-jump-to-location 0)
    (should (= (point) (point-min))))
  (test-jumper-teardown))

(ert-deftest test-jumper-jump-to-last-location ()
  "Should jump to last location (register 'z)."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (jumper--do-store-location)
    (let ((line2-pos (line-beginning-position 2)))
      (goto-char line2-pos)
      ;; Jump to location 0 (this stores current location in 'z)
      (jumper--do-jump-to-location 0)
      (should (= (point) (point-min)))
      ;; Jump to last location should go back to line 2
      (let ((result (jumper--do-jump-to-location -1)))
        (should (eq result 'jumped))
        (should (= (point) line2-pos)))))
  (test-jumper-teardown))

(ert-deftest test-jumper-jump-to-max-index ()
  "Should jump to location at maximum index."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "0123456789012345678")
    (goto-char (point-min))
    ;; Store at all positions
    (dotimes (i jumper-max-locations)
      (forward-char 1)
      (jumper--do-store-location))
    (goto-char (point-min))
    ;; Jump to last one (index 9, which is at position 10)
    (jumper--do-jump-to-location (1- jumper-max-locations))
    (should (= (point) (1+ jumper-max-locations))))
  (test-jumper-teardown))

;;; Boundary Cases - Remove Location

(ert-deftest test-jumper-remove-first-location ()
  "Should remove location at index 0."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2")
    (goto-char (point-min))
    (jumper--do-store-location)
    (forward-line 1)
    (jumper--do-store-location)
    (jumper--do-remove-location 0)
    (should (= jumper--next-index 1))
    ;; What was at index 1 should now be at index 0
    (should (= (aref jumper--registers 0) ?1)))
  (test-jumper-teardown))

(ert-deftest test-jumper-remove-last-location ()
  "Should remove location at last index."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (jumper--do-store-location)
    (forward-line 1)
    (jumper--do-store-location)
    (forward-line 1)
    (jumper--do-store-location)
    (jumper--do-remove-location 2)
    (should (= jumper--next-index 2)))
  (test-jumper-teardown))

(ert-deftest test-jumper-remove-with-cancel ()
  "Should return 'cancelled when index is -1."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test")
    (goto-char (point-min))
    (jumper--do-store-location)
    (let ((result (jumper--do-remove-location -1)))
      (should (eq result 'cancelled))
      (should (= jumper--next-index 1))))
  (test-jumper-teardown))

;;; Error Cases

(ert-deftest test-jumper-remove-when-empty ()
  "Should return 'no-locations when removing from empty list."
  (test-jumper-setup)
  (let ((result (jumper--do-remove-location 0)))
    (should (eq result 'no-locations)))
  (test-jumper-teardown))

;;; Helper Function Tests

(ert-deftest test-jumper-location-key-format ()
  "Should generate unique location keys."
  (with-temp-buffer
    (insert "line 1\nline 2")
    (goto-char (point-min))
    (let ((key1 (jumper--location-key)))
      (forward-line 1)
      (let ((key2 (jumper--location-key)))
        (should-not (string= key1 key2))
        ;; Keys should contain buffer name and position info
        (should (string-match-p ":" key1))
        (should (string-match-p ":" key2))))))

(ert-deftest test-jumper-register-available-p ()
  "Should correctly report register availability."
  (test-jumper-setup)
  (should (jumper--register-available-p))
  ;; Fill to capacity
  (setq jumper--next-index jumper-max-locations)
  (should-not (jumper--register-available-p))
  (test-jumper-teardown))

(ert-deftest test-jumper-format-location ()
  "Should format location for display."
  (test-jumper-setup)
  (with-temp-buffer
    (insert "test line with some content")
    (goto-char (point-min))
    (jumper--do-store-location)
    (let ((formatted (jumper--format-location 0)))
      (should formatted)
      (should (string-match-p "\\[0\\]" formatted))
      (should (string-match-p "test line" formatted))))
  (test-jumper-teardown))

(provide 'test-jumper)
;;; test-jumper.el ends here
