;;; test-org-drill-font-switching.el --- Tests for org-drill display management -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that org-drill automatically manages display settings (fonts, modeline)
;; and restores them when the session ends.
;;
;; These are unit tests for the pure logic functions, testing them in isolation
;; without requiring the full org-drill package.

;;; Code:

(require 'ert)

;; Define the functions we're testing (extracted from org-drill-config.el)

(defvar cj/org-drill-previous-preset nil
  "Stores the font preset active before starting org-drill.")

(defvar cj/org-drill-previous-modeline-format nil
  "Stores the modeline format active before starting org-drill.")

(defvar fontaine-current-preset nil
  "Current fontaine preset (mocked for testing).")

(defvar mode-line-format '("Mock modeline")
  "Mock modeline format for testing.")

(defvar org-drill-hide-modeline-during-session t
  "Whether to hide modeline during drill sessions.")

(defun fontaine-set-preset (preset)
  "Mock function: Set fontaine preset to PRESET."
  (setq fontaine-current-preset preset))

(defun cj/org-drill-setup-display ()
  "Set up display for drill sessions: larger fonts and hidden modeline."
  (unless cj/org-drill-previous-preset
    (setq cj/org-drill-previous-preset fontaine-current-preset))
  (fontaine-set-preset 'EBook)
  (when org-drill-hide-modeline-during-session
    (unless cj/org-drill-previous-modeline-format
      (setq cj/org-drill-previous-modeline-format mode-line-format))
    (setq mode-line-format nil)))

(defun cj/org-drill-restore-display ()
  "Restore display settings after drill session ends."
  (when cj/org-drill-previous-preset
    (fontaine-set-preset cj/org-drill-previous-preset)
    (setq cj/org-drill-previous-preset nil))
  (when cj/org-drill-previous-modeline-format
    (setq mode-line-format cj/org-drill-previous-modeline-format)
    (setq cj/org-drill-previous-modeline-format nil)))

;;; Font Management Tests

(ert-deftest test-org-drill-display/saves-current-preset ()
  "Test that starting org-drill saves the current font preset."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'default))
    (cj/org-drill-setup-display)
    (should (eq cj/org-drill-previous-preset 'default))))

(ert-deftest test-org-drill-display/switches-to-ebook ()
  "Test that starting org-drill switches to EBook preset."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'default))
    (cj/org-drill-setup-display)
    (should (eq fontaine-current-preset 'EBook))))

(ert-deftest test-org-drill-display/restores-previous-preset ()
  "Test that ending org-drill restores the previous font preset."
  (let ((cj/org-drill-previous-preset 'default)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'EBook))
    (cj/org-drill-restore-display)
    (should (eq fontaine-current-preset 'default))))

(ert-deftest test-org-drill-display/clears-saved-preset-after-restore ()
  "Test that restoring display clears the saved preset."
  (let ((cj/org-drill-previous-preset 'default)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'EBook))
    (cj/org-drill-restore-display)
    (should (null cj/org-drill-previous-preset))))

;;; Modeline Management Tests

(ert-deftest test-org-drill-display/hides-modeline ()
  "Test that starting org-drill hides the modeline when configured."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'default)
        (mode-line-format '("Mock modeline"))
        (org-drill-hide-modeline-during-session t))
    (cj/org-drill-setup-display)
    (should (null mode-line-format))
    (should (equal cj/org-drill-previous-modeline-format '("Mock modeline")))))

(ert-deftest test-org-drill-display/respects-modeline-config ()
  "Test that modeline hiding respects the configuration variable."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'default)
        (mode-line-format '("Mock modeline"))
        (org-drill-hide-modeline-during-session nil))
    (cj/org-drill-setup-display)
    (should (equal mode-line-format '("Mock modeline")))
    (should (null cj/org-drill-previous-modeline-format))))

(ert-deftest test-org-drill-display/restores-modeline ()
  "Test that ending org-drill restores the modeline."
  (let ((cj/org-drill-previous-preset 'default)
        (cj/org-drill-previous-modeline-format '("Mock modeline"))
        (fontaine-current-preset 'EBook)
        (mode-line-format nil))
    (cj/org-drill-restore-display)
    (should (equal mode-line-format '("Mock modeline")))
    (should (null cj/org-drill-previous-modeline-format))))

;;; Boundary Cases

(ert-deftest test-org-drill-display/does-not-save-preset-twice ()
  "Test that calling setup twice doesn't overwrite the saved preset."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'default))
    ;; First call saves 'default
    (cj/org-drill-setup-display)
    (should (eq cj/org-drill-previous-preset 'default))

    ;; Manually change current preset (simulating a preset change during drill)
    (setq fontaine-current-preset 'FiraCode)

    ;; Second call should NOT update saved preset
    (cj/org-drill-setup-display)
    (should (eq cj/org-drill-previous-preset 'default))
    (should-not (eq cj/org-drill-previous-preset 'FiraCode))))

(ert-deftest test-org-drill-display/restore-with-nil-previous-preset ()
  "Test that restore does nothing when no preset was saved."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'EBook))
    (cj/org-drill-restore-display)
    ;; Should remain at EBook (no restore happened)
    (should (eq fontaine-current-preset 'EBook))
    (should (null cj/org-drill-previous-preset))))

;;; Integration Tests

(ert-deftest test-org-drill-display/full-cycle ()
  "Test complete cycle: save -> switch -> restore."
  (let ((cj/org-drill-previous-preset nil)
        (cj/org-drill-previous-modeline-format nil)
        (fontaine-current-preset 'FiraCode)
        (mode-line-format '("Original modeline"))
        (org-drill-hide-modeline-during-session t))
    ;; Step 1: Start drill (save state, switch to EBook, hide modeline)
    (cj/org-drill-setup-display)
    (should (eq cj/org-drill-previous-preset 'FiraCode))
    (should (eq fontaine-current-preset 'EBook))
    (should (equal cj/org-drill-previous-modeline-format '("Original modeline")))
    (should (null mode-line-format))

    ;; Step 2: End drill (restore everything)
    (cj/org-drill-restore-display)
    (should (eq fontaine-current-preset 'FiraCode))
    (should (null cj/org-drill-previous-preset))
    (should (equal mode-line-format '("Original modeline")))
    (should (null cj/org-drill-previous-modeline-format))))

(provide 'test-org-drill-font-switching)
;;; test-org-drill-font-switching.el ends here
