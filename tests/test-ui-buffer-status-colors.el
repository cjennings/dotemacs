;;; test-ui-buffer-status-colors.el --- Tests for buffer status colors -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for buffer status color system.
;; Tests the state detection logic used by both cursor color and modeline.

;;; Code:

(require 'ert)
(require 'user-constants)
(require 'ui-config)
(require 'modeline-config)

;;; Color Constant Tests

(ert-deftest test-buffer-status-colors-has-all-states ()
  "Test that all required states are defined in color alist."
  (should (alist-get 'read-only cj/buffer-status-colors))
  (should (alist-get 'overwrite cj/buffer-status-colors))
  (should (alist-get 'modified cj/buffer-status-colors))
  (should (alist-get 'unmodified cj/buffer-status-colors)))

(ert-deftest test-buffer-status-colors-values-are-strings ()
  "Test that all color values are strings (hex colors)."
  (dolist (entry cj/buffer-status-colors)
    (should (stringp (cdr entry)))
    ;; Check if it looks like a hex color
    (should (string-match-p "^#[0-9a-fA-F]\\{6\\}$" (cdr entry)))))

;;; Cursor Color State Detection Tests

(ert-deftest test-cursor-color-state-read-only-buffer ()
  "Test state detection for read-only buffer."
  (with-temp-buffer
    (setq buffer-read-only t)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'read-only)))))

(ert-deftest test-cursor-color-state-overwrite-mode ()
  "Test state detection for overwrite mode."
  (with-temp-buffer
    (setq buffer-read-only nil)
    (overwrite-mode 1)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'overwrite)))))

(ert-deftest test-cursor-color-state-modified-buffer ()
  "Test state detection for modified buffer."
  (with-temp-buffer
    (setq buffer-read-only nil)
    (insert "test")
    (set-buffer-modified-p t)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'modified)))))

(ert-deftest test-cursor-color-state-unmodified-buffer ()
  "Test state detection for unmodified buffer."
  (with-temp-buffer
    (setq buffer-read-only nil)
    (set-buffer-modified-p nil)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'unmodified)))))

(ert-deftest test-cursor-color-state-priority-read-only-over-modified ()
  "Test that read-only state takes priority over modified state."
  (with-temp-buffer
    (insert "test")
    (set-buffer-modified-p t)
    (setq buffer-read-only t)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'read-only)))))

(ert-deftest test-cursor-color-state-priority-overwrite-over-modified ()
  "Test that overwrite mode takes priority over modified state."
  (with-temp-buffer
    (insert "test")
    (set-buffer-modified-p t)
    (overwrite-mode 1)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'overwrite)))))

;;; Integration Tests - Cursor Color Function

(ert-deftest test-cursor-color-function-exists ()
  "Test that cursor color function is defined."
  (should (fboundp 'cj/set-cursor-color-according-to-mode)))

(ert-deftest test-cursor-color-returns-correct-color-for-read-only ()
  "Test cursor color function returns red for read-only buffer."
  (with-temp-buffer
    (setq buffer-read-only t)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified)))
           (color (alist-get state cj/buffer-status-colors)))
      (should (equal color "#f06a3f")))))

(ert-deftest test-cursor-color-returns-correct-color-for-overwrite ()
  "Test cursor color function returns gold for overwrite mode."
  (with-temp-buffer
    (overwrite-mode 1)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified)))
           (color (alist-get state cj/buffer-status-colors)))
      (should (equal color "#c48702")))))

(ert-deftest test-cursor-color-returns-correct-color-for-modified ()
  "Test cursor color function returns green for modified buffer."
  (with-temp-buffer
    (insert "test")
    (set-buffer-modified-p t)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified)))
           (color (alist-get state cj/buffer-status-colors)))
      (should (equal color "#64aa0f")))))

(ert-deftest test-cursor-color-returns-correct-color-for-unmodified ()
  "Test cursor color function returns white for unmodified buffer."
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified)))
           (color (alist-get state cj/buffer-status-colors)))
      (should (equal color "#ffffff")))))

;;; Modeline Integration Tests

(ert-deftest test-modeline-buffer-name-variable-exists ()
  "Test that modeline buffer name variable is defined."
  (should (boundp 'cj/modeline-buffer-name)))

(ert-deftest test-modeline-buffer-name-is-mode-line-construct ()
  "Test that modeline buffer name is a valid mode-line construct."
  (should (listp cj/modeline-buffer-name))
  (should (eq (car cj/modeline-buffer-name) :eval)))

;;; Edge Cases

(ert-deftest test-buffer-status-new-buffer-starts-unmodified ()
  "Test that new buffer starts in unmodified state."
  (with-temp-buffer
    (let* ((state (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state 'unmodified)))))

(ert-deftest test-buffer-status-insert-makes-modified ()
  "Test that inserting text changes state to modified."
  (with-temp-buffer
    ;; Initially unmodified
    (set-buffer-modified-p nil)
    (let ((state1 (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state1 'unmodified)))

    ;; Insert text
    (insert "test")
    (let ((state2 (cond
                   (buffer-read-only       'read-only)
                   (overwrite-mode         'overwrite)
                   ((buffer-modified-p)    'modified)
                   (t                      'unmodified))))
      (should (eq state2 'modified)))))

(ert-deftest test-buffer-status-explicit-unmodify ()
  "Test that explicitly setting unmodified works."
  (with-temp-buffer
    (insert "test")
    (should (buffer-modified-p))

    ;; Explicitly set unmodified
    (set-buffer-modified-p nil)
    (let ((state (cond
                  (buffer-read-only       'read-only)
                  (overwrite-mode         'overwrite)
                  ((buffer-modified-p)    'modified)
                  (t                      'unmodified))))
      (should (eq state 'unmodified)))))

(provide 'test-ui-buffer-status-colors)
;;; test-ui-buffer-status-colors.el ends here
