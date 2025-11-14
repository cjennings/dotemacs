;;; test-mousetrap-mode--lighter.el --- Tests for lighter functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for mouse-trap-mode lighter functionality.
;; Tests the dynamic lighter display and interactive clicking behavior.

;;; Code:

(require 'ert)
(require 'mousetrap-mode)

;;; Normal Cases

(ert-deftest test-mousetrap-mode--lighter-normal-shows-mousetrap-when-enabled ()
  "Test lighter shows 🪤 when mode is enabled."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter (mouse-trap--lighter-string)))
      (should (string-match-p "🪤" lighter)))))

(ert-deftest test-mousetrap-mode--lighter-normal-shows-mouse-when-disabled ()
  "Test lighter shows 🐭 when mode is disabled."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode -1)
    (let ((lighter (mouse-trap--lighter-string)))
      (should (string-match-p "🐭" lighter)))))

(ert-deftest test-mousetrap-mode--lighter-normal-has-help-echo ()
  "Test lighter has help-echo tooltip."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter (mouse-trap--lighter-string)))
      (should (get-text-property 0 'help-echo lighter))
      (should (string-match-p "Toggle" (get-text-property 0 'help-echo lighter))))))

(ert-deftest test-mousetrap-mode--lighter-normal-has-mouse-face ()
  "Test lighter has mouse-face for hover highlighting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter (mouse-trap--lighter-string)))
      (should (eq (get-text-property 0 'mouse-face lighter) 'mode-line-highlight)))))

(ert-deftest test-mousetrap-mode--lighter-normal-has-local-map ()
  "Test lighter has local-map for click handling."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter (mouse-trap--lighter-string)))
      (should (get-text-property 0 'local-map lighter))
      (should (keymapp (get-text-property 0 'local-map lighter))))))

(ert-deftest test-mousetrap-mode--lighter-normal-keymap-has-mouse-1-binding ()
  "Test lighter keymap has mouse-1 binding."
  (should (keymapp mouse-trap--lighter-keymap))
  (let ((binding (lookup-key mouse-trap--lighter-keymap [mode-line mouse-1])))
    (should binding)
    (should (functionp binding))))

(ert-deftest test-mousetrap-mode--lighter-normal-added-to-mode-line-misc-info ()
  "Test lighter is added to mode-line-misc-info when mode enabled."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((mode-line-misc-info nil))  ; Start fresh
      (mouse-trap-mode 1)
      (should (member '(:eval (mouse-trap--lighter-string)) mode-line-misc-info)))))

(ert-deftest test-mousetrap-mode--lighter-normal-persists-when-mode-disabled ()
  "Test lighter stays in mode-line-misc-info when mode disabled.
This allows it to show the 🐭 indicator when mode is off."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((mode-line-misc-info nil))  ; Start fresh
      (mouse-trap-mode 1)
      (should (member '(:eval (mouse-trap--lighter-string)) mode-line-misc-info))
      (mouse-trap-mode -1)
      ;; Lighter should still be present (to show 🐭)
      (should (member '(:eval (mouse-trap--lighter-string)) mode-line-misc-info)))))

;;; Boundary Cases

(ert-deftest test-mousetrap-mode--lighter-boundary-toggle-changes-display ()
  "Test toggling mode changes lighter display between 🪤 and 🐭."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter-on (mouse-trap--lighter-string)))
      (should (string-match-p "🪤" lighter-on)))
    (mouse-trap-mode -1)
    (let ((lighter-off (mouse-trap--lighter-string)))
      (should (string-match-p "🐭" lighter-off)))
    (mouse-trap-mode 1)
    (let ((lighter-on-again (mouse-trap--lighter-string)))
      (should (string-match-p "🪤" lighter-on-again)))))

(ert-deftest test-mousetrap-mode--lighter-boundary-multiple-enables-no-duplicates ()
  "Test enabling mode multiple times doesn't create duplicate lighters."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((mode-line-misc-info nil))  ; Start fresh
      (mouse-trap-mode 1)
      (mouse-trap-mode -1)
      (mouse-trap-mode 1)
      ;; Should only have one entry
      (let ((count 0))
        (dolist (item mode-line-misc-info)
          (when (equal item '(:eval (mouse-trap--lighter-string)))
            (setq count (1+ count))))
        (should (= count 1))))))

(ert-deftest test-mousetrap-mode--lighter-boundary-different-buffers-independent ()
  "Test lighter state is independent in different buffers."
  (let ((buf1 (generate-new-buffer "test1"))
        (buf2 (generate-new-buffer "test2")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (emacs-lisp-mode)
            (mouse-trap-mode 1)
            (should (string-match-p "🪤" (mouse-trap--lighter-string))))
          (with-current-buffer buf2
            (emacs-lisp-mode)
            (mouse-trap-mode -1)
            (should (string-match-p "🐭" (mouse-trap--lighter-string))))
          ;; Verify buf1 still shows 🪤
          (with-current-buffer buf1
            (should (string-match-p "🪤" (mouse-trap--lighter-string)))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; Edge Cases

(ert-deftest test-mousetrap-mode--lighter-edge-always-evaluates-regardless-of-mode-state ()
  "Test that lighter entry always evaluates, even when mode is disabled.
This is critical - the entry structure is (:eval ...) not (mouse-trap-mode (:eval ...))
so it displays regardless of the mode variable's value."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((mode-line-misc-info nil))
      ;; Enable mode - adds lighter
      (mouse-trap-mode 1)
      (let ((entry (car (member '(:eval (mouse-trap--lighter-string)) mode-line-misc-info))))
        (should entry)
        ;; Entry should be (:eval ...) not (mouse-trap-mode (:eval ...))
        (should (eq (car entry) :eval))
        ;; Verify it's not conditional on mouse-trap-mode being the car
        (should-not (eq (car entry) 'mouse-trap-mode)))

      ;; Disable mode - lighter stays and still evaluates
      (mouse-trap-mode -1)
      (let ((entry (car (member '(:eval (mouse-trap--lighter-string)) mode-line-misc-info))))
        (should entry)
        (should (eq (car entry) :eval))))))

(ert-deftest test-mousetrap-mode--lighter-edge-string-always-has-space-prefix ()
  "Test lighter string always starts with space for proper modeline spacing."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter-on (mouse-trap--lighter-string)))
      (should (string-prefix-p " " lighter-on)))
    (mouse-trap-mode -1)
    (let ((lighter-off (mouse-trap--lighter-string)))
      (should (string-prefix-p " " lighter-off)))))

(ert-deftest test-mousetrap-mode--lighter-edge-properties-cover-entire-string ()
  "Test text properties are applied to entire lighter string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter (mouse-trap--lighter-string)))
      ;; Check properties at each position
      (dotimes (i (length lighter))
        (should (get-text-property i 'local-map lighter))
        (should (get-text-property i 'mouse-face lighter))
        (should (get-text-property i 'help-echo lighter))))))

(ert-deftest test-mousetrap-mode--lighter-edge-same-keymap-instance ()
  "Test all lighters use the same keymap instance for efficiency."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mouse-trap-mode 1)
    (let ((lighter1 (mouse-trap--lighter-string))
          (lighter2 (mouse-trap--lighter-string)))
      (should (eq (get-text-property 0 'local-map lighter1)
                  (get-text-property 0 'local-map lighter2)))
      (should (eq (get-text-property 0 'local-map lighter1)
                  mouse-trap--lighter-keymap)))))

(provide 'test-mousetrap-mode--lighter)
;;; test-mousetrap-mode--lighter.el ends here
