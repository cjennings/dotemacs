;;; test-jumper-public-wrappers.el --- Tests for the interactive jumper wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Internal helpers (`jumper--do-store-location',
;; `jumper--do-jump-to-location', `jumper--do-remove-location') are
;; already tested in test-jumper.el.  This file covers the three
;; interactive entry points that dispatch user input (and messages)
;; into those helpers:
;;
;;   jumper-store-location
;;   jumper-jump-to-location
;;   jumper-remove-location
;;
;; `message' is captured to verify the user-visible feedback path;
;; `completing-read' is stubbed when the wrapper prompts the user.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'jumper)

(defmacro test-jumper-wrappers--with-clean-state (&rest body)
  "Reset jumper state and capture messages, then evaluate BODY."
  `(let ((jumper--registers (make-vector jumper-max-locations nil))
         (jumper--next-index 0)
         (captured-message nil))
     ;; Clear the 'z' last-location register so completing-read prompts
     ;; don't show it from a previous test run in the same process.
     (set-register jumper--last-location-register nil)
     (dotimes (i jumper-max-locations)
       (set-register (+ ?0 i) nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq captured-message (apply #'format fmt args)))))
       ,@body)))

;;; jumper-store-location

(ert-deftest test-jumper-wrapper-store-fresh-location-messages-register ()
  "Normal: storing a fresh location messages the register that was assigned."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (insert "alpha\n")
     (goto-char (point-min))
     (jumper-store-location)
     (should (string-match-p "register" captured-message))
     (should (= jumper--next-index 1)))))

(ert-deftest test-jumper-wrapper-store-duplicate-messages-already-stored ()
  "Boundary: storing the same location twice surfaces the duplicate path."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (insert "alpha\n")
     (goto-char (point-min))
     (jumper-store-location)
     (setq captured-message nil)
     (jumper-store-location)
     (should (string-match-p "already stored" captured-message))
     (should (= jumper--next-index 1)))))

(ert-deftest test-jumper-wrapper-store-when-full-messages-no-space ()
  "Boundary: storing when every register is taken surfaces the no-space path."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (dotimes (i jumper-max-locations)
       (insert (format "line %d\n" i))
       (goto-char (point-max))
       (jumper-store-location))
     (insert "one-more\n")
     (goto-char (point-max))
     (setq captured-message nil)
     (jumper-store-location)
     (should (string-match-p "all jump locations" captured-message)))))

;;; jumper-jump-to-location

(ert-deftest test-jumper-wrapper-jump-no-locations-messages ()
  "Normal: with no stored locations the wrapper surfaces the empty state."
  (test-jumper-wrappers--with-clean-state
   (jumper-jump-to-location)
   (should (string-match-p "No locations" captured-message))))

(ert-deftest test-jumper-wrapper-jump-single-location-already-there ()
  "Normal: single-location toggle from the stored position surfaces the
already-there message."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (insert "alpha\n")
     (goto-char (point-min))
     (jumper-store-location)
     (setq captured-message nil)
     ;; Don't move -- jump should detect we're at the stored location.
     (jumper-jump-to-location)
     (should (string-match-p "already at" captured-message)))))

(ert-deftest test-jumper-wrapper-jump-multi-location-prompts ()
  "Normal: with multiple locations the wrapper prompts via completing-read
and jumps to the selected entry."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (insert "alpha\nbeta\n")
     ;; Two stored locations at different points.
     (goto-char (point-min))
     (jumper-store-location)
     (forward-line)
     (jumper-store-location)
     (setq captured-message nil)
     ;; Stub the prompt to pick the first candidate (index 0).
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _)
                  ;; Pick whichever candidate maps to integer 0.
                  (car (cl-find-if (lambda (cell) (equal (cdr cell) 0))
                                   collection)))))
       (jumper-jump-to-location))
     (should (string-match-p "Jumped" captured-message)))))

;;; jumper-remove-location

(ert-deftest test-jumper-wrapper-remove-no-locations-messages ()
  "Normal: removing with no stored locations surfaces the empty state."
  (test-jumper-wrappers--with-clean-state
   (jumper-remove-location)
   (should (string-match-p "No locations" captured-message))))

(ert-deftest test-jumper-wrapper-remove-prompts-and-deletes ()
  "Normal: with locations stored, removing prompts and deletes the chosen one."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (insert "alpha\nbeta\n")
     (goto-char (point-min))
     (jumper-store-location)
     (forward-line)
     (jumper-store-location)
     (should (= jumper--next-index 2))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _)
                  (car (cl-find-if (lambda (cell) (equal (cdr cell) 0))
                                   collection)))))
       (setq captured-message nil)
       (jumper-remove-location))
     (should (string-match-p "Location removed" captured-message))
     (should (= jumper--next-index 1)))))

(ert-deftest test-jumper-wrapper-remove-cancel-keeps-state ()
  "Boundary: choosing Cancel surfaces the cancelled message and changes nothing."
  (test-jumper-wrappers--with-clean-state
   (with-temp-buffer
     (insert "alpha\n")
     (goto-char (point-min))
     (jumper-store-location)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _)
                  (car (cl-find-if (lambda (cell) (equal (cdr cell) -1))
                                   collection)))))
       (setq captured-message nil)
       (jumper-remove-location))
     (should (string-match-p "cancelled" captured-message))
     (should (= jumper--next-index 1)))))

;;; jumper-setup-keys

(ert-deftest test-jumper-setup-keys-binds-prefix-to-jumper-map ()
  "Normal: `jumper-setup-keys' wires the prefix key to `jumper-map'."
  (let ((jumper-prefix-key "C-c M-j"))
    (jumper-setup-keys)
    (should (eq (keymap-lookup (current-global-map) jumper-prefix-key)
                jumper-map))
    ;; Cleanup so the test binding doesn't leak.
    (keymap-global-unset jumper-prefix-key)))

(provide 'test-jumper-public-wrappers)
;;; test-jumper-public-wrappers.el ends here
