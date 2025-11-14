;;; test-custom-misc-replace-fraction-glyphs.el --- Tests for cj/--replace-fraction-glyphs -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--replace-fraction-glyphs function from custom-misc.el
;;
;; This function bidirectionally converts between text fractions (1/4) and
;; Unicode fraction glyphs (¼). It supports 5 common fractions:
;; - 1/4 ↔ ¼
;; - 1/2 ↔ ½
;; - 3/4 ↔ ¾
;; - 1/3 ↔ ⅓
;; - 2/3 ↔ ⅔
;;
;; We test the NON-INTERACTIVE implementation (cj/--replace-fraction-glyphs)
;; to avoid mocking prefix arguments. This follows our testing best practice
;; of separating business logic from UI interaction.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-misc)

;;; Test Helpers

(defun test-replace-fraction-glyphs (input-text to-glyphs)
  "Test cj/--replace-fraction-glyphs on INPUT-TEXT.
TO-GLYPHS determines conversion direction.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--replace-fraction-glyphs (point-min) (point-max) to-glyphs)
    (buffer-string)))

;;; Normal Cases - Text to Glyphs

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-quarter ()
  "Should convert 1/4 to ¼."
  (let ((result (test-replace-fraction-glyphs "1/4" t)))
    (should (string= result "¼"))))

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-half ()
  "Should convert 1/2 to ½."
  (let ((result (test-replace-fraction-glyphs "1/2" t)))
    (should (string= result "½"))))

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-three-quarters ()
  "Should convert 3/4 to ¾."
  (let ((result (test-replace-fraction-glyphs "3/4" t)))
    (should (string= result "¾"))))

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-third ()
  "Should convert 1/3 to ⅓."
  (let ((result (test-replace-fraction-glyphs "1/3" t)))
    (should (string= result "⅓"))))

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-two-thirds ()
  "Should convert 2/3 to ⅔."
  (let ((result (test-replace-fraction-glyphs "2/3" t)))
    (should (string= result "⅔"))))

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-multiple ()
  "Should convert multiple fractions in text."
  (let ((result (test-replace-fraction-glyphs "Use 1/4 cup and 1/2 teaspoon" t)))
    (should (string= result "Use ¼ cup and ½ teaspoon"))))

(ert-deftest test-replace-fraction-glyphs-text-to-glyph-all-types ()
  "Should convert all fraction types."
  (let ((result (test-replace-fraction-glyphs "1/4 1/2 3/4 1/3 2/3" t)))
    (should (string= result "¼ ½ ¾ ⅓ ⅔"))))

;;; Normal Cases - Glyphs to Text

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-quarter ()
  "Should convert ¼ to 1/4."
  (let ((result (test-replace-fraction-glyphs "¼" nil)))
    (should (string= result "1/4"))))

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-half ()
  "Should convert ½ to 1/2."
  (let ((result (test-replace-fraction-glyphs "½" nil)))
    (should (string= result "1/2"))))

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-three-quarters ()
  "Should convert ¾ to 3/4."
  (let ((result (test-replace-fraction-glyphs "¾" nil)))
    (should (string= result "3/4"))))

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-third ()
  "Should convert ⅓ to 1/3."
  (let ((result (test-replace-fraction-glyphs "⅓" nil)))
    (should (string= result "1/3"))))

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-two-thirds ()
  "Should convert ⅔ to 2/3."
  (let ((result (test-replace-fraction-glyphs "⅔" nil)))
    (should (string= result "2/3"))))

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-multiple ()
  "Should convert multiple glyphs in text."
  (let ((result (test-replace-fraction-glyphs "Use ¼ cup and ½ teaspoon" nil)))
    (should (string= result "Use 1/4 cup and 1/2 teaspoon"))))

(ert-deftest test-replace-fraction-glyphs-glyph-to-text-all-types ()
  "Should convert all glyph types."
  (let ((result (test-replace-fraction-glyphs "¼ ½ ¾ ⅓ ⅔" nil)))
    (should (string= result "1/4 1/2 3/4 1/3 2/3"))))

;;; Boundary Cases

(ert-deftest test-replace-fraction-glyphs-empty-string ()
  "Should handle empty string."
  (let ((result (test-replace-fraction-glyphs "" t)))
    (should (string= result ""))))

(ert-deftest test-replace-fraction-glyphs-no-fractions-to-glyphs ()
  "Should handle text with no fractions (no-op) when converting to glyphs."
  (let ((result (test-replace-fraction-glyphs "hello world" t)))
    (should (string= result "hello world"))))

(ert-deftest test-replace-fraction-glyphs-no-fractions-to-text ()
  "Should handle text with no glyphs (no-op) when converting to text."
  (let ((result (test-replace-fraction-glyphs "hello world" nil)))
    (should (string= result "hello world"))))

(ert-deftest test-replace-fraction-glyphs-at-start ()
  "Should handle fraction at start of text."
  (let ((result (test-replace-fraction-glyphs "1/2 of the total" t)))
    (should (string= result "½ of the total"))))

(ert-deftest test-replace-fraction-glyphs-at-end ()
  "Should handle fraction at end of text."
  (let ((result (test-replace-fraction-glyphs "Reduce by 1/4" t)))
    (should (string= result "Reduce by ¼"))))

(ert-deftest test-replace-fraction-glyphs-repeated ()
  "Should handle repeated fractions."
  (let ((result (test-replace-fraction-glyphs "1/4 and 1/4 and 1/4" t)))
    (should (string= result "¼ and ¼ and ¼"))))

(ert-deftest test-replace-fraction-glyphs-very-long-text ()
  "Should handle very long text with many fractions."
  (let* ((long-text (mapconcat (lambda (_) "1/4") (make-list 50 nil) " "))
         (result (test-replace-fraction-glyphs long-text t)))
    (should (string-match-p "¼" result))
    (should-not (string-match-p "1/4" result))))

(ert-deftest test-replace-fraction-glyphs-bidirectional ()
  "Should correctly convert back and forth."
  (let* ((original "Use 1/4 cup")
         (to-glyph (test-replace-fraction-glyphs original t))
         (back-to-text (test-replace-fraction-glyphs to-glyph nil)))
    (should (string= to-glyph "Use ¼ cup"))
    (should (string= back-to-text original))))

;;; Error Cases

(ert-deftest test-replace-fraction-glyphs-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "1/4")
     (cj/--replace-fraction-glyphs (point-max) (point-min) t))
   :type 'error))

(ert-deftest test-replace-fraction-glyphs-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "1/4")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--replace-fraction-glyphs pos pos t)
      ;; Should complete without error
      (should (string= (buffer-string) "1/4")))))

(provide 'test-custom-misc-replace-fraction-glyphs)
;;; test-custom-misc-replace-fraction-glyphs.el ends here
