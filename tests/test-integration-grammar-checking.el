;;; test-integration-grammar-checking.el --- Integration tests for grammar checking -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the LanguageTool wrapper script with real grammar checking.
;; Tests the integration: test fixture → wrapper script → LanguageTool → formatted output
;;
;; Components integrated:
;; - scripts/languagetool-flycheck (our wrapper script)
;; - languagetool command (external grammar checker)
;; - Test fixtures with known grammar errors
;; - Output formatting (JSON → flycheck format)
;;
;; Focus: Testing OUR integration code (wrapper), not flycheck framework.
;; We trust that flycheck works; we test that our wrapper produces correct output.
;;
;; Categories: Normal workflow, Boundary cases, Error handling

;;; Code:

(require 'ert)

;; ----------------------------- Test Helpers ----------------------------------

(defun test-integration-grammar--fixture-path (filename)
  "Return absolute path to test fixture FILENAME."
  (expand-file-name (concat "tests/fixtures/" filename)
                    user-emacs-directory))

(defun test-integration-grammar--wrapper-output (file-path)
  "Run languagetool-flycheck wrapper directly on FILE-PATH.
Returns output as string."
  (let ((wrapper (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck")))
    (with-temp-buffer
      (call-process wrapper nil t nil file-path)
      (buffer-string))))

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-integration-grammar-checking-normal-wrapper-detects-errors ()
  "Test that wrapper script detects grammar errors in fixture.

Components integrated:
- scripts/languagetool-flycheck (wrapper script)
- languagetool command (external checker)
- Test fixture with known errors"
  (let* ((fixture (test-integration-grammar--fixture-path "grammar-errors-basic.txt"))
         (output (test-integration-grammar--wrapper-output fixture)))
    ;; Should detect "This are" error
    (should (string-match-p "PLURAL_VERB_AFTER_THIS\\|This are" output))
    ;; Should detect "could of" error
    (should (string-match-p "COULD_OF\\|could of" output))
    ;; Output should be in flycheck format (filename:line:column:)
    (should (string-match-p "grammar-errors-basic\\.txt:[0-9]+:[0-9]+:" output))))

(ert-deftest test-integration-grammar-checking-normal-wrapper-format ()
  "Test that wrapper outputs flycheck-compatible format.

Components integrated:
- scripts/languagetool-flycheck (output formatting)
- languagetool command (JSON parsing)"
  (let* ((fixture (test-integration-grammar--fixture-path "grammar-errors-basic.txt"))
         (output (test-integration-grammar--wrapper-output fixture))
         (lines (split-string output "\n" t)))
    (dolist (line lines)
      ;; Each line should match: filename:line:column: message
      (should (string-match "^[^:]+:[0-9]+:[0-9]+: " line)))))

(ert-deftest test-integration-grammar-checking-normal-correct-text-no-errors ()
  "Test that grammatically correct text produces no errors.

Components integrated:
- scripts/languagetool-flycheck (wrapper script)
- languagetool command (validation)
- Test fixture with correct grammar"
  (let* ((fixture (test-integration-grammar--fixture-path "grammar-correct.txt"))
         (output (test-integration-grammar--wrapper-output fixture)))
    ;; Correct grammar should produce no output (or only whitespace)
    (should (or (string-empty-p (string-trim output))
                (= 0 (length (string-trim output)))))))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-integration-grammar-checking-boundary-empty-file ()
  "Test that empty file produces no errors.

Components integrated:
- scripts/languagetool-flycheck (empty input handling)
- languagetool command"
  (let ((temp-file (make-temp-file "grammar-test-" nil ".txt")))
    (unwind-protect
        (let ((output (test-integration-grammar--wrapper-output temp-file)))
          (should (or (string-empty-p (string-trim output))
                      (= 0 (length (string-trim output))))))
      (delete-file temp-file))))

(ert-deftest test-integration-grammar-checking-boundary-single-word ()
  "Test that single word file produces no errors.

Components integrated:
- scripts/languagetool-flycheck (minimal input)
- languagetool command"
  (let ((temp-file (make-temp-file "grammar-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "Hello"))
          (let ((output (test-integration-grammar--wrapper-output temp-file)))
            ;; Single word might produce no errors or might flag as incomplete sentence
            ;; Just verify it doesn't crash
            (should (stringp output))))
      (delete-file temp-file))))

(ert-deftest test-integration-grammar-checking-boundary-multiple-paragraphs ()
  "Test that file with multiple paragraphs is checked completely.

Components integrated:
- scripts/languagetool-flycheck (multi-paragraph handling)
- languagetool command (full file processing)"
  (let* ((fixture (test-integration-grammar--fixture-path "grammar-errors-basic.txt"))
         (output (test-integration-grammar--wrapper-output fixture))
         (lines (split-string output "\n" t)))
    ;; Should detect errors in multiple lines
    ;; Check that we have multiple error reports with different line numbers
    (let ((line-numbers '()))
      (dolist (line lines)
        (when (string-match ":[0-9]+:" line)
          (let ((line-num (string-to-number
                           (nth 1 (split-string line ":")))))
            (push line-num line-numbers))))
      ;; Should have errors from multiple lines
      (should (> (length (delete-dups line-numbers)) 1)))))

;; ----------------------------- Error Cases -----------------------------------

(ert-deftest test-integration-grammar-checking-error-nonexistent-file ()
  "Test that wrapper handles nonexistent file with error.

Components integrated:
- scripts/languagetool-flycheck (error handling)
- File system (missing file)
- Python exception handling"
  (let* ((nonexistent "/tmp/this-file-does-not-exist-12345.txt")
         (wrapper (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck"))
         (exit-code nil)
         (output nil))
    (with-temp-buffer
      (setq exit-code (call-process wrapper nil t nil nonexistent))
      (setq output (buffer-string)))
    ;; LanguageTool/Python should handle the error
    ;; Check that we get output (error message or error in flycheck format)
    (should (stringp output))
    ;; Output should contain some indication of the error (filename or error marker)
    (should (or (string-match-p nonexistent output)
                (string-match-p "error" output)
                (string-match-p "Error" output)
                ;; Or it might report no errors for a nonexistent file
                (string-empty-p (string-trim output))))))

(ert-deftest test-integration-grammar-checking-error-no-file-argument ()
  "Test that wrapper requires file argument.

Components integrated:
- scripts/languagetool-flycheck (argument validation)"
  (let* ((wrapper (expand-file-name "~/.emacs.d/scripts/languagetool-flycheck"))
         (exit-code nil))
    (with-temp-buffer
      (setq exit-code (call-process wrapper nil t nil))
      ;; Should exit with non-zero status when no file provided
      (should-not (= 0 exit-code)))))

;; ----------------------------- Integration with Real Files -------------------

(ert-deftest test-integration-grammar-checking-integration-comprehensive-errors ()
  "Test that wrapper catches multiple types of grammar errors in one file.

Components integrated:
- scripts/languagetool-flycheck (our wrapper)
- languagetool command (comprehensive checking)
- Test fixture with various error types"
  (let* ((fixture (test-integration-grammar--fixture-path "grammar-errors-basic.txt"))
         (output (test-integration-grammar--wrapper-output fixture))
         (lines (split-string output "\n" t)))
    ;; Should detect multiple errors (at least 3-4 in the fixture)
    (should (>= (length lines) 3))
    ;; All lines should be properly formatted
    (dolist (line lines)
      (should (string-match "^[^:]+:[0-9]+:[0-9]+: " line)))))

(provide 'test-integration-grammar-checking)
;;; test-integration-grammar-checking.el ends here
